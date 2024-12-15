# ------------------------------------------------------------------------------
# Environmental Data Interpolation for Bird Tracking Data
# ------------------------------------------------------------------------------
# Author: Ali Moayedi
# Email: am636@st-andrews.ac.uk
# Date: October 2024
# 
# Description:
# This script interpolates ERA5 environmental data onto bird tracking data for 
# the spring migration season of 2020. It uses bilinear interpolation for spatial
# interpolation and inverse distance weighting for temporal interpolation.
# The aim is to integrate environmental conditions with geese movement data to
# explore their influence on migratory behavior, as detailed in the paper 
# "Multi-modal navigation in migratory birds: a data mining study."
# 
# Input:
#   - ERA5 NetCDF file: "adaptor.mars.internal-2020.nc"   # ERA5 hourly data on single levels from 1940 to present.
#   - Tracking data CSV file: "GeoMagResult_Spring_2020.csv"   # Traking data annotated with geomagnetic information
# 
# Output:
#   - CSV file with interpolated environmental variables: "GeoMag_Env_Spring_2020.csv"
# 
# Acknowledgments:
# ERA5 data source: Copernicus Climate Data Store (https://cds.climate.copernicus.eu)
# Data Source:
# - Geomagnetic annotation using MagGeo Tool: [https://github.com/MagGeo/MagGeo]

# Note:
# This script is part of the study: "Multi-modal, interrelated navigation in migratory birds: a data mining study"
# Please refer to the published paper for detailed methodology and dataset access.

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ncdf4", quietly = TRUE)) install.packages("ncdf4")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("geosphere", quietly = TRUE)) install.packages("geosphere")

library(dplyr)
library(ncdf4)
library(lubridate)
library(geosphere)


# --------------------------------------------------------------------------
# Part 1: Load and Prepare Data
# --------------------------------------------------------------------------

# Load ERA5 NetCDF data for 2020
nc_data = nc_open("adaptor.mars.internal-2020.nc") # ERA5 data file
print(nc_data)   # Print NetCDF details
print(names(nc_data$dim))  # List dimensions available in the file

# Load bird tracking data for the specific season and year
tracking_data <- read.csv("GeoMagResult_Spring_2020.csv")
tracking_data$timePosix <- as.POSIXct(tracking_data$timePosix, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Confirm the time zone of the tracking data
attr(tracking_data$timePosix, "tzone")

# Define environmental variables to interpolate
variables_to_interpolate <- c("u100","v100") 



# --------------------------------------------------------------------------
# Part 2: Time Indexing and Testing ERA5 Data
# --------------------------------------------------------------------------

# Extracting time attributes (from ERA5 dataset, time is in hours since 1900-01-01)
time_attrs <- ncatt_get(nc_data, "time")
nc_timestamps <- ncvar_get(nc_data, "time") 

# Convert NetCDF timestamps (in hours since 1900-01-01) to POSIXct
reference_date <- as.POSIXct("1900-01-01", tz = "UTC")
nc_timestamps_posix <- reference_date + nc_timestamps * 3600  # Convert hours to seconds


# --------------------------------------------------------------------------
# Part 3: Functions for Interpolation
# --------------------------------------------------------------------------

# Function to find the closest ERA5 timesteps (before and after a given timestamp)
find_closest_timesteps <- function(nc_data, ti_posix) {  # ti shoud be POSIXct
  nc_timestamps <- ncvar_get(nc_data, "time")  
  reference_date <- as.POSIXct("1900-01-01", tz = "UTC")
  nc_timestamps_posix <- reference_date + nc_timestamps * 3600  
  time_diffs <- difftime(ti_posix, nc_timestamps_posix, units = "secs") 
  before_idx <- max(which(nc_timestamps_posix <= ti_posix))  
  after_idx <- min(which(nc_timestamps_posix >= ti_posix))
  timestamp_of_before_data <- nc_timestamps_posix[before_idx]
  timestamp_of_after_data <- nc_timestamps_posix[after_idx]
  time_result <- list(before_idx = before_idx, 
                      timestamp_of_before_data = timestamp_of_before_data,
                      after_idx = after_idx, 
                      timestamp_of_after_data = timestamp_of_after_data)
  return(time_result)
  
}



# Function to calculate the average value of an environmental variable at the four corners of a grid cell
calculate_corner_var <- function(data, long_index, lat_index) {
  #corner1: at the top-right
  top_right_value <- mean(c(data[long_index - 1, lat_index], #top cell
                            data[long_index - 1, lat_index + 1], #top-right cell
                            data[long_index, lat_index], # cell point belongs to
                            data[long_index, lat_index + 1])) #right cell
  
  #corner2: at the bottom-right
  bottom_right_value <- mean(c(data[long_index, lat_index], # cell point belongs to
                               data[long_index, lat_index + 1], #right cell
                               data[long_index + 1, lat_index], # bottom
                               data[long_index + 1, lat_index + 1]))# bottom_right
  
  #corner3: at the bottom-left
  bottom_left_value <- mean(c(data[long_index, lat_index - 1], #left
                              data[long_index, lat_index], # cell point belongs to
                              data[long_index + 1, lat_index - 1], #bottom_left
                              data[long_index + 1, lat_index]))# bottom
  
  #corner4: at the top-left
  top_left_value <- mean(c(data[long_index - 1, lat_index - 1], #top-left
                           data[long_index - 1, lat_index], #top
                           data[long_index, lat_index -1], #left
                           data[long_index, lat_index]))# cell point belongs to
  #cell value
  cell_value<- data[long_index, lat_index]
  
  return(c(cell_value, top_right_value,bottom_right_value,bottom_left_value,top_left_value))
}



# Bilinear interpolation to estimate value at the tracking point
bilinear_interpolation <- function(corner_values, point_lon, point_lat, cell_lon, cell_lat) {
  # Calculate great circle distances 
  distances = c( distHaversine(c(point_lon, point_lat), c(cell_lon, cell_lat)),  # Center 
                 distHaversine(c(point_lon, point_lat), c(cell_lon - 0.125, cell_lat - 0.125)),  #top_right corner
                 distHaversine(c(point_lon, point_lat), c(cell_lon + 0.125, cell_lat - 0.125)),  #bottom_right corner
                 distHaversine(c(point_lon, point_lat), c(cell_lon + 0.125, cell_lat + 0.125)),   #bottom_left corner
                 distHaversine(c(point_lon, point_lat), c(cell_lon - 0.125, cell_lat + 0.125)))  #top_left
  total_distance <- sum(distances)
  weights <- 1 - (distances/total_distance)  # Calculate weights
  normalized_weights<-weights/sum(weights)
  interpolated_wind_value <- sum(normalized_weights * corner_values)  
  return(interpolated_wind_value) 
  
  
}



# --------------------------------------------------------------------------
# Part 4: Main Interpolation Process
# --------------------------------------------------------------------------

# Initialize columns for interpolated variables
for (Env_var in variables_to_interpolate) {
  col_name <- paste0(Env_var, "_interpolated") 
  tracking_data[col_name] <- NA 
}



# Loop through each tracking point and perform spatio-temporal interpolation
for (i in 1:nrow(tracking_data)) {
  point <- tracking_data[i, ]
  
  # Locate the closest ERA5 timesteps for interpolation
  time_result <- find_closest_timesteps(nc_data, point$timePosix)
  before_idx <- time_result$before_idx
  after_idx <- time_result$after_idx
  
  # Find the longitude and latitude indices in the NetCDF grid
  x_long = (point$location.long - (-2)) / 0.25
  long_index = round(x_long) + 1 
  x_lat = (79 - point$location.lat) / 0.25
  lat_index = round(x_lat) + 1
  long_center = (-2) + (long_index-1) * 0.25  
  lat_center = 79 - (lat_index-1) * 0.25  
  
  # Interpolation for each environmental variable
  for (Env_var in variables_to_interpolate) {
    before_data <- ncvar_get(nc_data, varid = Env_var, 
                             start = c(1, 1, before_idx), 
                             count = c(-1, -1, 1)) 
    after_data <- ncvar_get(nc_data, varid = Env_var, 
                            start = c(1, 1, after_idx), 
                            count = c(-1, -1, 1)) 
    
    before_corners <- calculate_corner_var(before_data, long_index, lat_index)
    after_corners <- calculate_corner_var(after_data, long_index, lat_index)
    
    before_estimated_value <- bilinear_interpolation(before_corners,point$location.long, point$location.lat, long_center, lat_center)
    after_estimated_value <- bilinear_interpolation(after_corners, point$location.long, point$location.lat, long_center, lat_center)
    
    # Temporal interpolation
    before_timediff <- abs(as.numeric(difftime(point$timePosix, time_result$timestamp_of_before_data, units = "secs"))) 
    after_timediff <- abs(as.numeric(difftime(point$timePosix, time_result$timestamp_of_after_data, units = "secs") ))
    
    # Calculate the weights based on the time differences
    total_timediff <- before_timediff + after_timediff
    before_weight <- 1 - before_timediff / total_timediff
    after_weight <- 1 - after_timediff / total_timediff
    
    # Normalized weights
    before_weightNorm<- before_weight/(abs(before_weight)+abs(after_weight))
    after_weightNorm <- after_weight/(abs(before_weight)+abs(after_weight))
    
    # Final interpolated value using weighted average of before and after
    interpolated_value <- before_weightNorm * before_estimated_value + after_weightNorm * after_estimated_value
    
    # Store interpolated value
    col_name <- paste0(Env_var, "_interpolated") 
    tracking_data[i, col_name] <- interpolated_value 
  }
  
}


# --------------------------------------------------------------------------
# Part 5: Save the Results
# --------------------------------------------------------------------------

# Write the final dataset with interpolated variables to a CSV file
write.csv(tracking_data, "GeoMag_Env_Spring_2020.csv",row.names = FALSE)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
