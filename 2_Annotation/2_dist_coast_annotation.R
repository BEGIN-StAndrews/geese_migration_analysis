# ------------------------------------------------------------------------------
# Annotate Tracking Data with Distance to Coast Using Bilinear Interpolation
# ------------------------------------------------------------------------------

# Description:
# This script integrates distance-to-coast data with bird tracking data by 
# using bilinear interpolation. It reads a compressed distance-to-coast dataset, 
# crops the data to the region of interest, and extracts the interpolated distance
# value for each tracking point.
# 
# Data Source:
# Distance to the Nearest Coast
# - Created by NASA's Ocean Biology Processing Group in June 2009.
# - Original 0.04-degree resolution data set, signed version:
#   - Negative distances represent locations over land.
# - For more details: [https://oceancolor.gsfc.nasa.gov/resources/docs/distfromcoast/]
# 
# Input:
#   - `dist2coast.signed.txt.bz2`: Compressed file with distance-to-coast data
#   - `Autumn_all_Mag.csv`: Bird tracking data for autumn migration
# 
# Output:
#   - `Autumn_all_DistCoast.csv`: Annotated tracking data with distance to coast
# 
# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("raster", quietly = TRUE)) install.packages("raster")
if (!requireNamespace("sp", quietly = TRUE)) install.packages("sp")
if (!requireNamespace("rgdal", quietly = TRUE)) install.packages("rgdal")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(raster)
library(sp)
library(rgdal)
library(dplyr)



# --------------------------------------------------------------------------
# Part 1: Load and Prepare Distance to Coast Data
# --------------------------------------------------------------------------

# Specify the path to the compressed distance-to-coast data
dist_coast_file <- "dist2coast.signed.txt.bz2"

# Read the compressed distance-to-coast data file
dist_coast_data <- read.table(bzfile(dist_coast_file), header = FALSE)
colnames(dist_coast_data) <- c("longitude", "latitude", "distance")

# Create a raster from the distance to coast data
dist_coast_raster <- rasterFromXYZ(dist_coast_data)

# Assign a Coordinate Reference System (CRS) to the raster (WGS84)
crs(dist_coast_raster) <- CRS("+proj=longlat +datum=WGS84")


# ------------------------------------------------------------------------------
# Part 2: Crop the Distance-to-Coast Raster to the Region of Interest
# ------------------------------------------------------------------------------


# Define the geographic extent based on the tracking data coverage
xmin <- -2   # Minimum longitude
xmax <- 120  # Maximum longitude
ymin <- 40   # Minimum latitude
ymax <- 85   # Maximum latitude
desired_extent <- extent(xmin, xmax, ymin, ymax)

# Crop the raster to the specified extent
dist_coast_raster_cropped <- crop(dist_coast_raster, desired_extent)



# ------------------------------------------------------------------------------
# Part 3: Load and Prepare Tracking Data
# ------------------------------------------------------------------------------
# Load bird tracking data
tracking_data <- read.csv("Autumn_all_Mag.csv")


# Convert tracking data to a SpatialPointsDataFrame for spatial operations
coordinates(tracking_data) <- ~location.long + location.lat
proj4string(tracking_data) <- CRS("+proj=longlat +datum=WGS84")

# ------------------------------------------------------------------------------
# Part 4: Extract Distance-to-Coast for Each Tracking Point
# ------------------------------------------------------------------------------
# Use bilinear interpolation to extract distance-to-coast values for each point
tracking_data$Distance.to.Coast <- extract(dist_coast_raster_cropped, tracking_data, method = 'bilinear')



# ------------------------------------------------------------------------------
# Part 5: Save the Annotated Tracking Data
# ------------------------------------------------------------------------------

# Convert the SpatialPointsDataFrame back to a regular data frame
tracking_data_df <- as.data.frame(tracking_data)

# Save the updated tracking data with distance-to-coast information
write.csv(tracking_data_df, "Autumn_all_DistCoast.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
