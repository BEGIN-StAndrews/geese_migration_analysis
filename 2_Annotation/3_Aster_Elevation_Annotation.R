# ------------------------------------------------------------------------------
#  Annotate Tracking Data with ASTER Elevation Data using Bilinear Interpolation
# ------------------------------------------------------------------------------

# Description:
# This script integrates ASTER elevation data with bird tracking data by using 
# bilinear interpolation. It processes multiple ASTER GeoTIFF files using a 
# Virtual Raster (VRT) for efficient handling of large datasets. Elevation values 
# are extracted for each tracking point and appended to the tracking data.
# 
# Data Source:
# ASTER Global Digital Elevation Model (GDEM) Version 3:
# - Produced by NASA and Japan's Ministry of Economy, Trade, and Industry (METI).
# - Resolution: 1 arc-second (~30 meters).
# - Distributed in GeoTIFF format via NASA's LP DAAC.
# - Link: [https://lpdaac.usgs.gov/products/astgtmv003/]
# 
# Input:
#   - ASTER GeoTIFF files from the specified directory.
#   - Tracking data file: "Autumn_all_DistCoast.csv".
# 
# Output:
#   - Annotated tracking data with elevation information: "Autumn_all_Elevation.csv".
# 
# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("sp", quietly = TRUE)) install.packages("sp")
if (!requireNamespace("rgdal", quietly = TRUE)) install.packages("rgdal")

library(terra)    # For handling raster data and virtual raster tiles (VRT)
library(sp)       # For spatial data manipulation
library(rgdal)    # For CRS and spatial data compatibility



# ------------------------------------------------------------------------------
# Part 1: Load and Prepare Elevation Data
# ------------------------------------------------------------------------------

rm(list=ls())

# List all the downloaded ASTER GeoTIFF files in the specified directory
dem_files <- list.files(path = "C:/Users/am636/Desktop/publish/DTM/ASTGTM_003-20241031_114315/", pattern = "\\.tif$", full.names = TRUE)

# Create a Virtual Raster (VRT) for efficient access to large datasets
# VRT allows processing without loading all GeoTIFF files into memory
dem_vrt <- vrt(dem_files)


# --------------------------------------------------------------------------
# Part 2: Load and Prepare Tracking Data
# --------------------------------------------------------------------------

# Load bird tracking data
tracking_data <- read.csv("Autumn_all_DistCoast.csv")


# Convert tracking data to a SpatialPointsDataFrame for spatial operations
coordinates(tracking_data) <- ~location.long + location.lat
proj4string(tracking_data) <- CRS("+proj=longlat +datum=WGS84")

# Convert tracking data to a 'terra' SpatVector for raster extraction
tracking_points <- vect(tracking_data, geom = c("location.long", "location.lat"), crs = "EPSG:4326")


# ------------------------------------------------------------------------------
# Part 3: Extract Elevation for Each Tracking Point using Bilinear Interpolation
# ------------------------------------------------------------------------------
# Extract elevation values for each tracking point using bilinear interpolation
extracted_values <- extract(dem_vrt, tracking_points, method = 'bilinear')

# Add the extracted elevation values to the tracking data
tracking_data$ASTER.Elevation <- extracted_values[, 2]  # Assuming the elevation values are in the second column

# Convert SpatialPointsDataFrame back to a regular data frame
df<- as.data.frame(tracking_data)

# --------------------------------------------------------------------------
# Part 4: Save the Annotated Tracking Data
# --------------------------------------------------------------------------

# Save the updated tracking data with elevation information
write.csv(df, "Autumn_all_Elevation.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
