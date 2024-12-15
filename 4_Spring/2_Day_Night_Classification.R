# ------------------------------------------------------------------------------
# Day/Night Classification in GPS Tracking Data of Geese Migration
# ------------------------------------------------------------------------------

# Description:
# This script determines whether each GPS data point for greater white-fronted geese 
# (Anser albifrons) during autumn migration occurred during day or night. Using geographic 
# location and timestamp, it calculates dawn and dusk times based on astronomical twilight 
# (solar depression angle of 18 degrees). The results are stored in a new column `night`, 
# where 1 indicates night and 0 indicates day. The annotated dataset is saved as a new file.
# 
# Input:
#   - Annotated GPS tracking data: 'Spring_FinalProcessed.csv'.
# 
# Output:
#   - Updated dataset with day/night classification: 'Spring_DayNight.csv'.
# 
# Data Source:
# - GPS tracking data processed in earlier steps of this pipeline.
# - Sunrise/sunset calculations performed using the `crepuscule` function from the `maptools` package.
# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("maptools", quietly = TRUE)) install.packages("maptools", repos = "http://R-Forge.R-project.org")

library(lubridate)  # For date-time operations
library(maptools)   # For dawn/dusk calculations using the `crepuscule` function

# ------------------------------------------------------------------------------
# Step 1: Load Tracking Data
# ------------------------------------------------------------------------------
# Load the processed GPS tracking data
migration_data <- read.csv("Spring_FinalProcessed.csv")
migration_data$timePosix <- as.POSIXct(migration_data$timePosix, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Add a `date` column for dawn/dusk calculations
migration_data$date <- as.Date(migration_data$timePosix)

# Initialize a column to store day/night classification
migration_data$night <- NA

# ------------------------------------------------------------------------------
# Step 2: Classify Each Data Point as Day or Night
# ------------------------------------------------------------------------------
# Loop through each row to classify based on dawn and dusk times
for (row in 1:nrow(migration_data)) { 
  
  # Extract location and date for the current row
  location <- matrix(c(migration_data[row, 'location.long'], migration_data[row, 'location.lat']), nrow = 1)
  date <- as.POSIXct(migration_data[row, 'date'], tz = 'UTC')
  
  # Calculate dawn and dusk times with a solar depression angle of 18 degrees
  dawn <- crepuscule(location, date, solarDep = 18, direction = "dawn", proj4string = CRS("+proj=longlat +datum=WGS84"), POSIXct.out = TRUE)
  dusk <- crepuscule(location, date, solarDep = 18, direction = "dusk", proj4string = CRS("+proj=longlat +datum=WGS84"), POSIXct.out = TRUE)
  
  # Extract the time component from dawn and dusk results
  dawn_time <- dawn$time[1]
  dusk_time <- dusk$time[1]
  actual_time <- as.POSIXct(migration_data[row, 'timePosix'], tz = 'UTC')
  
  # Classify as night (1) if before dawn or after dusk, or as day (0) otherwise
  if (is.na(dawn_time) || is.na(dusk_time)) {
    night_indicator <- 0  # Continuous day if dawn or dusk is missing
  } else if (actual_time < dawn_time || actual_time > dusk_time) {
    night_indicator <- 1
  } else {
    night_indicator <- 0
  }
  
  # Store the result in the 'night' column
  migration_data$night[row] <- night_indicator
}


# ------------------------------------------------------------------------------
# Step 3: Save the Updated Dataset
# ------------------------------------------------------------------------------
# Save the annotated dataset with day/night classification
write.csv(migration_data, "Spring_DayNight.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------