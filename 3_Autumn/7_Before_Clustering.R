# ------------------------------------------------------------------------------
# Data Filtering and Preparation for Clustering Analysis
# ------------------------------------------------------------------------------


# Description:
# This script filters migratory tracking data based on day/night classification 
# and time intervals, identifies outliers in clustering features, scales the 
# data for clustering analysis, and saves the processed datasets separately 
# for day and night.
# 
# Input:
#   - Dataset with angular deviations and other features: 'Autumn_Deviation.csv'.
# 
# Output:
#   - Processed day clustering features: 'Autumn_Day_CF.csv'.
#   - Processed night clustering features: 'Autumn_Night_CF.csv'.

# Note:
# The files "Autumn_Day_CF.csv" and "Autumn_Night_CF.csv" generated in this script
# will be used as input for the clustering process within the "5_Clustering" folder.
# Clustering is performed in Python due to R's limitations in handling large datasets.

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# ------------------------------------------------------------------------------
# Step 1: Load Tracking Data
# ------------------------------------------------------------------------------
# Load the dataset with deviations and other features
tracking_data <- read.csv("Autumn_Deviation.csv")

# Convert timestamp column to POSIXct format
tracking_data$timePosix <- as.POSIXct(tracking_data$timePosix, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# ------------------------------------------------------------------------------
# Step 2: Calculate Time Intervals and Filter Data
# ------------------------------------------------------------------------------

# Calculate time intervals between consecutive GPS fixes
tracking_data <- tracking_data %>%
  group_by(idyear) %>%
  mutate(time_interval = difftime(lead(timePosix), timePosix, units = 'mins'))

# Filter data for time intervals less than 10 minutes
filtered_data <- tracking_data %>% filter(time_interval < 10)

# Summarize time interval statistics for reference
mean_time_interval <- mean(filtered_data$time_interval, na.rm = TRUE)
median_time_interval <- median(filtered_data$time_interval, na.rm = TRUE)


# ------------------------------------------------------------------------------
# Step 3: Filter Day and Night Data
# ------------------------------------------------------------------------------

# Split the dataset into day and night subsets based on the 'night' column
day_data <- filtered_data %>%
  filter(night == 0) %>% # 
  group_by(idyear) %>%
  filter(n() >= 80) %>%  # Retain IDs with at least 80 data points
  ungroup()

night_data <- filtered_data %>%
  filter(night == 1) %>% 
  group_by(idyear) %>%
  filter(n() >= 20) %>%  # Retain IDs with at least 20 data points
  ungroup()


# ------------------------------------------------------------------------------
# Step 4: Select Clustering Features
# ------------------------------------------------------------------------------

# Define the clustering features for day and night datasets
clustering_vars_day <- day_data %>%
  dplyr::select(delta_MagHeading2, delta_apparent_dip, delta_inclination,
         wind_support, crosswind, Distance.to.Coast)

clustering_vars_night <- night_data %>%
  dplyr::select(delta_MagHeading2, delta_apparent_dip, delta_inclination,
         wind_support, crosswind, Distance.to.Coast)



# ------------------------------------------------------------------------------
# Step 5: Remove Outliers and Scale Features
# ------------------------------------------------------------------------------

# Define an outlier threshold (number of standard deviations)
outlier_threshold <- 5

# Filter outliers and scale data for day
day_filtered <- clustering_vars_day %>%
  filter(if_all(everything(), ~ abs(. - mean(.)) <= outlier_threshold * sd(.))) %>%  # 50614
  scale() %>%
  as.data.frame()


# Filter outliers and scale data for night
night_filtered <- clustering_vars_night %>%
  filter(if_all(everything(), ~ abs(. - mean(.)) <= outlier_threshold * sd(.))) %>% 
  scale() %>%
  as.data.frame()



# ------------------------------------------------------------------------------
# Step 6: Save Processed Datasets
# ------------------------------------------------------------------------------
# Save the processed clustering features for day and night
write.csv(day_filtered, "Autumn_Day_CF.csv", row.names = FALSE)     
write.csv(night_filtered, "Autumn_Night_CF.csv", row.names = FALSE) 


# ------------------------------------------------------------------------------
# Note:
# The files "Autumn_Day_CF.csv" and "Autumn_Night_CF.csv" generated in this script
# will be used as input for the clustering process within the "5_Clustering" folder.
# Clustering is performed in Python due to R's limitations in handling large datasets.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------