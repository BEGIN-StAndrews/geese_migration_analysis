# ------------------------------------------------------------------------------
# Calculating Clustering Features for Migration Analysis
# ------------------------------------------------------------------------------

# Description:
# This script calculates clustering features for migratory flights of greater white-fronted 
# geese (Anser albifrons). Using processed tracking data with geomagnetic, environmental, 
# and positional information, it calculates the change (delta) in various variables between 
# consecutive GPS fixes. These delta values will be used for clustering analysis to study 
# migratory behaviour.
# 
# Input:
#   - Processed dataset with magnetic and environmental variables: 'Autumn_Magnetic.csv'.
# 
# Output:
#   - Updated dataset with clustering features: 'Autumn_ClusteringFeatures.csv'.

# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# ------------------------------------------------------------------------------
# Step 1: Load Tracking Data
# ------------------------------------------------------------------------------
rm(list=ls())

# Load the processed dataset with magnetic and environmental variables
df <- read.csv("Autumn_Magnetic.csv")


# ------------------------------------------------------------------------------
# Step 2: Define Helper Functions
# ------------------------------------------------------------------------------
# Function to calculate the smallest degree difference (for angles)
calculate_smallest_degree_difference <- function(angle ) {
  diff1 = angle
  diff2 = 360 - diff1
  
  return(min(diff1, diff2))
}


# Function to calculate deltas within each ID group
calculate_delta_by_id <- function(x, id) {
  ave(x, id, FUN = function(x) c(diff(x), NA)) 
}



# ------------------------------------------------------------------------------
# Step 3: Calculate Clustering Features
# ------------------------------------------------------------------------------
df <- df %>%
  group_by(idyear) %>% 
  mutate(
    
    # Delta values for geomagnetic and environmental variables
    delta_intensity = abs(calculate_delta_by_id(F, idyear)),
    delta_apparent_dip = abs(calculate_delta_by_id(apparent_dip, idyear)),
    delta_MagHeading = abs(calculate_delta_by_id(MagHeading, idyear)),
    delta_inclination = abs(calculate_delta_by_id(I, idyear)),
    
  ) %>%
  ungroup() 



# ------------------------------------------------------------------------------
# Step 4: Calculate Smallest Degree Differences for Angles
# ------------------------------------------------------------------------------

df <- df %>%
  rowwise() %>% 
  mutate(
    delta_MagHeading2 = calculate_smallest_degree_difference(delta_MagHeading)
  ) %>%
  ungroup()


# ------------------------------------------------------------------------------
# Step 5: Save the Updated Dataset
# ------------------------------------------------------------------------------
# Save the dataset with calculated clustering features
write.csv(df,"Autumn_ClusteringFeatures.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
