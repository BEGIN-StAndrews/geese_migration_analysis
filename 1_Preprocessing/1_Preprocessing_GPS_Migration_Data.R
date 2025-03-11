# ------------------------------------------------------------------------------
# Preprocessing GPS Tracking Data of Greater White-fronted Geese (Anser albifrons)
# ------------------------------------------------------------------------------
# Author: Ali Moayedi
# Email: am636@st-andrews.ac.uk
# Date: October 2024
# 
# Description:
# This script preprocesses GPS tracking data for greater white-fronted geese,
# performing the following steps:
#   1. Removing insufficient satellite fixes
#   2. Filtering GPS bursts
#   3. Removing inconsistent locations
#   4. Filtering tracks by length
#   5. Removing outliers and individuals that changed population
#   6. Aggregating data across datasets
#   7. Removing dependent individuals
# 
# Input: CSV files containing raw GPS tracking data.
# Output: A cleaned and aggregated dataset saved as `Final_Aggregated_Data.csv`.
# 
# Note:
# This script is part of the study: "Multi-modal, interrelated navigation in migratory birds: a data mining study"
# Please refer to the published paper for detailed methodology and dataset access.
# ------------------------------------------------------------------------------

rm(list = ls())  

# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("move", quietly = TRUE)) install.packages("move")

library(dplyr)
library(move)

# ------------------------------------------------------------------------------
# Define Custom Functions
# ------------------------------------------------------------------------------

# Function to remove insufficient satellite fixes
remove_low_fixes <- function(data) {
  cleaned_data <- subset(data, gps.satellite.count > 3)
  cleaned_data <- cleaned_data[!duplicated(cleaned_data$timestamp), ]
  return(cleaned_data)
}

# Function to filter GPS bursts
filter_gps_bursts <- function(data) {
  data$timestamp2 <- as.POSIXct(data$timestamp, tz = "UTC")
  filtered_data <- data %>%
    mutate(time_diff = timestamp2 - lag(timestamp2, default = first(timestamp2))) %>%
    filter(time_diff > 10 | is.na(time_diff))
  return(filtered_data)
}

# Function to remove inconsistent locations based on expected range
remove_inconsistent_locations <- function(data) {
  data_filtered <- data %>%
    filter(location.long > 0 & location.long < 115 &
             location.lat > 44 & location.lat < 78)
  return(data_filtered)
}

# Function to calculate track lengths for each individual
filter_by_track_length <- function(data, threshold = 100) {
  track_lengths <- data.frame(
    individual.local.identifier = unique(data$individual.local.identifier),
    Length = sapply(unique(data$individual.local.identifier), 
                    function(id) sum(data$individual.local.identifier == id))
  )
  data_filtered <- data[data$individual.local.identifier %in% 
                          track_lengths$individual.local.identifier[
                            track_lengths$Length >= threshold], ]
  return(data_filtered)
}

# Function to remove outliers and individuals that changed population
remove_outliers_and_south <- function(data) {
  # Combine all known outliers and south IDs, identified using QGIS
  outlierIDs <- c("474_KOL_F")
  southIDs <- c("A25_KOL_F", "62_Wolodja_J", "20_Aleska_F", 
                "GWFG_2015_424", "85_KOL_F", "Lily_17", 
                "16_Clementina_F", "15_Kamilla_F", 
                "22_Alissa_F", "21_Kelemen_M")
  
  combined_ids <- unique(c(outlierIDs, southIDs))
  
  # Remove these IDs if they exist in the dataset
  filtered_data <- data[!data$individual.local.identifier %in% combined_ids, ]
  return(filtered_data)
}

# ------------------------------------------------------------------------------
# Process Each Dataset
# ------------------------------------------------------------------------------

datasets <- list("Disturbance of GWFG by IFV and IWWR 2017-2018.csv",
                 "Disturbance of GWFG by IFV and IWWR.csv",
                 "Geese_MPIAB_Kolguev2016.csv",
                 "LifeTrack Geese IG-RAS MPIAB ICARUS 2.csv",
                 "Geese_MPIAB_IFV_IWWR_Kolguev_HUN2018.csv")


for (dataset in datasets) {
  print(dataset)
  # Load the dataset
  df <- read.csv(dataset)
  
  # Step 1: Remove insufficient satellite fixes
  df_cleaned <- remove_low_fixes(df)
  
  # Step 2: Filter GPS bursts
  df_burst_filtered <- filter_gps_bursts(df_cleaned)
  
  # Step 3: Remove inconsistent locations
  df_filtered_locations <- remove_inconsistent_locations(df_burst_filtered)
  
  # Step 4: Filter by track length
  df_filtered_by_track_length <- filter_by_track_length(df_filtered_locations, threshold = 100)
  
  # Step 5: Remove outliers and individuals that changed population
  df_filtered_final <- remove_outliers_and_south(df_filtered_by_track_length)
  
  # Save intermediate results
  write.csv(df_filtered_final, paste0("Cleaned_", dataset), row.names = FALSE)
}

# ------------------------------------------------------------------------------
# Aggregate Processed Data
# ------------------------------------------------------------------------------

processed_files <- list.files(pattern = "^Cleaned_.*\\.csv$")
processed_data_list <- lapply(processed_files, read.csv)

select_common_columns <- function(data_list) {
  common_cols <- Reduce(intersect, lapply(data_list, colnames))
  data_list_common <- lapply(data_list, function(df) df[, common_cols, drop = FALSE])
  return(data_list_common)
}

processed_data_list_common <- select_common_columns(processed_data_list)
aggregated_data <- bind_rows(processed_data_list_common)

# Filter for the target species (Anser albifrons)
final_aggregated_data <- aggregated_data %>%
  filter(individual.taxon.canonical.name == "Anser albifrons")

# Remove dependent individuals
dependent_ids <- c("44_Adriana_F", "45_Adele_J", "47_Adam_J", "50_Tina_J", 
                   "53_Eva_F", "56_Evita_J", "59_Wolka_F", "62_Wolodja_J", 
                   "67_Jasminka_J", "70_Janika_F", "83_Charlotta_J", "84_Charly_J", 
                   "48_Tineke_F", "46_Adriaan_J", "82_Chris_M")

final_aggregated_data <- final_aggregated_data %>%
  filter(!individual.local.identifier %in% dependent_ids)

# Save the final aggregated dataset
write.csv(final_aggregated_data, "Final_Aggregated_Data.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
