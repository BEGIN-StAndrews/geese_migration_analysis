# ------------------------------------------------------------------------------
# Calculating Angular Deviations from Direct Paths in Migratory Bird Tracking Data
# ------------------------------------------------------------------------------

# Description:
# This script calculates angular deviations for greater white-fronted geese 
# (Anser albifrons) tracking data. It computes:
#   - True heading toward the final destination
#   - Deviation from this heading
#   - Deviation from the heading to the centre of the next stopover
# 
# Input:
#   - Processed tracking dataset with clustering features: 'Autumn_ClusteringFeatures.csv'.
#   - Stopover dataset: 'Stopovers.csv'.

# Note: 
# The annotated datasets ('Autumn_Migration_Annotated.csv' and 'Spring_Migration_Annotated.csv')
# along with identified 'Stopovers' are available in the Movebank study: 'Tracks of greater white-fronted
# geese annotated with geomagnetic and environmental information.' 
# Refer to the Movebank Data Repository for access details and the published paper 
# 'Multi-modal, interrelated navigation in migratory birds: a data mining study' for full methodology and preprocessing steps.

# 
# Output:
#   - Updated dataset with angular deviations: 'Autumn_Deviation.csv'.

# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("geosphere", quietly = TRUE)) install.packages("geosphere")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(dplyr)
library(geosphere)    
library(lubridate)    

# ------------------------------------------------------------------------------
# Step 1: Load Data
# ------------------------------------------------------------------------------
# Load tracking data and stopover data
tracking_data <- read.csv("Autumn_ClusteringFeatures.csv")
stopovers <- read.csv("stopovers.csv")


# Convert timestamp columns to POSIXct format
tracking_data$timestamp<- as.POSIXct(tracking_data$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
stopovers$timestamp.arrival<- as.POSIXct(stopovers$timestamp.arrival, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
stopovers$timestamp.departure<- as.POSIXct(stopovers$timestamp.departure, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")



# ------------------------------------------------------------------------------
# Step 2: Calculate Deviation from Destinations
# ------------------------------------------------------------------------------
# Function to calculate the true heading towards the final destination
calculate_heading_to_destination <- function(data, current_row) {
  
  bird_data <- subset(data, idyear == current_row$idyear)
  
    current_point <- current_row[c("location.long", "location.lat")]
  last_point <- tail(bird_data, n = 1)[c("location.long", "location.lat")]
  
  # Return NA if there’s no movement
  if (all(current_point == last_point)) {
    return(NA)
  }
  
  # Calculate the bearing and ensure it’s between 0 and 360 degrees
  true_heading <- bearing(current_point, last_point)
  
  if (true_heading < 0) {
    true_heading <- true_heading + 360
  }
  
  return(true_heading)
}



# Add columns for true heading and deviation from destination
tracking_data['True_Heading'] <- NA
tracking_data['Deviation'] <- NA


# Calculate true heading and deviation for each row
for (i in 1:nrow(tracking_data)) {
  
  tracking_data[i, 'True_Heading'] <- calculate_heading_to_destination(tracking_data, tracking_data[i,])
  tracking_data[i, 'Deviation'] <- abs(tracking_data[i, 'CalHeading'] - tracking_data[i, 'True_Heading'])
}


# Adjust deviation to account for the smallest angular difference (e.g., 10° vs. 350°)
Destdev<- tracking_data$Deviation
tracking_data$Deviation<- pmin(tracking_data$Deviation,360- Destdev)


# ------------------------------------------------------------------------------
# Step 3: Calculate Deviation from Next Stopover
# ------------------------------------------------------------------------------

# Add a column for heading to the next stopover
tracking_df<- tracking_data
tracking_df <- tracking_df %>%
  mutate(heading_to_stopover = NA)


# Calculate heading to the next stopover for each row
for (i in 1:nrow(tracking_df)) {
  
  current_point <- c(tracking_df$location.long[i], tracking_df$location.lat[i])
  current_id <- tracking_df$individual.local.identifier[i] 
  
  # Filter stopovers by individual ID and year
  filtered_stopovers <- stopovers %>%
    filter(individual.local.identifier == current_id,
           year(timestamp.arrival) == year(tracking_df$timestamp[i])) 
  
  # Find the closest stopover after the current timestamp
  next_stopover <- filtered_stopovers %>%
    filter(timestamp.arrival > tracking_df$timestamp[i]) %>%
    slice_min(order_by = timestamp.arrival)
  
  
  if (nrow(next_stopover) > 0) {
    stopover_center <- c(next_stopover$location.long, next_stopover$location.lat)
    tracking_df$heading_to_stopover[i] <- bearing(current_point, stopover_center)
    
    # Normalize heading_to_stopover
    if (tracking_df$heading_to_stopover[i] < 0) {
      tracking_df$heading_to_stopover[i] <- tracking_df$heading_to_stopover[i] + 360
    }
    
  }else{
    
    tracking_df$heading_to_stopover[i]<- NA
  }
  
  
  
}


# Calculate deviation from the next stopover heading
tracking_df <- tracking_df %>%
  mutate(deviation_from_stop = abs(CalHeading - heading_to_stopover))

# Adjust deviation to account for the smallest angular difference (e.g., 10° vs. 350°)
stopdev<- tracking_df$deviation_from_stop
tracking_df$deviation_from_stop<- pmin(tracking_df$deviation_from_stop,360- stopdev)



# ------------------------------------------------------------------------------
# Step 4: Save Final Dataset
# ------------------------------------------------------------------------------
# Remove rows with missing values and save the dataset
tracking_df2 <- na.omit(tracking_df) 
write.csv(tracking_df2,"Autumn_Deviation.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------

