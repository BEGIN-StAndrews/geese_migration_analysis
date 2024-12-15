# ------------------------------------------------------------------------------
# Calculating Wind Support and Crosswind for Migratory Flights
# ------------------------------------------------------------------------------


# Description:
# This script calculates wind support and crosswind components for the migratory flights 
# of greater white-fronted geese (Anser albifrons). Using interpolated wind data and the 
# bird's movement vector, it computes the angle between the movement and wind vectors, 
# and derives wind support and crosswind components for each GPS fix.
# 
# Input:
#   - Processed dataset with interpolated wind data: 'Spring_DayNight.csv'.
# 
# Output:
#   - Updated dataset with wind-related parameters: 'Spring_WindParameters.csv'.
# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# ------------------------------------------------------------------------------
# Step 1: Load the Dataset
# ------------------------------------------------------------------------------
# Load the processed tracking data with interpolated wind data
data <- read.csv("Spring_DayNight.csv")


# ------------------------------------------------------------------------------
# Step 2: Define Helper Function
# ------------------------------------------------------------------------------
# Function to calculate the angle between two vectors (movement and wind)
calculate_angle <- function(u1, v1, u2, v2) {
  dot_product <- u1 * u2 + v1 * v2
  magnitude1 <- sqrt(u1^2 + v1^2)
  magnitude2 <- sqrt(u2^2 + v2^2)
  
  angle <- acos(dot_product / (magnitude1 * magnitude2)) * (180 / pi)
  return(angle)
}


# ------------------------------------------------------------------------------
# Step 3: Calculate Wind Support and Crosswind Components
# ------------------------------------------------------------------------------
# Initialize empty vectors to store calculated values
angles <- c()
crosswinds <- c()
wind_supports <- c()
wind_speeds <- c()



# Loop through each data point to calculate wind-related parameters
for (i in 1:nrow(data)) {
  # Calculate movement vector components based on bird's heading
  movement_u <- sin(data$CalHeading[i] * (pi / 180))   # X-component
  movement_v <- cos(data$CalHeading[i] * (pi / 180))   # Y-component
  
  # Calculate the angle between the movement and wind vectors
  angle <- calculate_angle(movement_u, movement_v, data$u100_interpolated[i], data$v100_interpolated[i])
  
  # Calculate wind speed
  wind_speed <- sqrt(data$u100_interpolated[i]^2 + data$v100_interpolated[i]^2)
  
  # Calculate crosswind and wind support components
  crosswind <- wind_speed * sin(angle * (pi / 180))
  wind_support <- wind_speed * cos(angle * (pi / 180))
  
  # Append calculated values to vectors
  wind_speeds <- c(wind_speeds, wind_speed)
  angles <- c(angles, angle)
  wind_supports <- c(wind_supports, wind_support)
  crosswinds <- c(crosswinds, crosswind)
}

# Add the calculated parameters to the dataset
data$wind_speed <- wind_speeds
data$wind_angle <- angles
data$wind_support <- wind_supports
data$crosswind <- crosswinds




# ------------------------------------------------------------------------------
# Step 4: Save the Updated Dataset
# ------------------------------------------------------------------------------
# Save the updated dataset with wind-related parameters
write.csv(data, "Spring_WindParameters.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------