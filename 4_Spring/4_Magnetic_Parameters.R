# ------------------------------------------------------------------------------
# Calculating Magnetic Heading and Apparent Angle of Geomagnetic Inclination
# ------------------------------------------------------------------------------

# Description:
# This script calculates the magnetic heading and apparent angle of geomagnetic 
# inclination for greater white-fronted geese (Anser albifrons). Using geomagnetic 
# declination (D) and inclination (I), it derives the magnetic heading and 
# apparent dip angle for each GPS fix in the tracking data.
# 
# Input:
#   - Dataset with wind parameters: 'Spring_WindParameters.csv'.
# 
# Output:
#   - Updated dataset with magnetic heading and apparent inclination: 'Spring_Magnetic.csv'.
# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# ------------------------------------------------------------------------------
# Step 1: Load Tracking Data
# ------------------------------------------------------------------------------
# Load the processed dataset with wind parameters
df <- read.csv("Spring_WindParameters.csv")

# ------------------------------------------------------------------------------
# Step 2: Define Helper Function
# ------------------------------------------------------------------------------
# Normalize angles to be between 0 and 360 degrees
normalize_angle <- function(angle) {
  angle %% 360
}


# ------------------------------------------------------------------------------
# Step 3: Calculate Magnetic Heading
# ------------------------------------------------------------------------------
# Magnetic heading is calculated as the bird's heading corrected by the geomagnetic declination (D)
df <- df %>% 
  mutate(MagHeading = normalize_angle(CalHeading - D))



# ------------------------------------------------------------------------------
# Step 4: Calculate Apparent Inclination
# ------------------------------------------------------------------------------
# Apparent inclination (apparent dip) is derived using the geomagnetic inclination (I) and magnetic heading
df <- df %>%
  mutate(apparent_dip = (180/pi) * asin(    sin(I * pi/180) /
                                              sqrt(sin(MagHeading * pi/180)^2 * cos(I * pi/180)^2 +
                                                     sin(I * pi/180)^2)  )  )


# ------------------------------------------------------------------------------
# Step 5: Save the Updated Dataset
# ------------------------------------------------------------------------------
# Save the dataset with magnetic heading and apparent inclination

write.csv(df,"Spring_Magnetic.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------