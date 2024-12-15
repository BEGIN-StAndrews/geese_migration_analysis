# ------------------------------------------------------------------------------
# Seasonal Migration Data Extraction for Greater White-Fronted Geese (2015–2023)
# ------------------------------------------------------------------------------

# Description:
# This script extracts and processes geese migration data for spring and autumn
# from 2015 to 2023. It removes tracks with low GPS points, filtering only those 
# with a count above the threshold, and saves seasonal data for each year as 
# separate CSV files.
# 
# Input:
#   - `cleaned_data_no_stops_no_lowspeed.csv`: Cleaned and filtered GPS tracking data
# 
# Output:
#   - Seasonal CSV files for each year: `Spring<year>.csv`, `Autumn<year>.csv`
#   - Aggregated seasonal CSV files: `Spring_all.csv`, `Autumn_all.csv`
# 
# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(dplyr)
library(tidyr)
library(lubridate)

# ------------------------------------------------------------------------------
# Step 1: Load the Cleaned Tracking Data
# ------------------------------------------------------------------------------

rm(list=ls())

# Load the cleaned tracking data
tracking_data <- read.csv("cleaned_data_no_stops_no_lowspeed.csv")


# Add an identifier combining individual ID and year
tracking_data$timePosix <- as.POSIXct(tracking_data$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
tracking_data$idyear<-paste0(tracking_data$individual.local.identifier,"_",year(tracking_data$timePosix))



# Select relevant columns for processing
newOrder <- tracking_data %>% 
  dplyr::select(timestamp, location.long, location.lat,ground.speed,height.above.msl, individual.local.identifier, idyear,timePosix, CalSpeed,CalTurnAngle, CalHeading  )


# ------------------------------------------------------------------------------
# Step 2: Extract and Process Spring Data (2015–2023)
# ------------------------------------------------------------------------------
threshold <- 100  # Minimum number of points required for a track


for (year in 2015:2023) {
  
  # Filter data for the specified year and Spring period (March 1st to May 31st)
  spring_data <- newOrder %>%
    filter(
      year(timePosix) == year &     
        month(timePosix) > 2 &        
        month(timePosix) < 6           
    )
  
  # Remove tracks with insufficient data
  idyear_counts <- spring_data %>% group_by(idyear) %>% summarize(count = n())
  spring_data <- spring_data %>% filter(idyear %in% idyear_counts$idyear[idyear_counts$count >= threshold])
  
  # Save the Spring data for the current year
  write.csv(spring_data, paste0("Spring", year, ".csv"), row.names = FALSE)
}





# ------------------------------------------------------------------------------
# Step 3: Extract and Process Autumn Data (2016–2022)
# ------------------------------------------------------------------------------
threshold <- 100  # Minimum number of points required for a track


for (year in 2016:2022) {
  # Filter data for the specified year and Autumn period (August 15th to November 15th)
  autumn_data <- newOrder %>%
    filter(
      year(timePosix) == year &
        (
          (month(timePosix) == 8 & day(timePosix) > 14) |  # From August 15th
            (month(timePosix) > 8 & month(timePosix) < 11) | # September and October
            (month(timePosix) == 11 & day(timePosix) < 16)   # Up to November 15th
        )
    )
  
  # Remove tracks with insufficient data
  idyear_counts <- autumn_data %>% group_by(idyear) %>% summarize(count = n())
  autumn_data <- autumn_data %>% filter(idyear %in% idyear_counts$idyear[idyear_counts$count >= threshold])
  
  # Save the Autumn data for the current year
  write.csv(autumn_data, paste0("Autumn", year, ".csv"), row.names = FALSE)
}



# ------------------------------------------------------------------------------
# Step 4: Aggregate Spring Data
# ------------------------------------------------------------------------------


rm(list=ls())


# Load Spring data for all years
df1<- read.csv("Spring2015.csv")
df2<- read.csv("Spring2016.csv")
df3<- read.csv("Spring2017.csv")
df4<- read.csv("Spring2018.csv")
df5<- read.csv("Spring2019.csv")
df6<- read.csv("Spring2020.csv")
df7<- read.csv("Spring2021.csv")
df8<- read.csv("Spring2022.csv")
df9<- read.csv("Spring2023.csv")


# Combine Spring data into one dataframe
df<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)

# Save aggregated Spring data
write.csv(df, "Spring_all.csv", row.names = FALSE)



# ------------------------------------------------------------------------------
# Step 5: Aggregate Autumn Data
# ------------------------------------------------------------------------------
rm(list = ls())  


# Load Autumn data for all years
df1<- read.csv("Autumn2016.csv")
df2<- read.csv("Autumn2017.csv")
df3<- read.csv("Autumn2018.csv")
df4<- read.csv("Autumn2019.csv")
df5<- read.csv("Autumn2020.csv")
df6<- read.csv("Autumn2021.csv")
df7<- read.csv("Autumn2022.csv")


# Combine Autumn data into one dataframe
df <- rbind(df1, df2, df3, df4, df5, df6, df7)

# Save aggregated Autumn data
write.csv(df, "Autumn_all.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
