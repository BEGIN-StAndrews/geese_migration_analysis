# ------------------------------------------------------------------------------
# Removing Non-Flight Data Points from GPS Tracking Data
# ------------------------------------------------------------------------------

# Description:
# This script preprocesses GPS tracking data for greater white-fronted geese 
# (Anser albifrons) by performing the following:
#   1. Removing data points overlapping with stopovers, nesting, and moulting sites
#   2. Filtering low-speed points to retain active migratory behaviours
# 
# Input: 
#   - `Final_Aggregated_Data.csv`: Cleaned and aggregated tracking dataset
#   - `stopover_results.csv`: Detected stopover data
# 
# Output:
#   - `filtered_stopovers.csv`: Non-nested, non-redundant stopover data
#   - `cleaned_data_no_stops.csv`: Tracking data without stopover points
#   - `cleaned_data_no_stops_no_lowspeed.csv`: Cleaned dataset after low-speed filtering
# 

# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("move", quietly = TRUE)) install.packages("move")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("geosphere", quietly = TRUE)) install.packages("geosphere")

library(dplyr)
library(move)
library(lubridate)
library(geosphere)



# ------------------------------------------------------------------------------
# Define Helper Functions
# ------------------------------------------------------------------------------

# Convert lat/long coordinates to 3D Cartesian coordinates
latlong_to_xyz <- function(latlong_coords) {
  c(cos(latlong_coords[1]) * c(cos(latlong_coords[2]), sin(latlong_coords[2])), sin(latlong_coords[1]))
}

# Convert 3D Cartesian coordinates back to lat/long
xyz_to_latlong <- function(xyz_coords) {
  c(atan2(xyz_coords[3], sqrt(sum(xyz_coords[1:2]^2))), atan2(xyz_coords[2], xyz_coords[1]))
}


# Generating a Circle on a Sphere
generate_sphere_circle <- function(latitude, longitude, radius, sphere_radius = 6371008.8, num_points = 360) {
  circle_radius <- sin(radius / sphere_radius)
  angle_seq <- (2 * pi / num_points) * (0:num_points)
  points_matrix <- matrix(
    c(circle_radius * cos(angle_seq), circle_radius * sin(angle_seq), rep(sqrt(1 - circle_radius^2), num_points + 1)),
    ncol = 3
  )
  lat_angle <- -pi * (0.5 - (latitude / 180))
  rotation_y <- matrix(c(cos(lat_angle), 0, sin(lat_angle), 0, 1, 0, -sin(lat_angle), 0, cos(lat_angle)), nrow = 3)
  lon_angle <- longitude / 180 * pi
  rotation_z <- matrix(c(cos(lon_angle), sin(lon_angle), 0, -sin(lon_angle), cos(lon_angle), 0, 0, 0, 1), nrow = 3)
  rotation_matrix <- rotation_z %*% rotation_y
  points_matrix <- t(apply(points_matrix, 1, function(p) {
    rotation_matrix %*% p
  }))
  points_matrix <- t(apply(points_matrix, 1, xyz_to_latlong) * (180 / pi))
  max_index <- which.max(points_matrix[, 2])
  if (max_index != nrow(points_matrix)) {
    points_matrix <- points_matrix[c((max_index + 1):nrow(points_matrix), 1:max_index), ]
  }
  points_matrix
}


# Distance Calculations
dist_cartesian <- function(p, q) {
  sqrt(sum((p - q)^2))
}
dist_function <- dist_cartesian

dist_latlong <- function(p, q) {
  delta <- q - p
  a <- sin(delta[1] / 2)^2 + cos(p[1]) * cos(q[1]) * sin(delta[2] / 2)^2
  2 * asin(min(1, sqrt(a)))
}


# Calculating Midpoints
midpoint_cartesian <- function(p, q) {
  (p + q) * 0.5
}
midpoint_function <- midpoint_cartesian

midpoint_latlong <- function(p, q) {
  xyz_to_latlong(midpoint_cartesian(latlong_to_xyz(p), latlong_to_xyz(q)))
}


# Calculating Circumcircles
circumcircle_cartesian <- function(p, q, r) {
  l  <- function(x) { sqrt(sum(x^2)) }
  l2 <- function(x) { sum(x^2) }
  q <- q - p
  r <- r - p
  qxr2 <- l2(q) * l2(r) - sum(q * r)^2  # ||q cross r||^2
  d <- (l2(q) * r - l2(r) * q)
  center <- (sum(d * r) * q - sum(d * q) * r) / (2 * qxr2)
  radius <- l(q) * l(r) * l(q - r) / (2 * sqrt(qxr2))
  c(center + p, radius)
}
circumcircle_function <- circumcircle_cartesian

circumcircle_latlong <- function(p, q, r) {
  center <- xyz_to_latlong(circumcircle_cartesian(latlong_to_xyz(p), latlong_to_xyz(q), latlong_to_xyz(r))[1:3])
  c(center, dist_latlong(center, p))  # Recompute radius along great circle
}



# The MiniDisc Function
calculate_minidisc <- function(points_matrix, max_radius = Inf, sphere_radius = Inf) {
  if (nrow(points_matrix) <= 1) {
    if (nrow(points_matrix) == 0) {
      return(NULL)
    }
    disc <- c(points_matrix, 0)
  } else {
    miniDiscWithPoints <- function(i, Q) {
      init_points <- rbind(Q, points_matrix[1:2, ])[1:2, ]
      disc <- c(
        midpoint_function(init_points[1, ], init_points[2, ]),
        dist_function(init_points[1, ], init_points[2, ]) / 2
      )
      if (disc[length(disc)] > max_radius) {
        return(NULL)
      }
      for (j in (2 - nrow(Q) + seq_len(i - 2 + nrow(Q)))) {
        if (dist_function(points_matrix[j, ], disc[-length(disc)]) > disc[length(disc)]) {
          if (nrow(Q) >= 2) {
            disc <- circumcircle_function(Q[1, ], Q[2, ], points_matrix[j, ])
            if (disc[length(disc)] > max_radius) {
              return(NULL)
            }
          } else {
            disc <- miniDiscWithPoints(j - 1, rbind(Q, points_matrix[j, ]))
            if (is.null(disc)) { return(disc) }
          }
        }
      }
      disc
    }
    
    if (is.finite(sphere_radius)) {
      max_radius <- max_radius / sphere_radius
      points_matrix <- points_matrix * (pi / 180)
      dist_function <- dist_latlong
      midpoint_function <- midpoint_latlong
      circumcircle_function <- circumcircle_latlong
    }
    
    points_matrix <- unique(points_matrix)
    if (nrow(points_matrix) == 1) {
      disc <- c(points_matrix, 0)
    } else {
      perm <- c(1, nrow(points_matrix), 1 + sample(seq_len(nrow(points_matrix) - 2), size = nrow(points_matrix) - 2, replace = FALSE))
      points_matrix <- points_matrix[perm, ]
      disc <- miniDiscWithPoints(i = nrow(points_matrix), Q = matrix(double(0), ncol = ncol(points_matrix)))
    }
  }
  
  if (!is.null(disc)) {
    if (is.infinite(sphere_radius)) {
      names(disc) <- c("x", "y", "r")
    } else {
      disc[1:2] <- disc[1:2] * (180 / pi)
      disc[3] <- disc[3] * sphere_radius
      names(disc) <- c("lat", "long", "r")
    }
  }
  disc
}



# Stopover Detection Function
detect_stopovers <- function(track_data, min_time, max_radius) {
  if (length(grep("+proj=longlat", track_data@proj4string, value = FALSE)) >= 1) {
    dist_function <- dist_latlong
    sphere_radius <- 6371008.8
    rm <- max_radius / sphere_radius
  } else {
    sphere_radius <- Inf
    rm <- max_radius
  }
  min_time <- as.difftime(min_time, units = "secs")
  
  timestamps_vector <- track_data@timestamps
  coordinates_matrix <- track_data@coords[, 2:1]
  
  stopovers_dataframe <- data.frame(
    iStart = integer(),
    iEnd = integer(),
    duration = integer(),
    cLat = double(),
    cLong = double(),
    radius = double()
  )
  start <- end <- 1
  while (end < nrow(track_data)) {
    while (timestamps_vector[end] - timestamps_vector[start] < min_time && end < nrow(track_data)) {
      end <- end + 1
    }
    
    if (timestamps_vector[end] - timestamps_vector[start] >= min_time && dist_function(coordinates_matrix[start, ], coordinates_matrix[end, ]) <= rm) {
      disc <- calculate_minidisc(coordinates_matrix[start:end, ], max_radius, sphere_radius)
      if (!is.null(disc)) {
        while (!is.null(disc) && end < nrow(coordinates_matrix)) {
          so <- c(start, end, as.double(timestamps_vector[end] - timestamps_vector[start], units = "secs"), disc)
          end <- min(end + (end - start), nrow(coordinates_matrix))
          disc <- calculate_minidisc(coordinates_matrix[start:end, ], max_radius, sphere_radius)
        }
        if (!is.null(disc)) {
          so <- c(start, end, as.double(timestamps_vector[end] - timestamps_vector[start], units = "secs"), disc)
        } else {
          while (end > so[2]) {
            m <- ceiling((so[2] + end) / 2)
            disc <- calculate_minidisc(coordinates_matrix[start:m, ], max_radius, sphere_radius)
            if (is.null(disc)) {
              end <- m - 1
            } else {
              so <- c(start, m, as.double(timestamps_vector[m] - timestamps_vector[start], units = "secs"), disc)
            }
          }
        }
        stopovers_dataframe[nrow(stopovers_dataframe) + 1, ] <- so
        start <- end <- so[2]
      }
    }
    start <- start + 1
  }
  
  if (is.infinite(sphere_radius)) {
    colnames(stopovers_dataframe) <- c("iStart", "iEnd", "duration", "cX", "cY", "radius")
  } else {
    colnames(stopovers_dataframe) <- c("iStart", "iEnd", "duration", "cLat", "cLong", "radius")
  }
  rownames(stopovers_dataframe) <- NULL
  
  stopovers_dataframe
}


# ------------------------------------------------------------------------------
# Step 1: Finding stopover areas
# ------------------------------------------------------------------------------

# Load cleaned tracking data
data_csv <- read.csv("Final_Aggregated_Data.csv") 

# Convert into a Move object
move_data <- move(
  x = data_csv$location.long,
  y = data_csv$location.lat,
  time = as.POSIXct(data_csv$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
  proj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
  data = data_csv,
  animal = data_csv$individual.local.identifier
)

# Define different stop types with their durations and radii
" stopover sites are defined as locations where individuals stayed within a 30 km radius
for more than 48 hours (van Wijk et al., 2012). Similarly, nesting and moulting sites were
identified based on longer durations and smaller radii (>10 days within 2 km for nesting, 
>21 days within 30 km for moulting) (Kölzsch et al., 2019).  "

stop_types_list <- list(
  Stopover = list(duration = 172800, radius = 30000),    # 2 days, 30 km
  Nest_site = list(duration = 864000, radius = 2000),    # 10 days, 2 km
  Moult_site = list(duration = 1814400, radius = 30000)  # 21 days, 30 km
)


# Set time zone to UTC for consistency in time operations
Sys.setenv(tz = "UTC")

# Remove duplicate data points
move_data <- move_data[!duplicated(paste0(round_date(timestamps(move_data), "5 min"), trackId(move_data))), ]





# Process each individual and each scenario-------------------------------------

data_individuals <- move::split(move_data)
stopover_table <- data.frame(
  "individual.local.identifier" = character(),
  "timestamp.arrival" = character(),
  "timestamp.departure" = character(),
  "location.long" = numeric(),
  "location.lat" = numeric(),
  "duration" = numeric(),
  "radius" = numeric(),
  "stop.type" = character()
)

foreach(individual_data = data_individuals) %do% {
  names(individual_data) <- make.names(names(individual_data), allow_ = FALSE)
  
  for (stopover_type in names(stop_types_list)) {
    stop_duration <- stop_types_list[[stopover_type]]$duration
    stop_radius <- stop_types_list[[stopover_type]]$radius
    
    stopover_results <- detect_stopovers(individual_data, stop_duration, stop_radius)
    
    if (nrow(stopover_results) == 0) {
      next
    }
    
    arrival_times <- timestamps(individual_data)[stopover_results$iStart]
    departure_times <- timestamps(individual_data)[stopover_results$iEnd]
    individual_ids <- rep(namesIndiv(individual_data), length(stopover_results$iStart))
    
    if (any(names(individual_data) == "individual.local.identifier")) {
      id_vector <- individual_data@data$individual.local.identifier[stopover_results$iStart]
    } else if (any(names(individual_data) == "local.identifier")) {
      id_vector <- individual_data@data$local.identifier[stopover_results$iStart]
    } else {
      id_vector <- individual_ids
    }
    
    durations <- stopover_results$duration  # in seconds
    num_rows <- length(durations)
    
    res_dataframe <- data.frame(
      "individual.local.identifier" = individual_ids,
      "timestamp.arrival" = arrival_times,
      "timestamp.departure" = departure_times,
      "location.long" = stopover_results$cLong,
      "location.lat" = stopover_results$cLat,
      "duration" = durations,
      "radius" = stopover_results$radius,
      "stop.type" = stopover_type
    )
    
    stopover_table <- rbind(stopover_table, res_dataframe)
  }
}

# Save the results to a single CSV file-----------------------------------------
write.csv(stopover_table, file = "stopover_results.csv", row.names = FALSE)



# ------------------------------------------------------------------------------
# Step 2: Filtering and removing nested or duplicate stopovers
# ------------------------------------------------------------------------------

rm(list=ls())


stopovers <- read.csv("stopover_results.csv")

stopovers$timestamp.arrival<- as.POSIXct(stopovers$timestamp.arrival, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
stopovers$timestamp.departure<- as.POSIXct(stopovers$timestamp.departure, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

stopovers<-stopovers %>%
  arrange(individual.local.identifier, timestamp.arrival)


stopovers_filtered <- stopovers %>% 
  group_by(individual.local.identifier, timestamp.arrival) %>% 
  filter(timestamp.departure == max(timestamp.departure)) %>% 
  group_by(individual.local.identifier, timestamp.departure) %>%
  filter(timestamp.arrival == min(timestamp.arrival)) %>%
  ungroup()


# Keep one instance of each duplicate
stopovers_filtered2 <- stopovers_filtered %>%
  distinct(individual.local.identifier, timestamp.arrival, timestamp.departure, .keep_all = TRUE) 



write.csv(stopovers_filtered2, "filtered_stopovers.csv", row.names = FALSE)



# ------------------------------------------------------------------------------
# Step 3: Remove Points Overlapping with Stopovers
# ------------------------------------------------------------------------------

rm(list = ls())


# Load data
tracking_data <- read.csv("Final_Aggregated_Data.csv")
tracking_data$timestamp <- as.POSIXct(tracking_data$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

stopover_data <- read.csv("stopover_results.csv")
stopover_data$timestamp.arrival <- as.POSIXct(stopover_data$timestamp.arrival, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
stopover_data$timestamp.departure <- as.POSIXct(stopover_data$timestamp.departure, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")


  
# Create a new column to mark points for removal (1 = keep, 0 = remove)
tracking_data$to_remove <- 1
  
# Loop through each stop and flag data points in the dataset that fall within the stopover period
for (i in 1:nrow(stopover_data)) {
  tracking_data[which(tracking_data$individual.local.identifier == stopover_data[i, "individual.local.identifier"] & 
                   tracking_data$timestamp >= stopover_data[i, "timestamp.arrival"] & 
                   tracking_data$timestamp <= stopover_data[i, "timestamp.departure"]), "to_remove"] <- 0
  }
  
  
# Remove the points that overlap with stopovers, nests, and moults
cleaned_data <- tracking_data[tracking_data$to_remove == 1, ]

# Save the cleaned dataset
write.csv(cleaned_data, "cleaned_data_no_stops.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# Step 4:Filter Based on Speed
# ------------------------------------------------------------------------------

rm(list = ls())  


# Step 1: Calculate movement parameters

dataMove <- move("cleaned_data_no_stops.csv") 

dataMove$CalSpeed <- unlist(lapply(speed(dataMove),c, NA ))
dataMove$CalTurnAngle <-unlist(lapply(turnAngleGc(dataMove), function(x) c(NA, x, NA)))
dataMove$CalHeading <- unlist(lapply(angle(dataMove), c, NA))

dataMoveDF <- as.data.frame(dataMove)


# Step 2: Remove points where ground speed is lower than 36 km/h (10 m/s)
filtered_data_1 <- dataMoveDF %>%
filter(ground.speed > 9.9999 & !is.na(ground.speed) )

# Step 4: Filtering based on Calspeed being above 6 km/h
filtered_data_2 <- filtered_data_1 %>%
filter(CalSpeed > 1.66667 & !is.na(CalSpeed))  

  
# Save the cleaned dataset after low-speed filtering
write.csv(filtered_data_2, "cleaned_data_no_stops_no_lowspeed.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# End of Script
# ---------------------------------------------------------------------------
