
# ------------------------------------------------------------------------------
# Circular Histograms of Geese Headings Across Clusters
# ------------------------------------------------------------------------------

# Description:
# This script generates circular histograms to visualize the distribution of 
# headings (in degrees) for greater white-fronted geese across clusters. 
# It visualizes directional preferences for each cluster,
# revealing differences in migratory behaviour.
#
# Each cluster is labeled based on its dominant characteristics, and headings 
# are visualized on a compass-like circular axis (N, E, S, W).
#
# Input:
#   - Clustered datasets for autumn daytime and nighttime migrations:
#       * 'SpringDay_Clustered.csv'
#       * 'SpringNight_Clustered.csv'
#
# Output:
#   - Circular histograms visualized for each cluster (day and night).
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# 
#                   Spring Daytime Migrations
#
# ------------------------------------------------------------------------------
rm(list = ls())

# Load Necessary Libraries
library(dplyr)
library(ggplot2)
library(circular)

# ------------------------------------------------------------------------------
# Step 1: Load and Preprocess Data
# ------------------------------------------------------------------------------

# Load clustered data
df <- read.csv("SpringDay_Clustered.csv")

# Define cluster labels
cluster_labels <- c(
  "SD1:High ΔI' & Δψm", 
  "SD2: High ΔI", 
  "SD3: Inland",
  "SD4: High Crosswinds", 
  "SD5:Low ΔI' & Δψm\nTailwinds", 
  "SD6: Headwinds"
)


# Convert heading to radians and circular data
df <- df %>%
  mutate(
    heading_radians = CalHeading * pi / 180,
    heading_circular = circular(heading_radians, units = "radians", template = "geographics")
  )



# ------------------------------------------------------------------------------
# Step 2: Bin and Normalize Heading Data
# ------------------------------------------------------------------------------

df_freq <- df %>%
  group_by(cluster_id) %>%
  mutate(heading_bin = cut(CalHeading, breaks = seq(0, 360, by = 10), include.lowest = TRUE, labels = FALSE), 
         heading_bin_midpoint = (as.numeric(heading_bin) - 0.5) * 10) %>%
  group_by(cluster_id, heading_bin_midpoint) %>%
  summarize(n = n()) %>%          
  mutate(frequency = n / sum(n)) %>% # Normalize frequencies within cluster
  ungroup() %>%
  mutate(heading_circular_bin = circular(heading_bin_midpoint * pi / 180, units = "radians", template = "geographics"))  # Convert midpoints to circular (radians)



# ------------------------------------------------------------------------------
# Step 3: Plot Circular Histograms
# ------------------------------------------------------------------------------

ggplot(df_freq, aes(x = heading_circular_bin, y = frequency)) +
  geom_bar(stat = "identity", fill = "#32CD32", color = "grey30", alpha = 0.8, width = 2*pi/36) + 
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = c(0, pi/2, pi, 3*pi/2),
    labels = c("N", "E", "S", "W"),
    limits = c(0, 2 * pi)
  ) +
  facet_wrap(~factor(cluster_id, labels = cluster_labels), ncol = 3) +
  labs(x = NULL, y = NULL, title = NULL) +  # Add main title
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_line(color = "grey90"),
    strip.text = element_text(size = 15),
    legend.position = "none",
  )



# Save the plot
ggsave("SpringDay_CircularHistogram.png", width = 12, height = 9, dpi = 300)


# ------------------------------------------------------------------------------
# 
#                    Spring Nighttime Migrations
#
# ------------------------------------------------------------------------------

rm(list = ls())


# ------------------------------------------------------------------------------
# Step 1: Load and Preprocess Data
# ------------------------------------------------------------------------------

# Load clustered data
df <- read.csv("SpringNight_Clustered.csv")


# Define cluster labels
cluster_labels <- c(
  "High ΔI' & Δψm(1)", 
  "High ΔI' & Δψm(2)",
  "Headwinds,High Crosswinds",
  "Inland(1)", 
  "Inland(2)",
  "High ΔI", 
  "Normal",
  "Low ΔI' & Δψm",
  "Low ΔI' & Δψm,Tailwinds"
)



# Convert heading to radians and circular data
df <- df %>%
  mutate(
    heading_radians = CalHeading * pi / 180,
    heading_circular = circular(heading_radians, units = "radians", template = "geographics")
  )



# ------------------------------------------------------------------------------
# Step 2: Bin and Normalize Heading Data
# ------------------------------------------------------------------------------

df_freq <- df %>%
  group_by(cluster_id) %>%
  mutate(heading_bin = cut(CalHeading, breaks = seq(0, 360, by = 10), include.lowest = TRUE, labels = FALSE), 
         heading_bin_midpoint = (as.numeric(heading_bin) - 0.5) * 10) %>%
  group_by(cluster_id, heading_bin_midpoint) %>%
  summarize(n = n()) %>%          
  mutate(frequency = n / sum(n)) %>% # Normalize frequencies within cluster
  ungroup() %>%
  mutate(heading_circular_bin = circular(heading_bin_midpoint * pi / 180, units = "radians", template = "geographics"))  # Convert midpoints to circular (radians)



# ------------------------------------------------------------------------------
# Step 3: Plot Circular Histograms
# ------------------------------------------------------------------------------

ggplot(df_freq, aes(x = heading_circular_bin, y = frequency)) +
  geom_bar(stat = "identity", fill = "#32CD32", color = "grey30", alpha = 0.8, width = 2*pi/36) + 
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = c(0, pi/2, pi, 3*pi/2),
    labels = c("N", "E", "S", "W"),
    limits = c(0, 2 * pi)
  ) +
  facet_wrap(~factor(cluster_id, labels = cluster_labels), ncol = 3) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_line(color = "grey90"),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )



# Save the plot
ggsave("SpringNight_CircularHistogram.png", width = 12, height = 13.5, dpi = 300)


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------

