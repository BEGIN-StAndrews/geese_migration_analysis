# ------------------------------------------------------------------------------
# Visualizing Density Plots for Navigational Cues Across Clusters
# ------------------------------------------------------------------------------


# Description:
# This script generates density plots to visualize the distributions of key
# navigational cues across clusters for migratory geese tracking data. Cues
# include changes in apparent inclination (ΔI'), geomagnetic inclination (ΔI),
# geomagnetic heading (Δψm), distance to the coast (DtC), wind support (WS), and
# crosswind (CW). 
#
# Each cluster is labeled based on the dominant features of its distributions.
# Plots include:
#   - Grey density curves for overall cue distributions (excluding the focal cluster)
#   - Blue and red curves for cue distributions within the focal cluster, with:
#       * Blue indicating alignment with overall data
#       * Red highlighting distinct, cluster-specific patterns
#   - Median cue values marked with vertical lines and annotated numerically.
#
# Input:
#   - Clustered day and night datasets with clustering IDs: 
#       * 'AutumnDay_Clustered.csv'
#       * 'AutumnNight_Clustered.csv'
#
# Output:
#   - Combined density plots for day and night clusters, with cluster labels.

# ------------------------------------------------------------------------------

rm(list = ls())

# Load Necessary Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(lubridate)

# ------------------------------------------------------------------------------
#  
#                   Part 1: Autumn daytime migrations
#
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Step 1: Load and Reshape Data
# ------------------------------------------------------------------------------
# Load clustered day dataset (11 clusters identified)
df <- read.csv("AutumnDay_Clustered.csv") 


# Reshape data to long format for ggplot
data_long <- df %>%
  select(cluster_id, delta_apparent_dip, delta_inclination, delta_MagHeading2, wind_support, crosswind, Distance.to.Coast) %>%
  gather(key = "Variable", value = "Value", -cluster_id)

# Rename variables for clarity and to match paper notation
data_long$Variable <- recode(data_long$Variable,
                             'delta_apparent_dip' = "ΔI' (°)",
                             'delta_inclination' = "ΔI (°)",
                             'delta_MagHeading2' = "Δψm (°)",
                             'wind_support' = "WS (m/s)",
                             'crosswind' = "CW (m/s)",
                             'Distance.to.Coast' = "DtC (km)")

# Ensure the 'Value' column is numeric
data_long$Value <- as.numeric(as.character(data_long$Value))


# ------------------------------------------------------------------------------
# Step 2: Define Plotting Function
# ------------------------------------------------------------------------------

# Create a function to generate density plots for each variable
plot_density <- function(data, variable_name, x_min, x_max) {
  data_filtered <- data %>%
    filter(Variable == variable_name)
  
  # Calculate median values for each cluster
  median_values <- data_filtered %>%
    group_by(cluster_id) %>%
    summarize(median_val = median(Value, na.rm = TRUE))
  
  # Determine decimal places based on variable name
  decimal_places <- switch(variable_name,
                           "ΔI' (°)" = 2,
                           "ΔI (°)" = 3,
                           "Δψm (°)" = 1,
                           "WS (m/s)" = 1,
                           "CW (m/s)" = 1,
                           "DtC (km)" = 0)
  
  # Generate density plot
  p <- ggplot(data_filtered, aes(x = Value)) +
    geom_density(fill = "gray80", color = "gray60", alpha = 0.7) +
    geom_vline(data = median_values,
               aes(xintercept = median_val), color = "blue", linetype = "solid", size = 0.5) +
    geom_text(data = median_values,
              aes(x = median_val + 0.01 * (x_max - x_min), y = Inf, 
                  label = format(round(median_val, decimal_places), nsmall = decimal_places)),
              color = "blue", size = 4.5, hjust = -0.1, vjust = 2) + 
    facet_wrap(~ cluster_id, ncol = 1, scales = "fixed") +
    labs(x = NULL, y = NULL) + 
    xlim(x_min, x_max) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_blank(), 
      plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "lines"), 
      panel.spacing = unit(0.45, "lines"), 
      panel.background = element_blank(), 
      plot.background = element_blank(), 
      panel.grid = element_blank(), 
      axis.text.x = element_text(size = 13),
      plot.title = element_text(hjust = 0.5, size = 18),
      panel.border = element_rect(color = "gray80", fill = NA, linetype = "dashed")
    )
}

# ------------------------------------------------------------------------------
# Step 3: Define Axis Limits for Variables
# ------------------------------------------------------------------------------
# Manual x-axis limits for each variable
manual_limits <- list(
  "ΔI' (°)" = c(0, 13),
  "ΔI (°)" = c(0, 0.7),
  "Δψm (°)" = c(0, 112),
  "WS (m/s)" = c(-11, 13),
  "CW (m/s)" = c(0, 13),
  "DtC (km)" = c(-900, 150)
)



# ------------------------------------------------------------------------------
# Step 4: Generate and Annotate Plots
# ------------------------------------------------------------------------------

# Generate individual density plots for each variable
plots <- lapply(unique(data_long$Variable), function(var) {
  limits <- manual_limits[[var]]
  p <- plot_density(data_long, var, limits[1], limits[2])
  p + ggtitle(var)
})


# ------------------------------------------------------------------------------
# Step 5: Customize Cluster Labels
# ------------------------------------------------------------------------------

# Define cluster labels based on key navigational cues and dominant features
# These labels describe distinct behaviours observed in each cluster
cluster_labels_custom <- c(
  "AD1:\nHigh ΔI' & Δψm(1)\n(2%)", 
  "AD2:\nHigh ΔI' & Δψm(2)\n(2%)", 
  
  "AD3:\nHigh ΔI(1)\n(1%)",
  "AD4:\nHigh ΔI(2)\n(6%)", 
  
  "AD5:\nInland(1)\n(2%)", 
  "AD6:\nInland(2)\n(18%)", 
  
  "AD7: Low ΔI' & Δψm,\nTailwinds\n(10%)",
  "AD8:\nLow Crosswinds\n(17%)",
  
  "AD9:\nHigh Crosswinds\n(12%)",
  
  
  "AD10:\nHeadwinds(1)\n(10%)",
  "AD11:\nHeadwinds(2)\n(20%)"
  
  )

# Convert labels to text grobs for vertical arrangement
cluster_labels <- lapply(cluster_labels_custom, function(label) {
  textGrob(label, rot = 90, vjust = 0.5, gp = gpar(fontsize = 16), y = unit(0.5, "npc"))
})



# ------------------------------------------------------------------------------
# Step 6: Arrange and Display Final Plot
# ------------------------------------------------------------------------------

# Combine cluster labels and density plots
left_column <- arrangeGrob(grobs = cluster_labels, ncol = 1)
right_column <- do.call(grid.arrange, c(plots, ncol = length(plots)))

# Arrange the final plot with labeled clusters
final_plot <- grid.arrange(left_column, right_column, ncol = 2, widths = c(0.35, 5))

# Display the plot
print(final_plot)  # 1250* 2200


# ------------------------------------------------------------------------------
#         
#                     Part 2: Autumn nighttime migrations
#
# ------------------------------------------------------------------------------

rm(list = ls())


# ------------------------------------------------------------------------------
# Step 1: Load and Reshape Data
# ------------------------------------------------------------------------------
# Load clustered day dataset (11 clusters identified)
df <- read.csv("AutumnNight_Clustered.csv")

# Reshape data to long format for ggplot
data_long <- df %>%
  select(cluster_id, delta_apparent_dip, delta_inclination, delta_MagHeading2, wind_support, crosswind, Distance.to.Coast) %>%
  gather(key = "Variable", value = "Value", -cluster_id)

# Rename variables for clarity and to match paper notation
data_long$Variable <- recode(data_long$Variable,
                             'delta_apparent_dip' = "ΔI' (°)",
                             'delta_inclination' = "ΔI (°)",
                             'delta_MagHeading2' = "Δψm (°)",
                             'wind_support' = "WS (m/s)",
                             'crosswind' = "CW (m/s)",
                             'Distance.to.Coast' = "DtC (km)")

# Ensure the 'Value' column is numeric
data_long$Value <- as.numeric(as.character(data_long$Value))


# ------------------------------------------------------------------------------
# Step 2: Define Plotting Function
# ------------------------------------------------------------------------------

# Create a function to generate density plots for each variable
plot_density <- function(data, variable_name, x_min, x_max) {
  data_filtered <- data %>%
    filter(Variable == variable_name)
  
  # Calculate median values for each cluster
  median_values <- data_filtered %>%
    group_by(cluster_id) %>%
    summarize(median_val = median(Value, na.rm = TRUE))
  
  # Determine decimal places based on variable name
  decimal_places <- switch(variable_name,
                           "ΔI' (°)" = 2,
                           "ΔI (°)" = 3,
                           "Δψm (°)" = 1,
                           "WS (m/s)" = 1,
                           "CW (m/s)" = 1,
                           "DtC (km)" = 0)
  
  # Generate density plot
  p <- ggplot(data_filtered, aes(x = Value)) +
    geom_density(fill = "gray80", color = "gray60", alpha = 0.7) +
    geom_vline(data = median_values,
               aes(xintercept = median_val), color = "blue", linetype = "solid", size = 0.5) +
    geom_text(data = median_values,
              aes(x = median_val + 0.01 * (x_max - x_min), y = Inf, 
                  label = format(round(median_val, decimal_places), nsmall = decimal_places)),
              color = "blue", size = 4.5, hjust = -0.1, vjust = 2) + 
    facet_wrap(~ cluster_id, ncol = 1, scales = "fixed") +
    labs(x = NULL, y = NULL) + 
    xlim(x_min, x_max) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_blank(), 
      plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "lines"), 
      panel.spacing = unit(0.45, "lines"), 
      panel.background = element_blank(), 
      plot.background = element_blank(), 
      panel.grid = element_blank(), 
      axis.text.x = element_text(size = 13),
      plot.title = element_text(hjust = 0.5, size = 18),
      panel.border = element_rect(color = "gray80", fill = NA, linetype = "dashed")
    )
}



# ------------------------------------------------------------------------------
# Step 3: Define Axis Limits for Variables
# ------------------------------------------------------------------------------

# Manual x-axis limits for each variable
manual_limits <- list(
  "ΔI' (°)" = c(0, 9),
  "ΔI (°)" = c(0, 0.6),
  "Δψm (°)" = c(0, 80),
  "WS (m/s)" = c(-11, 13),
  "CW (m/s)" = c(0, 11),
  "DtC (km)" = c(-900, 150)
)


# ------------------------------------------------------------------------------
# Step 4: Generate and Annotate Plots
# ------------------------------------------------------------------------------

# Generate individual density plots for each variable
plots <- lapply(unique(data_long$Variable), function(var) {
  limits <- manual_limits[[var]]
  p <- plot_density(data_long, var, limits[1], limits[2])
  p + ggtitle(var)
})


# ------------------------------------------------------------------------------
# Step 5: Customize Cluster Labels
# ------------------------------------------------------------------------------

# Define cluster labels based on key navigational cues and dominant features
# These labels describe distinct behaviours observed in each cluster
cluster_labels_custom <- c(
  "AN1:\nHigh ΔI' & Δψm(1)\n(1%)", 
  "AN2:\nHigh ΔI' & Δψm(2)\n(4%)",
  "AN3:\nInland\n(13%)",
  "AN4:\nHigh ΔI\n(4%)", 
  "AN5:\nLow ΔI', Tailwinds\n(15%)",
  "AN6:\nTailwinds\n(15%)", 
  "AN7:\nLow ΔI, Headwinds\n(5%)",
  "AN8:\nHigh Crosswinds\n(10%)",
  "AN9:\nHigh ΔI' & Δψm(3)\n(13%)", 
  "AN10:\nNormal\n(10%)",
  "AN11:\nHeadwinds\n(10%)"
)


# Convert labels to text grobs for vertical arrangement
cluster_labels <- lapply(cluster_labels_custom, function(label) {
  textGrob(label, rot = 90, vjust = 0.5, gp = gpar(fontsize = 16), y = unit(0.5, "npc"))
})



# ------------------------------------------------------------------------------
# Step 6: Arrange and Display Final Plot
# ------------------------------------------------------------------------------

# Combine cluster labels and density plots
left_column <- arrangeGrob(grobs = cluster_labels, ncol = 1)
right_column <- do.call(grid.arrange, c(plots, ncol = length(plots)))

# Arrange the final plot with labeled clusters
final_plot <- grid.arrange(left_column, right_column, ncol = 2, widths = c(0.39, 5))

# Display the plot
print(final_plot)  # 1250* 2200


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
