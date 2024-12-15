# ------------------------------------------------------------------------------
# Focused Density Plots for Navigational Cues in Spring Daytime Clusters
# ------------------------------------------------------------------------------

# Description:
# This script generates focused density plots for key navigational cues across 
# spring daytime clusters (Fig. 9). Each cluster is highlighted based on its 
# dominant features, illustrating the distribution of:
#   - Changes in apparent inclination (ΔI')
#   - Changes in geomagnetic inclination (ΔI)
#   - Changes in geomagnetic heading (Δψm)
#   - Wind support (WS)
#   - Crosswind (CW)
#   - Distance to the coast (DtC)
#
# Cluster-specific distributions are highlighted in red or blue, with grey 
# curves showing the overall dataset. Each cluster is labeled with dominant 
# features and annotations.
#
# Input:
#   - Clustered spring daytime dataset: 'SpringDay_Clustered.csv'
#
# Output:
#   - Focused density plot for all clusters: 'SD_FocusedDensity.png'

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Clear Environment and Load Libraries
# ------------------------------------------------------------------------------
rm(list = ls())
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# ------------------------------------------------------------------------------
# Define Utility Functions
# ------------------------------------------------------------------------------
# Function to create density plots with optional x-axis labels and spacing adjustment
create_density_plot <- function(data, cluster_var, focus_cluster_id, variable, highlight = FALSE, show_x_label = FALSE) {
  data %>%
    mutate(is_focus = ifelse(.data[[cluster_var]] == focus_cluster_id, "Focus Cluster", "Others")) %>%
    ggplot(aes_string(x = variable, fill = "is_focus")) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = if (highlight) {
      c("Focus Cluster" = "red", "Others" = "grey")
    } else {
      c("Focus Cluster" = "blue", "Others" = "grey")
    }) +
    labs(title = NULL, x = if (show_x_label) x_labels[[variable]] else NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      text = element_text(size = 20),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "lines"),
      axis.text.x = if (show_x_label) element_text(size = 28) else element_blank(),
      axis.title.x = if (show_x_label) element_text(size = 42,
                                                    margin = margin(t = 50)) else element_blank(),
      axis.ticks.x = if (show_x_label) element_line() else element_blank()
    )
}

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------
spring_day <- read.csv("SpringDay_Clustered.csv")

# ------------------------------------------------------------------------------
# Define Plot Labels and Variables
# ------------------------------------------------------------------------------
# X-axis labels for variables
x_labels <- list(
  "delta_apparent_dip" = "ΔI' (°)",
  "delta_inclination" = "ΔI (°)",
  "delta_MagHeading2" = "Δψm (°)",
  "wind_support" = "WS (m/s)",
  "crosswind" = "CW (m/s)",
  "Distance.to.Coast" = "DtC (km)"
)

# Updated cluster labels
cluster_labels_custom <- c(
  "SD1:High ΔI' & Δψm", 
  "SD2: High ΔI", 
  "SD3: Inland",
  "SD4: High Crosswinds", 
  "SD5:Low ΔI' & Δψm\nTailwinds", 
  "SD6: Headwinds"
)

# Features to plot and cluster-specific highlights
features <- c("delta_apparent_dip", "delta_inclination", "delta_MagHeading2", 
              "wind_support", "crosswind", "Distance.to.Coast")

highlight_features <- list(
  `1` = c("delta_apparent_dip", "delta_MagHeading2"),
  `2` = c("delta_inclination"),
  `3` = c("Distance.to.Coast"),
  `4` = c("crosswind"),
  `5` = c("delta_apparent_dip", "delta_MagHeading2", "wind_support"),
  `6` = c("wind_support")
)

# ------------------------------------------------------------------------------
# Generate Density Plots for Each Cluster
# ------------------------------------------------------------------------------
density_plots <- list()
for (i in 1:6) {
  focus_cluster_id <- i  # Use numeric cluster IDs
  plots <- lapply(features, function(feature) {
    highlight <- feature %in% highlight_features[[as.character(focus_cluster_id)]]
    show_x_label <- (i == 6)  # Show x-axis labels only for the bottom row
    create_density_plot(spring_day, "cluster_id", focus_cluster_id, feature, highlight, show_x_label)
  })
  density_plots[[i]] <- plots
}

# ------------------------------------------------------------------------------
# Arrange Plots and Add Labels
# ------------------------------------------------------------------------------
# Create cluster labels with `textGrob`
cluster_labels <- lapply(cluster_labels_custom, function(label) {
  textGrob(label, rot = 90, hjust = 0.5, vjust = 0.5, gp = gpar(fontsize = 42))
})

# Arrange cluster labels and density plots
left_labels <- arrangeGrob(grobs = cluster_labels, ncol = 1)
right_plots <- list()
for (i in 1:6) {
  right_plots[[i]] <- do.call(grid.arrange, c(density_plots[[i]], ncol = length(features)))
}

# Create a density label at the far left
density_label <- textGrob("Density", rot = 90, hjust = 0.3, x = 0.8, gp = gpar(fontsize = 28))

# Combine the left labels, density label, and right plots
final_plots <- list()
for (i in 1:6) {
  final_plots[[i]] <- arrangeGrob(
    left_labels[i], density_label, right_plots[[i]], 
    ncol = 3, widths = c(0.4, 0.2, 12)
  )
}

# ------------------------------------------------------------------------------
# Combine and Save Final Plot
# ------------------------------------------------------------------------------
# Combine all clusters into a single grid
spring_day_plots <- do.call(grid.arrange, c(final_plots, ncol = 1))

# Save and display the final plot
ggsave("SD_FocusedDensity.png", spring_day_plots, width = 40, height = 40, dpi = 300)
print(spring_day_plots)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
