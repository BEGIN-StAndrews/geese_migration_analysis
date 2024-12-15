# ------------------------------------------------------------------------------
# Analysis of Movement Parameters and Deviations for Migratory Geese Clusters
# ------------------------------------------------------------------------------


# Description:
# This script analyzes movement parameters and navigational deviations for 
# clusters identified in the migratory tracking data of greater white-fronted 
# geese (Anser albifrons). It focuses on:
#   - Distribution of movement speed (V) and turning angle (θ) across clusters
#   - Deviation of geese headings from the great-circle route to destinations (σD)
#   - Deviation of geese headings from the great-circle route to stopovers (σS)
#
# Clusters are labeled based on dominant behavioral and navigational features.
# The visualizations highlight how clustering reflects variations in movement 
# behaviors and navigation strategies.
#
# Input:
#   - Clustered tracking data for autumn daytime: 'AutumnDay_Clustered.csv'
#   - Clustered tracking data for autumn nighttime: 'AutumnNight_Clustered.csv'
#
# Output:
#   - Boxplots for movement parameters:
#       * 'AD-MP.png' (daytime)
#       * 'AN-MP.png' (nighttime)
#   - Boxplots for navigational deviations:
#       * 'AD-Deviation.png' (daytime)
#       * 'AN-Deviation.png' (nighttime)
# ------------------------------------------------------------------------------

rm(list = ls())

# Load Necessary Libraries
library(dplyr)    
library(ggplot2)  
library(tidyr)    
library(stringr)  

# ------------------------------------------------------------------------------
#  
#       Part 1: Movement Parameters Analysis for Autumn Daytime
#
# ------------------------------------------------------------------------------

# Load the data
df <- read.csv("AutumnDay_Clustered.csv")


# Define cluster labels
cluster_labels <- c(
  "High ΔI' & Δψm(1)", 
  "High ΔI' & Δψm(2)", 
  "High ΔI(1)",
  "High ΔI(2)", 
  "Inland(1)", 
  "Inland(2)", 
  "Low ΔI' & Δψm,\nTailwinds",
  "Low Crosswinds",
  "High Crosswinds",
  "Headwinds(1)",
  "Headwinds(2)"
)



# Reshape data for plotting
variables_to_plot <- c("CalSpeed", "CalTurnAngle")
df_long <- df %>%
  pivot_longer(cols = variables_to_plot,
               names_to = "variable_name",
               values_to = "value")
df_long <- df_long %>%
  mutate(cluster_label = factor(cluster_id, levels = 1:11, labels = cluster_labels))


# Define colors for boxplots
outline_colors <- c("#1F78B4", "#E31A1C")
fill_colors <- c("#A6CEE3", "#FB9A99")


# Plot movement parameters
plot <- ggplot(df_long, aes(x = cluster_label, y = value, fill = variable_name)) +
  geom_boxplot(aes(color = variable_name),
               width = 0.4, 
               position = position_dodge(width = 0.6),  
               outlier.shape = 1, outlier.size = 0.1, outlier.alpha = 0.9,
               size = 0.4 ) +
  labs(
    x = "Cluster Labels",
    y = "Value"
  ) +
  theme_minimal(base_family = "Arial") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), expand = expansion(add = c(0.2, 0.2))) +
  scale_color_manual(values = outline_colors,
                     name = NULL,
                     labels = expression(paste("V (m/s)"), paste(theta, "°"))) +  
  scale_fill_manual(values = fill_colors,
                    name = NULL,
                    labels = expression(paste("V (m/s)"), paste(theta, "°"))) +  
  theme(
    axis.title.x = element_text(size = 9, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8, margin = margin(r = 5)),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust = 0.7),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(2, "cm"),  
    legend.spacing.y = unit(0, "cm"),  
    legend.position = "top",  
    legend.box.margin = margin(t = -10, b = 0, l = 0, r = 0), 
    legend.justification = "center") +
  coord_cartesian(ylim = c(0, 87))


# Save plot
ggsave( filename = "AD-MP.png", plot = plot, width = 16, height = 18,units = "cm",dpi = 600)



# ------------------------------------------------------------------------------
#  
#       Part 2: Deviation Analysis for Autumn Daytime
#
# ------------------------------------------------------------------------------


# Define variables to plot deviations
variables_to_plot <- c("Deviation", "deviation_from_stop")
df_long <- df %>%
  pivot_longer(cols = variables_to_plot,
               names_to = "variable_name",
               values_to = "value")
df_long <- df_long %>%
  mutate(cluster_label = factor(cluster_id, levels = 1:11, labels = cluster_labels))


# Plot deviations
plot <- ggplot(df_long, aes(x = cluster_label, y = value, fill = variable_name)) +
  geom_boxplot(aes(color = variable_name),
               width = 0.4, 
               position = position_dodge(width = 0.6),
               outlier.shape = 1, outlier.size = 0.1, outlier.alpha = 0.9,
               size = 0.4 ) +
  labs(
    x = "Cluster Labels",
    y = expression(paste("Deviation (", degree, ")"))  
  ) +
  theme_minimal(base_family = "Arial") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), expand = expansion(add = c(0.3, 0.3))) +
  scale_color_manual(values = outline_colors,
                     name = NULL,
                     labels = expression(paste(sigma[D], degree), paste(sigma[S], degree))) + 
  scale_fill_manual(values = fill_colors,
                    name = NULL,
                    labels = expression(paste(sigma[D], degree), paste(sigma[S], degree))) + 
  theme(
    axis.title.x = element_text(size = 9, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8, margin = margin(r = 5)),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust = 0.7),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(5, "cm"),
    legend.spacing.y = unit(0, "cm"),  
    legend.box.margin = margin(t = -10, b = 0, l = 0, r = 0),  
    legend.position = "top",  
    legend.justification = "center") +
  coord_cartesian(ylim = c(0, 122))


# Save plot
ggsave( filename = "AD-Deviation.png",  plot = plot,width = 16, height = 18, units = "cm", dpi = 600)



#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# ------------------------------------------------------------------------------
#  
#       Part 3: Movement Parameters Analysis for Autumn Nighttime
#
# ------------------------------------------------------------------------------

rm(list = ls())

# Load the data
df <- read.csv("AutumnNight_Clustered.csv")


# Define cluster labels
cluster_labels <- c(
  "High ΔI' & Δψm(1)", 
  "High ΔI' & Δψm(2)",
  "Inland",
  "High ΔI", 
  "Low ΔI', Tailwinds",
  "Tailwinds", 
  "Low ΔI, Headwinds",
  "High Crosswinds",
  "High ΔI' & Δψm(3)", 
  "Normal",
  "Headwinds"
)



# Reshape data for plotting
variables_to_plot <- c("CalSpeed", "CalTurnAngle")
df_long <- df %>%
  pivot_longer(cols = variables_to_plot,
               names_to = "variable_name",
               values_to = "value")
df_long <- df_long %>%
  mutate(cluster_label = factor(cluster_id, levels = 1:11, labels = cluster_labels))


# Define colors for boxplots
outline_colors <- c("#1F78B4", "#E31A1C")
fill_colors <- c("#A6CEE3", "#FB9A99")


# Plot movement parameters
plot <- ggplot(df_long, aes(x = cluster_label, y = value, fill = variable_name)) +
  geom_boxplot(aes(color = variable_name),
               width = 0.4,  
               position = position_dodge(width = 0.6),  
               outlier.shape = 1, outlier.size = 0.1, outlier.alpha = 0.9,
               size = 0.4 ) +
  labs(
    x = "Cluster Labels",
    y = "Value"
  ) +
  theme_minimal(base_family = "Arial") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), expand = expansion(add = c(0.2, 0.2))) +
  scale_color_manual(values = outline_colors,
                     name = NULL,
                     labels = expression(paste("V (m/s)"), paste(theta, "°"))) +  
  scale_fill_manual(values = fill_colors,
                    name = NULL,
                    labels = expression(paste("V (m/s)"), paste(theta, "°"))) +  
  theme(
    axis.title.x = element_text(size = 9, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8, margin = margin(r = 5)),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust = 0.7),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(2, "cm"),  
    legend.spacing.y = unit(0, "cm"),  
    legend.position = "top",  
    legend.box.margin = margin(t = -10, b = 0, l = 0, r = 0),  
    legend.justification = "center") +
  coord_cartesian(ylim = c(0, 60))


# Save plot
ggsave(filename = "AN-MP.png", plot = plot, width = 16, height = 18, units = "cm", dpi = 600)



# ------------------------------------------------------------------------------
#  
#       Part 4: Deviation Analysis for Autumn Nighttime
#
# ------------------------------------------------------------------------------


# Define variables to plot deviations
variables_to_plot <- c("Deviation", "deviation_from_stop")
df_long <- df %>%
  pivot_longer(cols = variables_to_plot,
               names_to = "variable_name",
               values_to = "value")
df_long <- df_long %>%
  mutate(cluster_label = factor(cluster_id, levels = 1:11, labels = cluster_labels))



# Plot deviations
plot <- ggplot(df_long, aes(x = cluster_label, y = value, fill = variable_name)) +
  geom_boxplot(aes(color = variable_name),
               width = 0.4,  
               position = position_dodge(width = 0.6),
               outlier.shape = 1, outlier.size = 0.1, outlier.alpha = 0.9,
               size = 0.4 ) +
  labs(
    x = "Cluster Labels",
    y = expression(paste("Deviation (", degree, ")"))  
  ) +
  theme_minimal(base_family = "Arial") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), expand = expansion(add = c(0.3, 0.3))) +
  scale_color_manual(values = outline_colors,
                     name = NULL,
                     labels = expression(paste(sigma[D], degree), paste(sigma[S], degree))) + 
  scale_fill_manual(values = fill_colors,
                    name = NULL,
                    labels = expression(paste(sigma[D], degree), paste(sigma[S], degree))) + 
  theme(
    axis.title.x = element_text(size = 9, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8, margin = margin(r = 5)),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust = 0.7),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.x = unit(5, "cm"),
    legend.spacing.y = unit(0, "cm"),  
    legend.box.margin = margin(t = -10, b = 0, l = 0, r = 0),  
    legend.position = "top",  
    legend.justification = "center" ) +
  coord_cartesian(ylim = c(0, 115))


# Save plot
ggsave(filename = "AN-Deviation.png", plot = plot,width = 16, height = 18,units = "cm",dpi = 600)


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------


