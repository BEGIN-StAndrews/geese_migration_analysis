# ------------------------------------------------------------------------------
# Focused Analysis of Migratory Navigation Deviations Across Clusters
# ------------------------------------------------------------------------------

# Description:
# This script performs a detailed statistical analysis of deviations for migratory bird tracking data,
# focusing on deviations from destinations and stopovers for different migratory periods.
# It uses Kruskal-Wallis tests and Dunn's post-hoc tests to identify significant differences between clusters.

# Input:
#   - Clustered datasets for spring and autumn migrations:
#       * 'SpringDay_Clustered.csv', 'SpringNight_Clustered.csv'
#
# Output:
#   - Detailed results for each analysis, including:
#       * Median deviations for each cluster
#       * Kruskal-Wallis test results
#       * Dunn's test comparisons highlighting clusters with similar deviations

# ------------------------------------------------------------------------------

# Load necessary libraries
if (!requireNamespace("FSA", quietly = TRUE)) install.packages("FSA")
library(dplyr)
library(FSA)  # For Dunn's test

# ------------------------------------------------------------------------------
# Define Helper Function for Analysis
# ------------------------------------------------------------------------------
perform_focused_analysis <- function(data_file, cluster_labels, target_cluster, deviation_col, period, day_night) {
  # Load data
  df <- read.csv(data_file)
  
  # Assign cluster labels
  df$cluster_label <- factor(df$cluster_id, levels = 1:length(cluster_labels), labels = cluster_labels)
  
  # Calculate median deviations for each cluster
  median_devs <- df %>%
    group_by(cluster_label) %>%
    summarize(median_deviation = median(.data[[deviation_col]], na.rm = TRUE), .groups = "drop")
  
  # Print detailed median deviations
  cat(sprintf("\n--- Median Deviation from %s during %s-%s migrations ---\n",
              ifelse(deviation_col == "Deviation", "destinations", "stopovers"), period, day_night))
  print(median_devs)
  
  # Identify the cluster with the lowest median deviation
  target_median <- median_devs %>% filter(cluster_label == target_cluster) %>% pull(median_deviation)
  
  # Kruskal-Wallis Test
  cat(sprintf("\n--- Kruskal-Wallis Test for deviation from %s during %s-%s migrations ---\n",
              ifelse(deviation_col == "Deviation", "destinations", "stopovers"), period, day_night))
  kruskal_result <- kruskal.test(reformulate("cluster_label", response = deviation_col), data = df)
  print(kruskal_result)
  
  # Dunn's Test
  cat(sprintf("\n--- Dunn's Test for deviation from %s during %s-%s migrations ---\n",
              ifelse(deviation_col == "Deviation", "destinations", "stopovers"), period, day_night))
  dunn_result <- dunnTest(reformulate("cluster_label", response = deviation_col), data = df, method = "bonferroni")
  dunn_results_df <- as.data.frame(dunn_result$res)
  dunn_results_df$Comparison <- strsplit(as.character(dunn_results_df$Comparison), " - ")
  
  # Filter comparisons involving the target cluster
  relevant_comparisons <- dunn_results_df[sapply(dunn_results_df$Comparison, function(x) target_cluster %in% x), ]
  
  # Identify clusters not significantly different from the target cluster
  not_significant <- relevant_comparisons %>% filter(P.adj >= 0.05)
  
  if (nrow(not_significant) == 0) {
    cat(sprintf("\nThe '%s' cluster showed significantly lower deviation compared to all other clusters.\n",
                target_cluster))
  } else {
    cat(sprintf("\nClusters not significantly different from '%s':\n", target_cluster))
    print(not_significant)
  }
}

# ------------------------------------------------------------------------------
# Define Cluster Labels and Run Analysis
# ------------------------------------------------------------------------------
spring_day_labels <- c(
  "High ΔI' & Δψm", 
  "High ΔI", 
  "Inland",
  "High Crosswinds", 
  "Low ΔI' & Δψm, Tailwinds",
  "Headwinds"
)


spring_night_labels <- c(
  "High ΔI' & Δψm(1)", 
  "High ΔI' & Δψm(2)",
  "Headwinds, High Crosswinds",
  "Inland(1)", 
  "Inland(2)",
  "High ΔI", 
  "Normal",
  "Low ΔI' & Δψm",
  "Low ΔI' & Δψm, Tailwinds"
)




# Run analyses
perform_focused_analysis("SpringDay_Clustered.csv", spring_day_labels, "Low ΔI' & Δψm, Tailwinds", "Deviation", "Spring", "Day")
perform_focused_analysis("SpringDay_Clustered.csv", spring_day_labels, "Low ΔI' & Δψm, Tailwinds", "deviation_from_stop", "Spring", "Day")
perform_focused_analysis("SpringNight_Clustered.csv", spring_night_labels, "Normal", "Deviation", "Spring", "Night")
perform_focused_analysis("SpringNight_Clustered.csv", spring_night_labels, "Low ΔI' & Δψm, Tailwinds", "deviation_from_stop", "Spring", "Night")

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
