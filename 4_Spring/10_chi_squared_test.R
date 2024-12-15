# ------------------------------------------------------------------------------
# Chi-Square Analysis and Proportion Statistics for Clusters
# ------------------------------------------------------------------------------


# Description:
# This script performs a Chi-Square test of independence and calculates median
# and maximum proportions of individuals within clusters for migratory bird data.
#
# Input:
#   - Clustered datasets for migratory periods:
#       * 'SpringDay_Clustered.csv'
#       * 'SpringNight_Clustered.csv'
#
# Output:
#   - Chi-Square test results, including:
#       * Chi-Square statistic
#       * p-value
#   - Median and maximum proportions of individuals within each cluster:
#       * Saved to 'Spring_Proportion_Results.csv'
# ------------------------------------------------------------------------------

rm(list = ls())

# Load Necessary Libraries
library(dplyr)

# ------------------------------------------------------------------------------
# Function Definitions
# ------------------------------------------------------------------------------

# Function to process each period for Chi-Square Test
process_period <- function(file_path, cluster_col, id_col) {
  # Load the data
  df <- read.csv(file_path)
  
  # Create a contingency table
  contingency_table <- table(df[[id_col]], df[[cluster_col]])
  
  # Perform Chi-Square Test
  chi_squared_test <- chisq.test(contingency_table)
  
  # Organize results into a tibble
  results <- tibble(
    Period = file_path,
    Chi_Square_Statistic = chi_squared_test$statistic,
    Chi_Square_p_Value = chi_squared_test$p.value
  )
  
  return(list(results = results, contingency_table = contingency_table, chi_squared_test = chi_squared_test))
}

# Function to calculate median and max proportion for each cluster
find_median_proportion_fixed <- function(data_path, cluster_col, id_col) {
  # Read the data
  df <- read.csv(data_path)
  
  # Group by cluster and individual ID, and calculate counts
  df_grouped <- df %>%
    group_by(!!sym(cluster_col), !!sym(id_col)) %>%
    summarise(count = n(), .groups = "drop")
  
  # Calculate total count and proportions within each cluster
  df_proportions <- df_grouped %>%
    group_by(!!sym(cluster_col)) %>%
    mutate(
      total_in_cluster = sum(count),
      proportion = (count / total_in_cluster) * 100 # Percentage
    ) %>%
    ungroup()
  
  # Calculate median and max proportion for each cluster
  summary_stats <- df_proportions %>%
    group_by(!!sym(cluster_col)) %>%
    summarise(
      median_proportion = median(proportion),
      max_proportion = max(proportion),
      .groups = "drop"
    )
  
  return(summary_stats)
}


# ------------------------------------------------------------------------------
# Define Periods and Perform Analyses
# ------------------------------------------------------------------------------

# Define periods with dataset paths and column names
periods <- list(
  AutumnDay = list(file = "SpringDay_Clustered.csv", cluster_col = "cluster_id", id_col = "individual.local.identifier"),
  AutumnNight = list(file = "SpringNight_Clustered.csv", cluster_col = "cluster_id", id_col = "individual.local.identifier")
)


# Perform Chi-Square Tests for all periods
all_results <- lapply(periods, function(period) {
  process_period(period$file, period$cluster_col, period$id_col)
})

# Combine Chi-Square results into a single table
final_results <- bind_rows(lapply(all_results, function(res) res$results))

# Print the Chi-Square test results
print(final_results)


# Perform Median and Max Proportion Analysis for all periods
all_proportions <- lapply(periods, function(period) {
  find_median_proportion_fixed(period$file, period$cluster_col, period$id_col) %>%
    mutate(Period = gsub("_Clustered.csv", "", basename(period$file)))
})


# Combine proportion results into a single table
final_proportion_results <- bind_rows(all_proportions)

# Print proportion results
print(final_proportion_results)

# Save proportion results to CSV
write.csv(final_proportion_results, "Spring_Proportion_Results.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
