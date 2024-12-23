# ------------------------------------------------------------------------------ 
# Validation Indices for Clustering 
# ------------------------------------------------------------------------------ 
# Author: Ali Moayedi 
# Description: 
# This script calculates Silhouette and Calinski-Harabasz (CH) indices for 
# evaluating clustering solutions. Results are saved as plots and printed. 
# ------------------------------------------------------------------------------

import numpy as np
import pandas as pd
from scipy.cluster.hierarchy import linkage, fcluster
from sklearn.metrics import silhouette_score, calinski_harabasz_score
import matplotlib.pyplot as plt

def calculate_validation_indices(filename, output_plot, title_suffix):
    """
    Calculates validation indices for clustering.
    
    Args:
        filename: Input CSV file with features for clustering.
        output_plot: File name for saving the validation plot.
        title_suffix: Suffix for plot titles (e.g., "Autumn Day").
    """
    # Load data
    data = pd.read_csv(filename)
    X = data.values

    # Perform hierarchical clustering
    linked = linkage(X, method='ward')

    # Calculate validation indices
    cluster_range = range(2, 20)
    silhouette_scores = []
    calinski_harabasz_scores = []

    for k in cluster_range:
        cluster_labels = fcluster(linked, k, criterion='maxclust')
        silhouette_scores.append(silhouette_score(X, cluster_labels))
        calinski_harabasz_scores.append(calinski_harabasz_score(X, cluster_labels))

    # Plot results
    plt.figure(figsize=(12, 6))
    plt.subplot(1, 2, 1)
    plt.plot(cluster_range, silhouette_scores, marker='o', label='Silhouette Score')
    plt.title(f'Silhouette Score - {title_suffix}')
    plt.xlabel('Number of Clusters')
    plt.ylabel('Score')
    plt.grid(True)
    plt.legend()

    plt.subplot(1, 2, 2)
    plt.plot(cluster_range, calinski_harabasz_scores, marker='o', label='CH Index')
    plt.title(f'Calinski-Harabasz Index - {title_suffix}')
    plt.xlabel('Number of Clusters')
    plt.ylabel('Score')
    plt.grid(True)
    plt.legend()

    plt.tight_layout()
    plt.savefig(output_plot)
    plt.show()

    print(f"Validation plots saved to {output_plot}")

# Run for all datasets
datasets = {
    "Autumn Day": ("Autumn_Day_CF.csv", "Validation_Indices_Autumn_Day.png"),
    "Autumn Night": ("Autumn_Night_CF.csv", "Validation_Indices_Autumn_Night.png"),
    "Spring Day": ("Spring_Day_CF.csv", "Validation_Indices_Spring_Day.png"),
    "Spring Night": ("Spring_Night_CF.csv", "Validation_Indices_Spring_Night.png"),
}

for title_suffix, (input_file, output_file) in datasets.items():
    calculate_validation_indices(input_file, output_file, title_suffix)
