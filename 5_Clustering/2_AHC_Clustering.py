# ------------------------------------------------------------------------------ 
# Hierarchical Clustering and Dendrograms 
# ------------------------------------------------------------------------------ 
# Author: Ali Moayedi 
# Description: 
# This script performs hierarchical clustering, determines the optimal number 
# of clusters, and visualizes the dendrogram. Results are saved as CSV and PNG. 
# ------------------------------------------------------------------------------

import numpy as np
import pandas as pd
from scipy.cluster.hierarchy import linkage, dendrogram, fcluster
from scipy.spatial.distance import pdist
import matplotlib.pyplot as plt


def find_optimal_k(linked, X, max_ratio=0.25):
    """
    Determines the optimal number of clusters (k) such that no cluster exceeds
    the specified max_ratio of the total sample size.

    Args:
        linked: Linkage matrix from hierarchical clustering.
        X: Input dataset.
        max_ratio: Maximum allowable ratio for the largest cluster size.

    Returns:
        optimal_k: Optimal number of clusters.
        clusters: Cluster assignments for each sample.
    """
    n_samples = len(X)
    for k in range(2, n_samples):
        clusters = fcluster(linked, k, criterion="maxclust")
        cluster_sizes = np.bincount(clusters)
        if max(cluster_sizes) / n_samples <= max_ratio:
            return k, clusters
    return n_samples - 1, clusters  # Return the largest possible number of clusters


def hierarchical_clustering(filename, output_csv, output_plot, title_suffix, max_ratio=0.25, truncate_p=70, color_threshold=None):
    """
    Performs hierarchical clustering and saves cluster assignments and dendrogram.

    Args:
        filename: Input CSV file with features for clustering.
        output_csv: File name to save the cluster assignments.
        output_plot: File name to save the dendrogram plot.
        title_suffix: Suffix for plot titles (e.g., "Autumn Day").
        max_ratio: Maximum allowable ratio for largest cluster size.
        truncate_p: Number of clusters to display in the truncated dendrogram.
        color_threshold: Specific distance for cutting the dendrogram (optional).
    """
    # --------------------------------------------------------------------------
    # Step 1: Load Data
    # --------------------------------------------------------------------------
    data = pd.read_csv(filename)
    X = data.values

    # --------------------------------------------------------------------------
    # Step 2: Perform Hierarchical Clustering
    # --------------------------------------------------------------------------
    # Compute linkage using Ward's method
    linked = linkage(X, method="ward")

    # --------------------------------------------------------------------------
    # Step 3: Determine Optimal Number of Clusters
    # --------------------------------------------------------------------------
    optimal_k, clusters = find_optimal_k(linked, X, max_ratio=max_ratio)

    # --------------------------------------------------------------------------
    # Step 4: Reorder Clusters Based on Dendrogram Order
    # --------------------------------------------------------------------------
    dendro = dendrogram(linked, no_plot=True)
    leaf_order = dendro["leaves"]  # Get the leaf order from the dendrogram
    unique_clusters_in_order = np.unique([clusters[i - 1] for i in leaf_order])
    reordered_clusters = np.zeros_like(clusters)

    for new_id, old_id in enumerate(unique_clusters_in_order, start=1):
        reordered_clusters[clusters == old_id] = new_id

    # Add reordered cluster IDs to the DataFrame
    data["cluster_id"] = reordered_clusters
    data.to_csv(output_csv, index=False)
    print(f"Clusters saved to {output_csv}")

    # --------------------------------------------------------------------------
    # Step 5: Visualize and Save the Dendrogram
    # --------------------------------------------------------------------------
    plt.figure(figsize=(12, 8))
    dendrogram(
        linked,
        orientation="top",
        truncate_mode="lastp",
        p=truncate_p,
        color_threshold=color_threshold if color_threshold else linked[-optimal_k, 2],
        leaf_rotation=90.0,
        leaf_font_size=10.0,
        show_leaf_counts=True,
    )
    plt.title(f"Dendrogram - {title_suffix}")
    plt.xlabel("Data Points")
    plt.ylabel("Distance")
    plt.savefig(output_plot, bbox_inches="tight")
    plt.show()
    print(f"Dendrogram saved to {output_plot}")


# ------------------------------------------------------------------------------ 
# Process All Datasets
# ------------------------------------------------------------------------------ 

datasets = {
    "Autumn Day": (
        "Autumn_Day_CF.csv",
        "Autumn_Day_with_ClusterID.csv",
        "Dendrogram_Autumn_Day.png",
        70,
        92,  # `p` and `color_threshold` for Autumn Day
    ),
    "Autumn Night": (
        "Autumn_Night_CF.csv",
        "Autumn_Night_with_ClusterID.csv",
        "Dendrogram_Autumn_Night.png",
        60,
        37,  # `p` and `color_threshold` for Autumn Night
    ),
    "Spring Day": (
        "Spring_Day_CF.csv",
        "Spring_Day_with_ClusterID.csv",
        "Dendrogram_Spring_Day.png",
        50,
        240.5,  # `p` and `color_threshold` for Spring Day
    ),
    "Spring Night": (
        "Spring_Night_CF.csv",
        "Spring_Night_with_ClusterID.csv",
        "Dendrogram_Spring_Night.png",
        50,
        43,  # `p` and `color_threshold` for Spring Night
    ),
}

for title_suffix, (input_file, output_csv, output_plot, truncate_p, color_threshold) in datasets.items():
    hierarchical_clustering(input_file, output_csv, output_plot, title_suffix, truncate_p=truncate_p, color_threshold=color_threshold)
