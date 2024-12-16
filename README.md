# README: Greater White-Fronted Geese Migration Analysis Codes

## Overview:
This repository contains the scripts and workflows used in the study titled "Multi-modal, interrelated navigation in migratory birds: a data mining study." The workflow includes preprocessing GPS tracking data, annotating it with environmental and geomagnetic variables, clustering the data, and interpreting the results through statistical and visual analyses. The repository is designed to ensure the reproducibility of the methods described in the paper. The final annotated dataset has been made available in the Movebank Data Repository (Kölzsch et al. XX). 

## Repository Structure:
### 1. Preprocessing:
This folder contains scripts for cleaning and organising raw GPS tracking data, including removing errors, aggregating datasets, isolating active migratory flight data, and segmenting data into seasonal datasets. The preprocessing workflow reflects the steps described in the paper but is not required for users downloading the final dataset.

### 2. Annotation:
This folder annotates preprocessed data with environmental variables, including:
- Wind conditions from ERA5 datasets.
- Coastal proximity data from NASA's Distance to Coast dataset.
- Elevation data from ASTER GDEM dataset.

Geomagnetic data annotation uses the [MagGeo Repository] (https://github.com/username/MagGeo), available separately on GitHub. Refer to the MagGeo repository for detailed geomagnetic data extraction methods.

### 3. Autumn and 4. Spring:
These folders contain workflows for seasonal analyses of autumn and spring migration data. Processes include movement parameter calculations, clustering feature extraction, and statistical analyses. Users of the final dataset can refer directly to these workflows for analysis without repeating preprocessing steps.
Key steps include:
- Processing annotated GPS data.
- Classifying day vs. night data points.
- Calculating wind support, crosswind, and geomagnetic parameters such as apparent angle of geomagnetic inclination.
- Extracting clustering features (deltas in environmental and geomagnetic cues) to study behavioural patterns.
- Performing statistical analyses and generating visualizations to interpret results.

### 5. Clustering:
This folder evaluates clustering solutions and performs Agglomerative Hierarchical Clustering (AHC). Outputs include dendrograms, cluster IDs, and validation metrics such as Silhouette and Calinski-Harabasz indices.

## Requirements:
- R Packages: tidyverse, move, maptools
- Python Libraries: numpy, pandas, scipy, sklearn

## Contact:
For questions or issues, please contact:
- Ali Moayedi
- University of St Andrews, UK
- Email: am636@st-andrews.ac.uk


## Citation:
If you use this code, please cite the associated paper:  

Moayedi, A., et al. *"Multi-modal, interrelated navigation in migratory birds: a data mining study", Ecological Informatics (2024). DOI: [Pending]  

