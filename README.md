# Airline Passenger Satisfaction

This project aims to analyze a real dataset on airline passenger satisfaction using data mining and machine learning techniques, including exploratory data analysis, dimensionality reduction, clustering, and classification models.

## Objectives
- Identify the main drivers of passenger satisfaction
- Apply dimensionality reduction techniques (PCA) to reduce feature space
- Discover hidden patterns using clustering methods
- Build and evaluate predictive classification models


## Dataset

The dataset is publicly available on Kaggle:  
https://www.kaggle.com/datasets/mysarahmadbhat/airline-passenger-satisfaction

A local copy of the dataset is included in the repository.


## Implementation

To reproduce this project, first install the required R packages.
The repository includes an `install.R` script that automatically installs and loads all required dependencies.


## Methods

### Exploratory Data Analysis (EDA)
- Data cleaning and preprocessing
- Univariate and bivariate analysis
- Correlation analysis

### Dimensionality Reduction
- Principal Component Analysis (PCA)
- Interpretation of principal components

### Unsupervised Learning
- K-Means clustering
- Hierarchical clustering
- Gaussian Mixture Models (GMM)

### Supervised Learning
- Linear Discriminant Analysis (LDA)
- Quadratic Discriminant Analysis (QDA)
- Decision Trees
- Logistic Regression
- K-Nearest Neighbors (KNN)

## Model Evaluation

The performance of the models was evaluated using multiple metrics depending on the method:

- Accuracy and confusion matrix for all supervised learning models
- ROC curve and AUC for Logistic Regression
- Cross-validation for hyperparameter tuning (KNN)
- Internal clustering validation:
  - Silhouette score
  - Calinski-Harabasz index
  - Davies-Bouldin index
  - Ball-Hall index
  - Gamma index
- External clustering validation using Adjusted Rand Index (ARI)
- Model selection criteria:
  - Elbow method (K-Means)
  - BIC (Gaussian Mixture Models)
  - Variance explained / Kaiser criterion / elbow method  (PCA)


## Results

Some of the main results are summarized below. More detailed analyses can be found in the full report.

- PCA proved useful for reducing dimensionality and mitigating multicollinearity among variables. Although it did not consistently improve predictive performance, it was particularly beneficial for clustering methods by providing a more stable and structured feature space.

- Clustering methods were able to identify meaningful latent structures in the data; however, these clusters did not directly correspond to the satisfaction variable, which is expected given their unsupervised nature.

- The supervised learning models achieved broadly similar performance in terms of accuracy. Logistic Regression stood out due to its strong balance between predictive performance and low computational cost, making it an efficient and robust model for this dataset.

- Decision Trees provided an intuitive and interpretable structure, enabling clear identification of the most relevant variables influencing passenger satisfaction.

- In contrast, K-Nearest Neighbors (KNN) showed higher sensitivity to dataset size and feature dimensionality, resulting in increased computational cost and variability in performance depending on the data representation.

- Overall, the results indicate that passenger satisfaction is strongly influenced by factors related to in-flight service quality, digital convenience, onboard comfort, and overall travel experience.

## Tools & Technologies

- Programming Language: R
- Development Environment: RStudio
- Data Manipulation: dplyr, tidyr, tidyverse
- Visualization: ggplot2, corrplot, rpart.plot
- Machine Learning:
  - Classification: MASS (LDA/QDA), rpart (Decision Trees), caret (KNN), glm (Logistic Regression)
  - Clustering: stats (kmeans, hclust), mclust (GMM), cluster, clusterCrit
  - Dimensionality Reduction: stats (PCA - prcomp)
- Evaluation Metrics:
  - Accuracy, Confusion Matrix
  - ROC Curve (pROC)
  - Adjusted Rand Index (mclust)
  - Silhouette, Davies-Bouldin, Calinski-Harabasz


## Authors

Ana Mafalda Araújo do Carmo

Rafaela Afonso Claro Pinto

