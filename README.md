# Airline Passenger Satisfaction

This project aims to analyze a real dataset on airline passenger satisfaction using data mining and machine learning techniques, including exploratory data analysis, dimensionality reduction, clustering, and classification models.

## Objectives
- Identify the main drivers of passenger satisfaction
- Apply dimensionality reduction techniques (PCA) to reduce feature space
- Discover hidden patterns using clustering methods
- Build and evaluate predictive classification models

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
- Cluster validation (Silhouette, CH, DB, ARI)

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

## Dataset

The dataset is publicly available on Kaggle:  
https://www.kaggle.com/datasets/johndddddd/customer-satisfaction

A local copy of the dataset is included in the repository under `/data`.


## Results
(Add key insights or best model performance here)
-copiar do pdf


## Authors

Ana Mafalda Araújo do Carmo

Rafaela Afonso Claro Pinto

