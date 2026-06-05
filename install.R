
packages <- c(
  "readxl",
  "dplyr",
  "tidyverse",
  "ggplot2",
  "corrplot",
  "knitr",
  "MASS",
  "cluster",
  "clusterCrit",
  "mclust",
  "pROC",
  "rpart",
  "rpart.plot",
  "caret",
  "class"
)

# Check which packages are not installed yet
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

# Install missing packages only
if (length(to_install) > 0) {
  install.packages(to_install)
}

# Load all packages
lapply(packages, library, character.only = TRUE)