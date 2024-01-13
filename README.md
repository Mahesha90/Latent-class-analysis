# Using Latent Class Analysis to analyze interviews with Program Directors

## Overview
I conducted this project as a part of my PhD studies, which involved analyzing interviews with program directors about student dropouts. The analysis included using both Latent Class Analysis (LCA) and Cluster Analysis to identify patterns and clusters within the collected data. The results of this analysis have already been published.

## Setup
To run the analysis, you need to have R Studio installed and connect with Github if you need to upload the data to a Github repository.

## Latent Class Analysis (LCA)
Latent Class Analysis (LCA) is a statistical technique used for identifying underlying patterns or clusters within a dataset. It falls under the broader category of latent variable modeling, where the focus is on uncovering unobservable (latent) structures that explain the observed relationships in the data.

### Data Loading
Load the data for LCA:

```R
lca.data <- read.table("./Binary_AllCodes_12.txt", header = TRUE)
tocluster <- lca.data[, c(-1)]
```
