library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(reshape2)

survey_df <- read.csv("D:\Bootcamp VCU datasets\Survey.csv")
head(survey_df)

cols_for_clustering <- 18:46
survey_df[cols_for_clustering] <- lapply(survey_df[cols_for_clustering], as.numeric)
survey_df <- survey_df %>% drop_na()

# Standardize the data
scaled_data <- scale(survey_df[, cols_for_clustering])

# Use the Elbow method to find the optimal number of clusters
fviz_nbclust(scaled_data, kmeans, method = "wss")

# Perform k-means clustering with the chosen number of clusters
set.seed(123)  # For reproducibility
k <- 5
kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)

kmeans_result

# Create a new dataframe with cluster characteristics
cluster_summary <- survey_df %>%
  mutate(Cluster = kmeans_result$cluster) %>%
  group_by(Cluster) %>%
  summarise(across(cols_for_clustering, list(mean = mean, sd = sd)))

print(cluster_summary)

