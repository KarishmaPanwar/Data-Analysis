# Karishma Panwar
# This code import a dataset of country-level data and generate clusters using 
# the k-means clustering method.

# Install the tidyverse and rules packages
# install.packages("tidyverse")
# install.packages("factoextra")

# Load the tidyverse and rules libraries
library(tidyverse)
library(stats)
library(factoextra)
library(cluster)
library(gridExtra)

# Set the working directory to your Lab10 folder
setwd("C:/Users/ual-laptop/Documents/Rworkspace/Lab11")

# Read InstacartTransactions.csv into an object called instacartTransactions 
# using the read.transactions() function
countries <- read_csv(file = 'CountryData.csv',
                             col_types = 'cnnnnini',
                             col_names = TRUE)

# Display the countries tibble on the console
print(countries)

# Display the structure of the countries tibble
str(countries)


# Display a summary of countries
summary(countries)

# Convert the column containing the country name to the row title of the tibble
countries <- countries %>% column_to_rownames(var = "Country")

# Remove countries from the tibble with missing data in any feature using the 
# following code:
countries <- countries %>% drop_na()

# View the summary of the countries tibble again to ensure there are no NA 
# values
summary(countries)

# cluster the countries based on their corruption index and the number of days 
# it takes to open a business
countriesScaled <- countries %>%
  select(CorruptionIndex, DaysToOpenBusiness) %>% scale()

# Set the random seed to 679
set.seed(679)

# Generate the k-means clusters in an object called countries4Clusters using 4 
# clusters and a value of 25 for nstart
countries4Clusters <- kmeans(x = countriesScaled,
                             centers = 4,
                             nstart = 25)

# Display cluster sizes on the console
countries4Clusters$size

# Display cluster centers (z-scores) on the console
countries4Clusters$centers

# Visualize the clusters
fviz_cluster(object = countries4Clusters,
             data = countriesScaled,
             repel = FALSE)

# Optimizing the value for k  
# Elbow method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = 'wss')

# Average silhouette method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = 'silhouette')

# Gap statistic method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = 'gap_stat')

# Regenerate the cluster analysis using the optimal number of clusters
countries3Clusters <- kmeans(x = countriesScaled,
                             centers = 3,
                             nstart = 25)

# Display cluster sizes on the console
countries3Clusters$size

countries3Clusters$cluster

# Display cluster centers (z-scores) on the console
countries3Clusters$centers

# Visualize the clusters
fviz_cluster(object = countries3Clusters,
             data = countriesScaled,
             repel = FALSE)

# Determine similarities and differences among the clusters using the remaining 
# features in the dataset (GiniCoefficient, GDPPerCapita, EduPercGovSpend, 
# EduPercGDP, and CompulsoryEducationYears
countries %>%
  mutate(cluster = countries3Clusters$cluster) %>%
  select(cluster,
         GiniCoefficient,
         GDPPerCapita,
         EduPercGovSpend,
         EduPercGDP,
         CompulsoryEducationYears) %>%
  group_by(cluster) %>%
  summarise_all("mean")
