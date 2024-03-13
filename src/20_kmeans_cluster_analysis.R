# The purpose of this script is to conduct a k-means cluster analysis
# on the data derived from the mann-kendall and sen's slope tests
# because the trends for ice on and off seem to be grouped

# Libraries
library(tidyverse)
library(factoextra)
library(cluster)
library(here)


#load data
mk_results <- read_csv(here('data/n_american_ice_phenology/results/mk_results.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    event = as.factor(event)
  ) %>% 
  select(-rel_slope)

#remove rows with missing values
mk_results_on <- mk_results %>% 
  na.omit(mk_results) %>% 
  select(1:3,4:5) %>% 
  filter(
    event == 'ice_on'
  )

#scale each variable to have a mean of 0 and sd of 1
mk_scaled <- scale(mk_results[,c(3,4,5,6,7,8)])



str(df)


#load data
df <- USArrests

#remove rows with missing values
df <- na.omit(df)

#scale each variable to have a mean of 0 and sd of 1
df_scaled <- scale(df)

#view first six rows of dataset
head(df_scaled)

#find optimal number of clusters and starting positions

#Method 1: Number of Clusters vs. the Total Within Sum of Squares
fviz_nbclust(df_scaled, kmeans, method = "wss")
#Look for the "bend" when the sum of squares begins to level off
#It appears to be around 4 in the plot

#Method 2: Number of Clusters vs. Gap Statistic
# The gap stat. compares the total intra-cluster variation for 
#different values of k with their expected values for a distribution 
#with no clustering.

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df_scaled,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
#A notable peak at 4, so we will proceed with k clusters

#Kmeans analysis

#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(df_scaled, centers = 4, nstart = 25)

#view results
km

#plot results of final k-means model
fviz_cluster(km, data = df)

#find means of each cluster
aggregate(USArrests, by=list(cluster=km$cluster), mean)









