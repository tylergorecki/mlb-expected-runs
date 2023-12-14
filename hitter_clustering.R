# Assuming you have your data in a data frame named 'df'
# Specify the number of clusters (k)
k <- 3

# Select the columns for clustering
data1 <- read.csv('stats.csv')
data2 <- read.csv('batting.csv')
data <- merge(data1, data2, on='player_id')
data <- data[data$max_hit_speed > 90,]

data_for_clustering <- data[,c('on_base_plus_slg','max_hit_speed')]


# Perform k-means clustering
kmeans_result <- kmeans(data_for_clustering, centers = k)

# Print the cluster assignments for each data point
print(kmeans_result$cluster)

# Print the cluster centers
print(kmeans_result$centers)

# Plot kmeans results
plot(data$on_base_percent, data_for_clustering$max_hit_speed, 
     col = kmeans_result$cluster, pch = 19, main = "K-Means Clustering")
points(kmeans_result$centers[, "on_base_plus_slg"], kmeans_result$centers[, "max_hit_speed"], col = 'blue', pch = 16)

data$cluster <- kmeans_result$cluster
clusters <- data[,c('last_name..first_name', 'cluster')]




library(tidyr)

# Assuming 'df' is your data frame
# Pivot the table to create three separate columns
df_pivoted <- data.frame(pivot_wider(clusters, names_from = cluster, values_from = last_name..first_name))

# Print the pivoted data frame
print(df_pivoted)




##########################################################################

# Hitter type by contact % (1 - whiff %)

hitters <- read.csv('whiff.csv')
hitters <- hitters[,1:4]

hitters$contact_percent <- 100-hitters$whiff_percent
summary(hitters$contact_percent)

library(ggplot2)

# 70, 78 for 27/44/28 split
# 71, 77 for 32/35/33 (even) split
hitters$groups <- ifelse(hitters$contact_percent<70, 'power', 
                         ifelse(hitters$contact_percent<78, 'middle', 'contact'))

ggplot(hitters, aes(x = contact_percent, fill = groups)) +
  geom_histogram(alpha = 0.85, binwidth = .75) +
  labs(title = "Density Plot of Contact Percentage by Hitter Group", x = "Contact Percentage", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))

table(hitters$groups)/nrow(hitters)*100


hitters2 <- hitters[,c(2,7)]
write.csv(hitters2, 'hitter_groups.csv', row.names = F)

groups.names <- pivot_wider(hitters, names_from = groups, values_from = last_name..first_name)
groups.names <- groups.names[,c(5,6,7)]
