# Loading libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(GGally)
set.seed(302)

# Importing data sets
profit_df <- read_csv("/Users/huongngo/Desktop/SPA DRP - Research/eda-airline-data/Data/profit_data.csv")
traffic_capacity_df <- read_csv("/Users/huongngo/Desktop/SPA DRP - Research/eda-airline-data/Data/traffic_capacity_data.csv")

# Graph theme
g_theme <- theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Preliminary Visualizations

# Scatterplot Matrix for profit_df
profit_scatter_df <- profit_df %>% select(net_income, 
                                          op_profit_loss, 
                                          prop_freight, 
                                          op_revenues,
                                          flying_ops,
                                          maintenance,
                                          aircft_services,
                                          op_expenses)
plot(profit_scatter_df)

net_income_neg <- profit_df %>% filter(net_income < 0)
net_income_pos <- profit_df %>% filter(net_income > 0)

ggplot(data = net_income_neg, aes(x = log10(-net_income))) +
  geom_histogram(bins = 15) +
  theme_bw()

ggplot(data = net_income_pos, aes(x = log10(net_income))) +
  geom_histogram(bins = 15) +
  theme_bw()

op_loss <- profit_df %>% filter(op_profit_loss < 0)
op_profit <- profit_df %>% filter(op_profit_loss > 0)

ggplot(data = op_loss, aes(x = log10(-op_profit_loss))) +
  geom_histogram(bins = 15) +
  theme_bw()

ggplot(data = op_profit, aes(x = log10(op_profit_loss))) +
  geom_histogram(bins = 15) +
  theme_bw() 

# PCA

# Retrieving dataframe with only numerical values
profit_pca_df <- profit_df[-(19:26)]
# Running PCA
profit_pca_list <- princomp(profit_pca_df, cor = TRUE)
# Generating scree plot to choose number of components
screeplot(profit_pca_list, type = "lines")

# Plotting the loadings in two dimensional space
profit_pca_list$loadings <- unclass(profit_pca_list$loadings)

profit_loadings <- as.data.frame(profit_pca_list$loadings)

ggplot(data = profit_loadings, aes(x = Comp.1, 
                                   y = Comp.2, 
                                   label = row.names(profit_loadings))) +
  geom_point() +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "Loadings of First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_loadings, aes(x = Comp.1, 
                                   y = Comp.3, 
                                   label = row.names(profit_loadings))) +
  geom_point() +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "Loadings of First and Third Principal Components",
       x = "First Principal Component",
       y = "Third Principal Component") +
  g_theme

ggplot(data = profit_loadings, aes(x = Comp.2, 
                                   y = Comp.3, 
                                   label = row.names(profit_loadings))) +
  geom_point() +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "Loadings of Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

# Plotting the principal components in two dimensional space
profit_scores <- as.data.frame(profit_pca_list$scores)
profit_scores <- profit_scores %>% 
  mutate(carrier_name = profit_df$unique_carrier_name,
         year = as.factor(profit_df$year),
         quarter = as.factor(profit_df$quarter),
         region = as.factor(profit_df$region))

# Get all distinct carriers
carrier_names <- profit_df %>% 
  distinct(unique_carrier_name)

# Divide principal components into 2 groups: large and small air carriers
profit_scores_large <- profit_scores %>%
  filter(carrier_name %in% c("American Airlines Inc.",
                             "Delta Air Lines Inc.",
                             "Southwest Airlines Co.",
                             "Alaska Airlines Inc.",
                             "United Air Lines Inc.",
                             "JetBlue Airways",
                             "Spirit Air Lines"))
profit_scores_small <- profit_scores %>%
  filter(!(carrier_name %in% c("American Airlines Inc.",
                               "Delta Air Lines Inc.",
                               "Southwest Airlines Co.",
                               "Alaska Airlines Inc.",
                               "United Air Lines Inc.",
                               "JetBlue Airways",
                               "Spirit Air Lines")))

# PC1 and PC2 - Colored by year
ggplot(data = profit_scores, aes(x = Comp.1,
                                 y = Comp.2,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.1,
                                 y = Comp.2,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(-3, 3) +
  ylim(-3, 3) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.1,
                                 y = Comp.2,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(0, 2) +
  ylim(-1, 1) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.1,
                                 y = Comp.2,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(1.25, 1.75) +
  ylim(-1, 0.5) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme 

# PC1 and PC2 - Colored by quarter, Faceted by year
# 2019
ggplot(data = profit_scores %>% filter(year == 2019), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores %>% filter(year == 2019), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(-5, 1) +
  ylim(-0.5, 3) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

# 2020
ggplot(data = profit_scores %>% filter(year == 2020), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores %>% filter(year == 2020), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(-2.5, 2.5) +
  ylim(-2.5, 0.5) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores %>% filter(year == 2020), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(1, 2) +
  ylim(-0.25, 0.5) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

# 2021
ggplot(data = profit_scores %>% filter(year == 2021), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores %>% filter(year == 2021), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(0, 2.5) +
  ylim(-1, 1) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

ggplot(data = profit_scores %>% filter(year == 2021), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = quarter)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(1.375, 1.75) +
  ylim(0, 0.25) +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

# PC1 and PC3
ggplot(data = profit_scores, aes(x = Comp.1,
                                 y = Comp.3,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(0, 2.5) +
  ylim(-1, 1) +
  labs(title = "First and Third Principal Components",
       x = "First Principal Component",
       y = "Third Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.1,
                                 y = Comp.3,
                                 label = carrier_name,
                                 color = region)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  facet_wrap(profit_scores$year) +
  labs(title = "First and Third Principal Components",
       x = "First Principal Component",
       y = "Third Principal Component") +
  g_theme

# PC2 and PC3
ggplot(data = profit_scores, aes(x = Comp.2,
                                 y = Comp.3,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.2,
                                 y = Comp.3,
                                 label = carrier_name,
                                 color = region)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  facet_wrap(profit_scores$year) +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.2,
                                 y = Comp.3,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(-2.5, 2.5) +
  ylim(-1, 0) +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.2,
                                 y = Comp.3,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(-1.25, 1.25) +
  ylim(-0.75, 0.5) +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

ggplot(data = profit_scores, aes(x = Comp.2,
                                 y = Comp.3,
                                 label = carrier_name,
                                 color = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  xlim(0, 0.5) +
  ylim(-0.5, -0.125) +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

# K-Means Clustering

# Scaling the data
profit_scaled_df <- profit_df[, 1:18]
mu <- colMeans(profit_scaled_df)
sigma <- sapply(profit_scaled_df, sd)
for (i in 1:ncol(profit_scaled_df)) {
  for (j in 1:nrow(profit_scaled_df)) {
    profit_scaled_df[j, i] <- (profit_scaled_df[j, i] - mu[i])/sigma[i]
  }
}

# Finding k (entire data)
wss <- rep(0, 6)
for (i in 2:6) {
  wss[i] <- sum(kmeans(profit_scaled_df, centers = i)$withinss)
}

wss_df <- data.frame(wss)
wss_df <- wss_df %>% 
  mutate(num_cluster = 1:6) %>%
  filter(rownames(wss_df) != 1)

ggplot(data = wss_df, aes(x = num_cluster, y = wss)) +
  geom_line() +
  geom_point(size = 4) +
  theme_bw()

k_clusters_4 <- kmeans(profit_scaled_df, centers = 4)

ggplot(data = profit_scores, aes(x = Comp.1,
                                 y = Comp.2,
                                 label = carrier_name,
                                 color = as.factor(k_clusters_4$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

# Finding k (categorized by year)
profit_scaled_df <- profit_scaled_df %>%
  mutate(year = profit_df$year)

# 2019
profit_scaled_2019 <- profit_scaled_df %>% 
  filter(year == 2019)

wss_2019 <- rep(0, 6)
for (i in 2:6) {
  wss_2019[i] <- sum(kmeans(profit_scaled_2019, centers = i)$withinss)
}

# Getting all tested k values
wss_2019_df <- data.frame(wss_2019)
wss_2019_df <- wss_2019_df %>% 
  mutate(num_cluster = 1:6) %>%
  filter(rownames(wss_2019_df) != 1)

# Plotting elbow plot to find optimal k
ggplot(data = wss_2019_df, aes(x = num_cluster, y = wss_2019)) +
  geom_line() +
  geom_point(size = 4) +
  theme_bw()

# Running k-means clustering on 2019 data
k_clusters_5_2019 <- kmeans(profit_scaled_2019, centers = 5)

# Plotting with PC1 and PC2
ggplot(data = profit_scores %>% filter(year == 2019), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_5_2019$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

# Plotting with PC1 and PC3
ggplot(data = profit_scores %>% filter(year == 2019), aes(x = Comp.1,
                                                          y = Comp.3,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_5_2019$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First and Third Principal Components",
       x = "First Principal Component",
       y = "Third Principal Component") +
  g_theme

# Plotting with PC2 and PC3
ggplot(data = profit_scores %>% filter(year == 2019), aes(x = Comp.2,
                                                          y = Comp.3,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_5_2019$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

# Preparing dataset for parallel coordinates plot
k_clusters_4_2019_pc <- profit_scaled_2019 %>%
  mutate(cluster = as.factor(k_clusters_5_2019$cluster))

# First 9 variables
ggparcoord(data = k_clusters_4_2019_pc,
           columns = 1:9,
           groupColumn = "cluster") +
  labs(title = "Parallel Coordinates Plot",
       subtitle = "First 9 Variables") +
  theme_bw()

# Last 10 variables
ggparcoord(data = k_clusters_4_2019_pc,
           columns = 9:18,
           groupColumn = "cluster") +
  labs(title = "Parallel Coordinates Plot",
       subtitle = "Last 10 Variables") +
  theme_bw()

# 2020
profit_scaled_2020 <- profit_scaled_df %>% 
  filter(year == 2020)

wss_2020 <- rep(0, 6)
for (i in 2:6) {
  wss_2020[i] <- sum(kmeans(profit_scaled_2020, centers = i)$withinss)
}

# Getting all tested k values
wss_2020_df <- data.frame(wss_2020)
wss_2020_df <- wss_2020_df %>% 
  mutate(num_cluster = 1:6) %>%
  filter(rownames(wss_2020_df) != 1)

# Plotting elbow plot to find optimal k
ggplot(data = wss_2020_df, aes(x = num_cluster, y = wss_2020)) +
  geom_line() +
  geom_point(size = 4) +
  theme_bw()

# Running k-means clustering on 2020 data
k_clusters_4_2020 <- kmeans(profit_scaled_2020, centers = 4)

# Plotting PC1 and PC2
ggplot(data = profit_scores %>% filter(year == 2020), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_4_2020$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

# Plotting PC1 and PC3
ggplot(data = profit_scores %>% filter(year == 2020), aes(x = Comp.1,
                                                          y = Comp.3,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_4_2020$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First and Third Principal Components",
       x = "First Principal Component",
       y = "Third Principal Component") +
  g_theme

# Plotting PC2 and PC3
ggplot(data = profit_scores %>% filter(year == 2020), aes(x = Comp.2,
                                                          y = Comp.3,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_4_2020$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

# Preparing dataset forr parallel coordinates plot
k_clusters_4_2020_pc <- profit_scaled_2020 %>%
  mutate(cluster = as.factor(k_clusters_4_2020$cluster))

# First 9 variables
ggparcoord(data = k_clusters_4_2020_pc,
           columns = 1:9,
           groupColumn = "cluster") +
  labs(title = "Parallel Coordinates Plot",
       subtitle = "First 9 Variables") +
  theme_bw()

# Last 10 variables
ggparcoord(data = k_clusters_4_2020_pc,
           columns = 9:18,
           groupColumn = "cluster") +
  labs(title = "Parallel Coordinates Plot",
       subtitle = "Last 10 Variables") +
  theme_bw()

# 2021
profit_scaled_2021 <- profit_scaled_df %>% 
  filter(year == 2021)

wss_2021 <- rep(0, 6)
for (i in 2:6) {
  wss_2021[i] <- sum(kmeans(profit_scaled_2021, centers = i)$withinss)
}

# Getting all tested k values
wss_2021_df <- data.frame(wss_2021)
wss_2021_df <- wss_2021_df %>% 
  mutate(num_cluster = 1:6) %>%
  filter(rownames(wss_2021_df) != 1)

# Plotting elbow plot to find optimal k
ggplot(data = wss_2021_df, aes(x = num_cluster, y = wss_2021)) +
  geom_line() +
  geom_point(size = 4) +
  theme_bw()

# Running k-means clustering on 2021 data
k_clusters_4_2021 <- kmeans(profit_scaled_2021, centers = 4)

# Plotting PC1 and PC2
ggplot(data = profit_scores %>% filter(year == 2021), aes(x = Comp.1,
                                                          y = Comp.2,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_4_2021$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component") +
  g_theme

# Plotting PC1 and PC3
ggplot(data = profit_scores %>% filter(year == 2021), aes(x = Comp.1,
                                                          y = Comp.3,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_4_2021$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "First and Third Principal Components",
       x = "First Principal Component",
       y = "Third Principal Component") +
  g_theme

# Plotting PC2 and PC3
ggplot(data = profit_scores %>% filter(year == 2021), aes(x = Comp.2,
                                                          y = Comp.3,
                                                          label = carrier_name,
                                                          color = as.factor(k_clusters_4_2021$cluster))) +
  geom_point(size = 3,
             alpha = 0.5) +
  geom_text_repel(size = 4,
                  color = "red",
                  nudge_x = 0.02,
                  nudge_y = -0.02) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  labs(title = "Second and Third Principal Components",
       x = "Second Principal Component",
       y = "Third Principal Component") +
  g_theme

# Preparing dataset for parallel coordinates plot
k_clusters_4_2021_pc <- profit_scaled_2021 %>%
  mutate(cluster = as.factor(k_clusters_4_2021$cluster))

# First 9 variables
ggparcoord(data = k_clusters_4_2021_pc,
           columns = 1:9,
           groupColumn = "cluster") +
  labs(title = "Parallel Coordinates Plot",
       subtitle = "First 9 Variables") +
  theme_bw()

# Last 10 variables
ggparcoord(data = k_clusters_4_2021_pc,
           columns = 9:18,
           groupColumn = "cluster") +
  labs(title = "Parallel Coordinates Plot",
       subtitle = "Last 10 Variables") +
  theme_bw()
