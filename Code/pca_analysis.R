# Loading libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel) 

# Import datasets
profit_df <- read_csv("/Users/huongngo/Desktop/SPA DRP - Research/eda-airline-data/Data/profit_data.csv")
traffic_capacity_df <- read_csv("/Users/huongngo/Desktop/SPA DRP - Research/eda-airline-data/Data/traffic_capacity_data.csv")

# Retrieving dataframe with only numerical values
profit_pca_df <- profit_df[-(19:26)]
# Running PCA
profit_pca_list <- princomp(profit_pca_df, cor = TRUE)
# Generating scree plot to choose number of components
screeplot(profit_pca_list, type = "lines")

# Graph theme

g_theme <- theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

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




