library(RSQLite)
setwd("/Users/hyeongjeongyi/Documents/RA")
source("EDA.R")
source("modeling.R")

library(ggplot2)
ggplot(mse_data, aes(x = Method, y = as.factor(SubsampleSize), label = sprintf("%.5f", MSE))) +
  geom_tile(fill = "white", color = "black") +
  geom_text(size = 4) +
  labs(title = "MSE Values for Different Methods and Subsample Sizes",
       x = "Method", y = "Subsample Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plots for each method
library(ggplot2)
library(gridExtra)
lr_plot <- ggplot(mse_data[mse_data$Method == "Linear Regression", ], 
                  aes(x = as.factor(SubsampleSize), y = MSE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "MSE Values for Linear Regression",
       x = "Subsample Size", y = "MSE") +
  theme_minimal()

# XGBoost
xgboost_plot <- ggplot(mse_data[mse_data$Method == "XGBoost", ], 
                       aes(x = as.factor(SubsampleSize), y = MSE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "MSE Values for XGBoost",
       x = "Subsample Size", y = "MSE") +
  theme_minimal()

# Lasso
lasso_plot <- ggplot(mse_data[mse_data$Method == "Lasso", ], 
                     aes(x = as.factor(SubsampleSize), y = MSE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "MSE Values for Lasso",
       x = "Subsample Size", y = "MSE") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(lr_plot, xgboost_plot, lasso_plot, ncol = 1)


