# Load necessary libraries for data manipulation, visualization, and modeling
library(ggplot2)       # For data visualization
library(readr)         # For reading data from CSV files
library(tidyverse)     # For data manipulation, includes dplyr and ggplot2
library(patchwork)     # For combining multiple ggplot objects into a single plot
library(broom)         # For tidying model outputs

# Load the dataset, renaming columns for easier readability
snake_data <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17q11RattlesnakeDigestion.csv") %>% 
  rename(meal_size = mealSize, body_temp = tempChange)  # Rename variables to more intuitive names

# Fit a linear regression model predicting body temperature change from meal size
snake_regression <- lm(body_temp ~ meal_size, data = snake_data)

# Plot (a): Scatterplot with regression line
a <- ggplot(snake_data, aes(x = meal_size, y = body_temp)) + 
  geom_point() +                            # Scatterplot of data points
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression line without confidence interval shading
  labs(title = "Regression")                # Title for the plot

# Plot (b): Scatterplot with regression line and 95% confidence interval
b <- ggplot(snake_data, aes(x = meal_size, y = body_temp)) + 
  geom_point() +                            # Scatterplot of data points
  geom_smooth(method = "lm") +              # Linear regression line with 95% confidence interval shading
  labs(title = "95% Confidence Interval")   # Title for the plot

# Plot (c): Scatterplot with 95% prediction interval
c <- bind_cols(
    snake_regression %>% augment(),                        # Augment model data with residuals, fitted values
    snake_regression %>% predict(interval = "predict") %>% # Get prediction intervals for new data points
      data.frame()                                         # Convert prediction intervals to a data frame
  ) %>%
  ggplot(aes(x = meal_size, y = body_temp)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "maroon", alpha = .7) + # Shaded 95% prediction interval
  geom_point() +                                                          # Scatterplot of data points
  geom_smooth(method = lm, se = FALSE, col = "yellow") +                  # Linear regression line without confidence interval
  labs(title = "95% Prediction Interval")                                 # Title for the plot

# Combine plots a, b, and c into a single layout with labels A, B, and C
a + b + c + plot_annotation(tag_levels = "A")
