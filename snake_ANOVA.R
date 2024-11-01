library(ggplot2)
library(readr)
library(tidyverse)
library(patchwork)
library(broom)

snake_data <-  read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17q11RattlesnakeDigestion.csv")%>% 
  rename(meal_size = mealSize, body_temp = tempChange)
 
snake_regression <- lm(body_temp ~ meal_size, data = snake_data) 

# Plot (a): Shows the total deviation of each observation from the mean of body temperature change
a <- ggplot(mutate(snake_data, mean_temp = mean(body_temp)), 
            aes(x = meal_size, y = body_temp)) +
  geom_point(alpha = .8, size = 4) +               # Scatter plot of meal size vs. temperature change
  geom_hline(aes(yintercept = mean_temp)) +        # Horizontal line at the mean temperature change
  geom_segment(aes(xend = meal_size, yend = mean_temp), color = "black", alpha = .5) + # Line segments from each point to the mean
  labs(title = "Total deviation", y = "Temp change", x = "Meal size") +  # Title and axis labels
  theme(axis.text = element_text(size = 12),       # Custom axis text size
        axis.title = element_text(size = 15),      # Custom axis title size
        plot.title = element_text(size = 18))      # Custom plot title size

# Plot (b): Shows model deviation from the mean, displaying the predicted values
b <- ggplot(mutate(augment(snake_regression), mean_temp = mean(body_temp)), 
            aes(x = meal_size, y = body_temp)) +
  geom_point(alpha = .8, size = 4) +               # Scatter plot of meal size vs. temperature change
  geom_hline(aes(yintercept = mean_temp)) +        # Horizontal line at the mean temperature change
  geom_segment(aes(xend = meal_size, y = .fitted, yend = mean_temp), color = "black", alpha = .5) + # Line segments from predictions to mean
  geom_line(aes(y = .fitted), alpha = 2, color = "blue") + # Line showing predicted values (fitted line)
  labs(title = "Model deviation", y = "Temp change", x = "Meal size") +  # Title and axis labels
  theme(axis.text = element_text(size = 12),       # Custom axis text size
        axis.title = element_text(size = 15),      # Custom axis title size
        plot.title = element_text(size = 18))      # Custom plot title size

# Plot (c): Shows error (residual) deviation, which is the difference between observed and predicted values
c <- ggplot(augment(snake_regression), 
            aes(x = meal_size, y = body_temp)) +
  geom_point(alpha = .8, size = 4) +               # Scatter plot of meal size vs. temperature change
  geom_segment(aes(xend = meal_size, yend = .fitted), color = "black", alpha = .5) + # Line segments from observations to predictions
  geom_line(aes(y = .fitted), alpha = 2, color = "blue") + # Line showing predicted values (fitted line)
  labs(title = "Error deviation", y = "Temp change", x = "Meal size") +  # Title and axis labels
  theme(axis.text = element_text(size = 12),       # Custom axis text size
        axis.title = element_text(size = 15),      # Custom axis title size
        plot.title = element_text(size = 18))      # Custom plot title size

# Combine plots a, b, and c into a single layout with titles and collected axis labels
a + b + c + 
  plot_annotation(tag_levels = "A") +              # Add tags A, B, C to each plot
  plot_layout(axis_titles = "collect")             # Collect axis titles in the combined layout
