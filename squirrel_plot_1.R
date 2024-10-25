# Load necessary libraries
library(readr)      # for reading in the CSV file
library(forcats)    # for working with factors and reordering factor levels
library(dplyr)      # for data manipulation
library(ggplot2)    # for data visualization

# Read in the data from a CSV file hosted online
sqrl <- read_csv("https://raw.githubusercontent.com/ybrandvain/biostat/master/data/sqrl.csv")

# Reorder the ambient_temp factor to ensure the levels are in the desired order: cold, warm, hot
# This helps for better visualization in the plot, arranging from coldest to hottest
sqrl <- mutate(sqrl, ambient_temp = fct_relevel(ambient_temp, "cold", "warm", "hot"))

# Create a scatter plot with jitter to show body temperature by ambient temperature
ggplot(sqrl, aes(x = ambient_temp, y = body_temp, color = ambient_temp))+
  geom_jitter(height = 0, width = .2, size = 2, alpha = .35, show.legend = FALSE)+
  # Add error bars representing the mean and confidence limits
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .2, color = "black")+
  # Manually set the colors for the ambient temperature categories: blue for cold, orange for warm, red for hot
  scale_color_manual(values = c("blue", "orange", "red"))
