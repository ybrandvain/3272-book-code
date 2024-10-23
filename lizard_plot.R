# Load the necessary libraries
library(readr)     # For reading CSV files from URLs or local sources
library(dplyr)     # For data manipulation and cleaning
library(ggplot2)   # For data visualization

# Read the lizard dataset from the provided URL and remove rows with missing values
lizards <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12e3HornedLizards.csv") %>% 
  na.omit()  # Removes any rows with NA (missing) values

# Create a jitter plot with mean and confidence interval overlay
ggplot(lizards, aes(x = Survival, y = squamosalHornLength, color = Survival)) +   # `aes()` defines the aesthetics of the plot (x-axis, y-axis, and color)  
  geom_jitter(height = 0, width = 0.2, size = 1.5, alpha = 0.5, show.legend = FALSE) +  # Adds points with slight random noise (jitter) horizontally, to avoid overlap # `height = 0` ensures there's no vertical jitter, `width = 0.2` adds some horizontal spread # `size = 1.5` sets the point size, `alpha = 0.5` makes points semi-transparent
  stat_summary(fun.data = "mean_cl_normal", color = "black")  # Adds a summary statistic layer. `mean_cl_normal` calculates mean and confidence interval # `color = "black"` makes the summary statistic line black, for contrast with the points
