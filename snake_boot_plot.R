# Load required libraries
library(reader)    # For reading CSV data (note: should be readr, as reader doesn't include read_csv())
library(ggplot2)   # For creating visualizations
library(dplyr)     # For data manipulation and piping (%>%)
library(patchwork) # For arranging multiple ggplot objects

# Load and clean the dataset
snake_data <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17q11RattlesnakeDigestion.csv") %>% 
  rename(meal_size = mealSize, body_temp = tempChange)
# Reads in the data from the URL, renames the columns 'mealSize' to 'meal_size' and 'tempChange' to 'body_temp' for easier use.

# Generate bootstrap samples for regression coefficients
snake_boot <- replicate(1000, simplify = FALSE,
                        slice_sample(snake_data, prop=1, replace = TRUE) %>% # Resample data with replacement (1000 samples)
                          lm(body_temp ~ meal_size, data = .) %>%           # Fit a linear model for each bootstrap sample
                          coef()) %>%                                       # Extract coefficients (intercept and slope)
  bind_rows()  # Bind results into a single data frame with each row representing coefficients of a bootstrap sample

# Create scatterplot with bootstrap regression lines
a <- ggplot(snake_data, aes(x = meal_size, y = body_temp)) + 
  geom_abline(data = snake_boot, alpha = .025, 
              aes(slope = meal_size, intercept = `(Intercept)`)) + # Plot each bootstrap regression line
  geom_point(alpha = .7, size = 3) +                              # Add original data points
  labs(y = "temp change", x = "meal size", 
       title = "The bootstrap dist of regression lines\n\n\n") +  # Add axis labels and title
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 15)) # Set text sizes

# Plot bootstrap distribution of slope and intercept
snake_boot_plot <- ggplot(snake_boot, aes(x = `(Intercept)`, y = meal_size)) +
  geom_point(alpha = .4) +                                         # Scatterplot of intercept vs. slope from bootstrap samples
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 15)) +
  labs(y = "slope", title = "The bootstrap dist. of slopes & intercepts") # Labels and title for the plot

# Add histograms to the margins of snake_boot_plot for intercept and slope distributions
b <- ggMarginal(snake_boot_plot, type = "histogram", color = "white")

# Combine plots with patchwork
a + b + plot_annotation(tag_levels = 'A') + plot_layout(widths = c(4,5))
# Combine 'a' (scatterplot with regression lines) and 'b' (scatterplot with marginal histograms)
# Use plot_annotation for tagging subplots, and set layout widths for each plot
