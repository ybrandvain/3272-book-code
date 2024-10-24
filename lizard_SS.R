# Load necessary libraries
library(readr)         # For reading CSV files
library(dplyr)         # For data manipulation
library(ggplot2)       # For creating plots
library(patchwork)     # For combining multiple plots

# Read in the lizard dataset from an online source and remove any rows with missing values
lizards <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12e3HornedLizards.csv") %>% 
  na.omit()

# Create a modified dataset with additional columns for sum of squares calculations
lizard_SS <- lizards %>%
  group_by(Survival) %>%                                # Group by the 'Survival' variable
  mutate(prediction = mean(squamosalHornLength)) %>%    # Calculate the mean squamosal horn length for each group as 'prediction'
  ungroup() %>%
  mutate(id = 1:n()) %>%                                # Add a unique 'id' for each row
  mutate(
    grand_mean     = mean(squamosalHornLength),         # Calculate the overall mean (grand mean) of squamosal horn length
    total_error    = squamosalHornLength - grand_mean,  # Total deviation: the difference between the observed values and the grand mean
    model_explains = prediction - grand_mean,           # Model-explained deviation: difference between group mean and grand mean
    residual_error = squamosalHornLength - prediction   # Residual deviation: difference between observed values and group mean
  )

# Plot A: Show total sum of squares (SS_total), i.e., deviations from the grand mean
a <- ggplot(lizard_SS, aes(x = id, xend = id, y = squamosalHornLength, yend = grand_mean)) +
  geom_point() +                                      # Plot points for each squamosal horn length
  geom_segment(linewidth = .2, color = "brown4") +    # Draw lines connecting points to the grand mean (representing total error)
  labs(y = "Squamosal Horn Length", title = "(A) SS_total    =") +  # Add labels
  facet_wrap(~Survival, ncol = 2, scales = "free_x") + # Create facets by 'Survival' with free x-scales
  theme(
    plot.title = element_text(size = 20, color = "brown4"),  # Customize title appearance
    axis.title = element_text(size = 20),                    # Customize axis labels
    strip.text = element_text(size = 20)                     # Customize facet strip text
  )

# Plot B: Show sum of squares explained by the model (SS_model)
b <- ggplot(lizard_SS, aes(x = id, xend = id, y = squamosalHornLength, yend = grand_mean)) +
  geom_point() +                                         # Plot points for each squamosal horn length
  geom_segment(aes(y = prediction), linewidth = .2, color = "blue") +  # Connect points to the group mean (prediction)
  labs(y = "Squamosal Horn Length", title = "(B) SS_model    +") +     # Add labels
  facet_wrap(~Survival, ncol = 2, scales = "free_x") +                  # Facet by 'Survival'
  theme(
    axis.text.y = element_blank(),                         # Remove y-axis text
    axis.title.y = element_blank(),                        # Remove y-axis label
    plot.title = element_text(size = 20, color = "blue"),  # Customize title appearance
    axis.title = element_text(size = 20),                  # Customize axis labels
    strip.text = element_text(size = 20)                   # Customize facet strip text
  )

# Plot C: Show residual sum of squares (SS_error), i.e., deviations from group means
c <- ggplot(lizard_SS, aes(x = id, xend = id, y = squamosalHornLength, yend = prediction)) +
  geom_point() +                                        # Plot points for each squamosal horn length
  geom_segment(linewidth = .2, color = "orange") +      # Draw lines connecting points to the prediction (residual error)
  labs(y = "Squamosal Horn Length", title = "(C) SS_error") +   # Add labels
  facet_wrap(~Survival, ncol = 2, scales = "free_x") +  # Facet by 'Survival'
  theme(
    axis.text.y = element_blank(),                      # Remove y-axis text
    axis.title.y = element_blank(),                     # Remove y-axis label
    plot.title = element_text(size = 20, color = "orange"),  # Customize title appearance
    axis.title = element_text(size = 20),               # Customize axis labels
    strip.text = element_text(size = 20)                # Customize facet strip text
  )

# Combine the three plots (SS_total, SS_model, SS_error) using patchwork
a + b + c
