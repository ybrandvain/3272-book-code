library(ggplot2)           # ggplot2 for creating visualizations
library(dplyr)             # dplyr for data manipulation
library(palmerpenguins)    # palmerpenguins for the penguin dataset

# Perform 1000 bootstrap replications
boot_slope_diff <- replicate(1000, simplify = FALSE,
          z_focal_penguins %>%                     # Use the Z-transformed penguin data
            group_by(species) %>%                  # Group by species
            slice_sample(prop = 1, replace = TRUE) %>%  # Resample each group with replacement (bootstrap)
            summarise(slope = cov(flipper_length_mm, body_mass_g) / var(body_mass_g)) %>%  # Calculate slope for each bootstrap sample
            summarise(diff_slope = diff(slope))     # Calculate the difference in slopes between the species
          ) %>%
  bind_rows()  # Combine the results into a single data frame
  
# Create a histogram of the bootstrap distribution of the difference in slopes
ggplot(boot_slope_diff, aes(x = diff_slope)) +
  geom_histogram() +                              # Plot a histogram of the slope differences
  geom_vline(data = . %>%                         # Add vertical lines at the 2.5% and 97.5% quantiles (bootstrap confidence intervals)
               reframe(diff_slope = quantile(diff_slope, prob = c(0.025, 0.975))),
             aes(xintercept = diff_slope), color = "red", lty = 2) +  # Red dashed lines for the quantiles
  labs(title = "Bootstrap distribution of the difference in slopes")  # Add a title to the plot
