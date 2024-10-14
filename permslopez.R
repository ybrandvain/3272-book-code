library(ggplot2)           # Load ggplot2 for creating visualizations
library(dplyr)             # Load dplyr for data manipulation
library(palmerpenguins)    # Load palmerpenguins for the dataset

# Filter the penguin dataset to include only Adelie and Chinstrap species, 
# remove rows with missing values for flipper length and body mass, and select relevant columns
focal_penguins <- penguins %>%
  filter(species %in% c("Adelie", "Chinstrap")) %>%
  filter(!is.na(flipper_length_mm + body_mass_g)) %>%
  select(species, flipper_length_mm, body_mass_g)

# Define a function to Z-transform data (standardize: subtract mean and divide by standard deviation)
Ztrans <- function(x) { (x - mean(x)) / sd(x) }

# Apply Z-transformation to flipper length and body mass, grouped by species
z_focal_penguins <- focal_penguins %>%
  group_by(species) %>%
  mutate_all(Ztrans)

# Perform 1000 permutations to create a null distribution of the difference in slopes between species
# Shuffle species labels and calculate the slope for each permutation
perm_dist_z <- replicate(1000, simplify = FALSE,
          z_focal_penguins %>%
            ungroup() %>%                        # Ungroup to avoid issues with group-wise resampling
            mutate(species = sample(species, replace = FALSE)) %>% # Shuffle species labels randomly without replacement
            group_by(species) %>%                # Group by species again after shuffling
            summarise(slope = cov(flipper_length_mm, body_mass_g) / var(body_mass_g)) %>%  # Calculate slope for each species
            summarise(perm_diff_slope = diff(slope))  # Compute the difference in slopes between the species
          ) %>%
  bind_rows()  # Combine the results from all 1000 permutations into a single data frame

# Plot the permuted distribution of the difference in slopes
ggplot(perm_dist_z, aes(x = perm_diff_slope)) +
  geom_histogram() +                                  # Create a histogram of the permuted slope differences
  geom_vline(data = . %>%                             # Add vertical lines for the 2.5% and 97.5% quantiles (95% confidence interval)
               reframe(perm_diff_slope = quantile(perm_diff_slope, c(0.025, 0.975))),
             aes(xintercept = perm_diff_slope), color = "red", lty = 2) +  # Red dashed lines for the confidence intervals
  geom_vline(xintercept = 0.1734, color = "purple") +  # Add a vertical line at the observed slope difference (0.1734)
  labs(title = "The permuted distribution of difference in slopes",        # Add a title
       subtitle = "Flipper length as a function of body mass (both Z-transformed)") +  # Add a subtitle describing the transformation
  annotate(geom = "label", color = "black", x = 0, y = 50, label = "permuted\ndistribution", size = 6) + # Add a label for permuted distribution
  annotate(geom = "label", color = "purple", x = 0.1734, y = 50, label = "observed\nslope", size = 6)  # Add a label for the observed slope
