library(ggplot2)           # ggplot2 for creating visualizations, 
library(dplyr)             # dplyr for data manipulation, 
library(palmerpenguins)    # palmerpenguins for the dataset

boot_slope <- replicate(1000, simplify = FALSE, {
  penguins %>% 
    filter(!is.na(flipper_length_mm + body_mass_g)) %>%   # Filter out rows with missing values in flipper length or body mass
    slice_sample(prop = 1, replace = TRUE) %>%            # Perform bootstrap sampling (resample with replacement)
    summarise(b_boot = cov(body_mass_g, flipper_length_mm) / var(body_mass_g))  # Calculate the slope of the relationship using covariance/variance
}) %>% 
  bind_rows()  # Combine the results from all bootstrap samples into one data frame

# Create a histogram of the bootstrap distribution of the slopes
ggplot(boot_slope, aes(x = b_boot)) +  
  geom_histogram() +                                      # Plot a histogram of the bootstrapped slopes
  geom_vline(data = . %>% 
               reframe(b_boot = quantile(b_boot, c(0.025, 0.975))),  # Add vertical lines at the 2.5% and 97.5% quantiles (confidence intervals)
             aes(xintercept = b_boot), color = "red", lty = 2) +     # Red dashed lines for the quantiles
  labs(title = "The bootstrap distribution of slopes",                 # Add a title
       subtitle = "Flipper length as a function of body mass")         # Add a subtitle describing the relationship being examined
