# Load required libraries
library(tibble)   # For creating and working with tibbles (tidyverse data frames)
library(ggplot2)  # For creating plots

# Define the number of trials and observed successes
n_trials <- 256          # Total number of trials
n_success <- 233         # Number of successes observed

# Define the null hypothesis probability
null_p <- 1 / 2          # Null hypothesis: equal probability (e.g., fair coin)

# Calculate the expected number of successes under the null hypothesis
expected <- null_p * n_trials  

# Compute the observed difference from the null hypothesis
obs_diff_null <- abs(n_success - expected)  

# Create a null distribution tibble
lean_null_dist <- tibble(
  n_toward_equator = 0:n_trials,  # Possible number of successes (range from 0 to total trials)
  prob = dbinom(x = 0:n_trials, size = n_trials, prob = null_p),  # Binomial probabilities
  as_or_more = (abs(n_toward_equator - expected) >= obs_diff_null)  # Flag values as extreme or more
)

# Plot the null distribution
ggplot(lean_null_dist, aes(x = n_toward_equator, y = prob, fill = as_or_more)) +
  geom_col() +  # Create a bar plot
  scale_fill_manual(values = c("black", "red")) +  # Color extreme values red
  geom_vline(xintercept = n_success, color = "red")  # Add a vertical line at the observed success
