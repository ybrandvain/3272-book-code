# The primary goal of this code is to illustrate the effect of sample size on the p-value distribution and the probability of rejecting the null hypothesis (power). 
# The simulation generates p-values from t-tests for varying sample sizes and compares two scenarios:
     # 1. Null is True: Simulating data where the true mean is zero, matching the null hypothesis.
     # 2. Null is False: Simulating data where the true mean is 0.3, providing an effect size to test against the null hypothesis.
#The code then produces two key visualizations:
     # 1. P-value distribution: This plot shows how the distribution of p-values changes depending on sample size and whether the null hypothesis is true or false.
     # 2. Power/rejection probability plot: This plot shows how the probability of rejecting the null hypothesis (i.e., obtaining p-values less than 0.05) changes as the sample size increases, for both true and false null hypotheses.
# The goal is to demonstrate how increasing sample size improves statistical power and leads to more p-values below the significance threshold when the null hypothesis is false. 
# Conversely, it shows that when the null is true, the rate of p-values below 0.05 remains constant at the significance level (alpha), regardless of sample size.
# These visualizations help illustrate key statistical concepts such as the importance of sample size in hypothesis testing and how statistical power changes as sample size increases.



# Load the packages
library(ggplot2)
library(dplyr)
library(tibble)
library(cowplot)  # For plot_grid
library(ggthemes)
# Define sample sizes as powers of 2 (from 2^1 to 2^9, or 2 to 512)
sample.size <- c(2^(1:9))

# Simulate p-values from t-tests for varying sample sizes and calculate the proportion of p-values below alpha (0.05)
sim <- tibble(p_reject = 
                c(sapply(sample.size , function(X){
                  # Null is False: Simulate data with true mean of 0.3, perform t-test, and check if p-value < 0.05
                  mean(replicate(1000, unlist(t.test(rnorm(X), mu = 0.3)["p.value"]) < 0.05))}),
                  sapply(sample.size , function(X){
                    # Null is True: Simulate data with true mean of 0, perform t-test, and check if p-value < 0.05
                    mean(replicate(1000, unlist(t.test(rnorm(X), mu = 0)["p.value"]) < 0.05))}))) %>%
  # Create a column indicating whether the null hypothesis is true or false
  mutate(H_0 = factor(rep(c("Null is False", "Null is True"), each = length(sample.size)), 
                      levels = c("Null is True", "Null is False"))) %>%
  # Replicate sample sizes for both true and false null hypotheses
  mutate(sample.size = rep(sample.size, 2))

# Create plot to show proportion of rejections (p-values < alpha) for both null true and false scenarios
acceptreject <- ggplot(sim, aes(x = sample.size, y = p_reject)) +
  geom_point(show.legend = FALSE) +  # Plot points for each sample size and p_reject value
  geom_smooth(se = FALSE, show.legend = FALSE) +  # Add a smoothed line to visualize the trend
  facet_wrap(~H_0) +  # Create separate plots for "Null is True" and "Null is False"
  scale_x_continuous(trans = "log2") +  # Use log scale for sample size
  geom_hline(yintercept = 0.05, color = "purple") +  # Add a horizontal line at alpha = 0.05
  theme_tufte() +  # Apply a minimalistic theme
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0.05, 0.25, 0.5, 0.75, 1)) +  # Set y-axis limits
  ylab(expression(P~'('~reject~null~'|'~alpha~""==0.05~')')) +  # Set y-axis label
  ggtitle("P(reject the null)") +  # Set plot title
  annotation_logticks(sides = "b", base = 10, size = 0.2) +  # Add log ticks on the x-axis
  theme(strip.text = element_text(size = 15, color = "blue"),  # Customize facet strip text
        strip.background = element_rect(colour = "grey", fill = "grey"),  # Customize facet strip background
        axis.line = element_line(colour = "black"),  # Customize axis lines
        panel.border = element_rect(colour = "black", fill = NA)) +  # Add black border around the panel
  geom_hline(yintercept = seq(0, 0.75, 1/4), color = "lightgrey")  # Add light grey gridlines

# Simulate p-value distributions for different sample sizes under both null true and false conditions
p.vals <- bind_rows(
  # Null is False: Simulate p-values for different sample sizes with a true mean of 0.3
  tibble(n = rep(sample.size, each = 1000), 
         p = c(sapply(sample.size, function(X) {
           replicate(1000, unlist(t.test(rnorm(X), mu = 0.3)["p.value"]))})),
         H0 = "Null is False"),
  # Null is True: Simulate p-values for different sample sizes with a true mean of 0
  tibble(n = rep(sample.size, each = 1000), 
         p = c(sapply(sample.size, function(X) {
           replicate(1000, unlist(t.test(rnorm(X), mu = 0)["p.value"]))})),
         H0 = "Null is True")) %>%
  mutate(H0 = factor(H0, levels = c("Null is True", "Null is False")))

# Plot cumulative frequency distribution of p-values for different sample sizes
pvalsfig <- ggplot(p.vals %>% mutate(`sample size` = factor(n)), aes(x = p, color = `sample size`)) +
  stat_ecdf(linewidth = 1) +  # Plot the empirical cumulative distribution function
  facet_wrap(~H0) +  # Create separate plots for "Null is True" and "Null is False"
  scale_color_brewer(type = "seq", palette = "RdPu") +  # Use sequential color palette
  xlim(c(0, 1)) + theme_light() +  # Limit x-axis to [0, 1]
  labs(title = "P-value distribution", y = "Cumulative frequency", x = "P-value") +  # Set axis labels and title
  theme_tufte() +  # Apply a minimalistic theme
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0.05, 0.25, 0.5, 0.75, 1), 
                     labels = c(".05", "0.25", "0.50", "0.75", "1")) +  # Set x-axis breaks
  geom_vline(xintercept = 0.05, color = "purple") +  # Add vertical line at alpha = 0.05
  theme(strip.text = element_text(size = 15, color = "blue"),  # Customize facet strip text
        strip.background = element_rect(colour = "grey", fill = "grey"),  # Customize facet strip background
        axis.line = element_line(colour = "black"),  # Customize axis lines
        panel.border = element_rect(colour = "black", fill = NA)) +  # Add black border around the panel
  geom_hline(yintercept = seq(0, 0.75, 1/4), color = "lightgrey")  # Add light grey gridlines

# Combine the two plots (p-value distribution and rejection probability) in a grid
top <- plot_grid(pvalsfig + theme(legend.position = "none"), get_legend(pvalsfig), rel_widths = c(5, 1), ncol = 2)
bottom <- plot_grid(acceptreject, NULL, ncol = 2, rel_widths = c(5, 1))

# Display the combined plot with two rows (a) p-value distribution and (b) rejection probability
plot_grid(top, bottom, nrow = 2, labels = c("a", "b"))
