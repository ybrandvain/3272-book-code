library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)  # For combining multiple plots

# Set degrees of freedom for the t-distribution
df <- 182

# Generate a tibble of x values and their corresponding t-distribution density values
a <- tibble(x = seq(-5, 5, .001), y = dt(x = x, df = df)) %>% 
  mutate(l = case_when( 
    x > 4.35 ~ "f1",      # Label values greater than the observed t-value as "f1"
    x < -4.35 ~ "f2",     # Label values less than the negative observed t-value as "f2"
    TRUE ~ "a"            # All other values are labeled as "a"
  )) %>%
  # Create a plot showing the t-distribution and highlight the areas more extreme than the observed t-value
  ggplot(aes(x = x, y = y, fill = l)) +
  geom_area(color = "black") +  # Fill the area under the curve, outlining with black
  scale_fill_manual(values = c("white", "blue", "blue")) +  # White for the non-extreme areas, blue for the extreme ones
  theme(legend.position = "none") +  # Remove the legend
  geom_vline(xintercept = c(-1, 1) * qt(p = 0.025, df = df), color = "red", lty = 2) +  # Red dashed lines for critical t-values at alpha = 0.05
  labs(x = "t-value", y = "Probability Density", 
       title = "Sampling distribution for t with 182 df", 
       subtitle = expression(paste("Red lines show critical t for ", alpha, " = 0.05, df = 182")))

# Zoom in on a smaller area of the t-distribution for better visualization
b <- tibble(x = seq(-5, 5, .0002), y = dt(x = x, df = df)) %>% 
  mutate(l = case_when(
    x > 4.3 ~ "f1",      # Label values more extreme than 4.3 as "f1"
    x < -4.3 ~ "f2",     # Label values less extreme than -4.3 as "f2"
    TRUE ~ "a"           # All other values are labeled as "a"
  )) %>%
  ggplot(aes(x = x, y = y, fill = l)) +
  geom_area(color = NA) +  # No outline for the area under the curve in the zoomed plot
  scale_fill_manual(values = c("white", "blue", "blue")) +  # White for non-extreme areas, blue for extreme ones
  theme(legend.position = "none") +  # Remove the legend
  geom_vline(xintercept = c(-1, 1) * qt(p = 0.025, df = df), color = "red", lty = 2) +  # Red dashed lines for critical t-values
  labs(x = "t-value", y = "Probability Density", title = "Zoom in", 
       subtitle = "Blue areas are as or more extreme than our t-value") +
  ylim(c(0, .0002))  # Set the y-axis limits for the zoomed plot

a+b
