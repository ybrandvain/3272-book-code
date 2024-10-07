##############
Goals:
Illustrate Hypothesis Testing: This code visualizes two scenarios in hypothesis testing — one where a test statistic is not extreme enough to reject the null hypothesis and another where the test statistic is extreme enough to reject it.
##P-value Interpretation: The code calculates and displays p-values for both cases, providing a concrete example of how the areas in the null distribution correspond to p-values.
#Teach Sampling Distributions: By comparing the test statistics against the null distribution, this visualization helps to explain the concept of sampling variability and how it influences decision-making in hypothesis testing.
##############


###########################################################################################
############# WARNING  THIS TAKES A WHILE AND A BUNCH OF MEMORY ###########################
###########################################################################################



# Load the necessary libraries
library(ggplot2)   # For data visualization
library(dplyr)     # For data manipulation
library(patchwork) # For combining multiple plots

# Create a base ggplot object `a` representing the null sampling distribution of the test statistic
a <- ggplot(data = sample.dist, aes(x = estimate)) +
  # ylab("") + 
  xlab("Estimates from sampling dist.") +   # Label the x-axis
  annotate(geom = "text", x = .575, y = .023, label = "Estimate from \nmy sample", color = "steelblue") +
  labs(title = "A null distribution", y = "", x= "test statistic") # Title of the plot and axis labels

# Define two test statistics: one unremarkable and one exciting
unremarkable_teststat <- 0.2
exciting_teststat <- 0.5

# Summarize areas under the curve to calculate p-values for both test statistics
my.ps <- sample.dist %>% 
  dplyr::summarise(lower.area.2 = round(mean( estimate <= -unremarkable_teststat), digits = 3),  # Area on the left of -0.2
            upper.area.2 =  round(mean( estimate >= unremarkable_teststat), digits = 3),          # Area on the right of 0.2
            p.val.2 = lower.area.2 + upper.area.2,                                                # Total p-value for 0.2

            lower.area.7 = round(mean( estimate <= -exciting_teststat ), digits = 3),             # Area on the left of -0.5
            upper.area.7 =  round(mean( estimate >= exciting_teststat ), digits = 3),             # Area on the right of 0.5
            p.val.7 = lower.area.7 + upper.area.7) %>%                                            # Total p-value for 0.5
  unlist()

# Plot for the unremarkable test statistic (fail to reject null hypothesis)
fail_to_reject <- a +  
  geom_histogram(aes(alpha = abs(estimate) >= unremarkable_teststat,  # Highlight values beyond test statistic
                     fill  = abs(estimate) >= unremarkable_teststat,  # Color based on the test statistic
                     y = ..count../sum(..count..)),                   # Normalize the y-axis to represent probability
                 bins = 100, color = "white", lwd = .1,               # Histogram properties (100 bins, white borders)
                 show.legend = FALSE) +
  scale_alpha_manual(values = c(1, .3)) +                             # Control transparency for highlights
  scale_fill_manual(values = c("lightsalmon", "steelblue")) +         # Colors for inside/outside the test stat range
  geom_segment(aes(x = .55, xend = unremarkable_teststat, y = .018, yend = 0), 
               size = 1, color = "steelblue", arrow = arrow(length = unit(0.1, "npc"))) + 
  annotate(geom = "text", x = c(-.8, .8), y = c(.01, .01),            # Annotate areas under the curve with p-values
             label = c(sprintf("Area\n%s", my.ps["lower.area.2"]), sprintf("Area\n%s", my.ps["upper.area.2"])), 
             color = "steelblue") +  
  labs(title = "Unremarkable test statistic",                         # Title and subtitle with test stat and p-value
       subtitle = sprintf("Test Stat = %s; P-value = %s", unremarkable_teststat, my.ps["p.val.2"]),
       y = "probability")

# Plot for the exciting test statistic (reject the null hypothesis)
reject <- a +  
  geom_histogram(aes(alpha = abs(estimate) >= exciting_teststat,  # Highlight values beyond test statistic
                     fill  = abs(estimate) >= exciting_teststat,  # Color based on the test statistic
                     y = ..count../sum(..count..)),               # Normalize the y-axis to represent probability
                 bins = 100, color = "white", lwd = .1,           # Histogram properties (100 bins, white borders)
                 show.legend = FALSE) +
  scale_alpha_manual(values = c(1, .3)) +                         # Control transparency for highlights
  scale_fill_manual(values = c("lightsalmon", "steelblue")) +     # Colors for inside/outside the test stat range
  geom_segment(aes(x = .55, xend = exciting_teststat, y = .018, yend = 0), 
               size = 1, color = "steelblue", arrow = arrow(length = unit(0.1, "npc"))) + 
  annotate(geom = "text", x = c(-.8, .8), y = c(.01, .01),        # Annotate areas under the curve with p-values
             label = c(sprintf("Area\n%s", my.ps["lower.area.7"]), sprintf("Area\n%s", my.ps["upper.area.7"])), 
             color = "steelblue") +  
  labs(title = "Surprising test statistic",                       # Title and subtitle with test stat and p-value
       subtitle = sprintf("Test Stat = %s; P-value = %s", exciting_teststat, my.ps["p.val.7"]),
       y = "probability") 
    
# Combine the two plots side by side and add annotation for plot panels
fail_to_reject + reject + plot_annotation(tag_levels = "a")
