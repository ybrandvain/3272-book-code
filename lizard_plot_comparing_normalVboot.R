library(readr)
library(dplyr)
library(ggplot2)

# Get the data from the provided URL and remove any rows with missing values
lizards <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12e3HornedLizards.csv") %>% 
  na.omit()

#### Get the summaries
lizard_summaries <- lizards %>%
  group_by(Survival) %>%
  summarise(n = n(),                          # Sample size for each group
            df = n - 1,                       # Degrees of freedom for each group
            mean_horn = mean(squamosalHornLength),  # Mean horn length for each group
            var_horn = var(squamosalHornLength))    # Variance of horn length for each group

### Calculate pooled variance
lizard_summaries_2 <- lizard_summaries %>% 
  summarise(pooled_variance = sum(df * var_horn) / sum(df), # Calculate the pooled variance
            mean_horn_diff = diff(mean_horn))               # Calculate the difference in means between groups

### Bootstrapping to estimate the distribution of the difference in means
lizard_boot <- replicate(n = 10000, simplify = FALSE,
                         lizards %>%
                           group_by(Survival)                     %>%
                           slice_sample(prop = 1, replace = TRUE) %>%
                           summarise(mean_horn = mean(squamosalHornLength)) %>%
                           summarise(mean_horn_diff = diff(mean_horn))) %>%
  bind_rows()

#### Estimating uncertainty using confidence intervals
alpha <- 0.05
lizard_summaries_3 <- lizard_summaries %>%
  summarise(est_diff        = abs(diff(mean_horn)),             # Estimated difference in means
            pooled_variance = sum(df * var_horn) / sum(df),     # Pooled variance
            se              = sqrt(sum(pooled_variance/n)),     # Standard error
            crit_t_95       = qt(p = alpha/2, df = sum(df), lower.tail = FALSE), # Critical t-value for 95% confidence interval
            lower_95CI      = est_diff - se * crit_t_95,        # Lower bound of the 95% confidence interval
            upper_95CI      = est_diff + se * crit_t_95)        # Upper bound of the 95% confidence interval

lizard_summaries_3

### Plot comparing the sampling distribution estimated by bootstrapping to a normal distribution
ggplot(lizard_boot, aes(x = mean_horn_diff)) + 
  geom_histogram(aes(y =..density..), colour = "white", fill = "black") +
  stat_function(fun = dnorm, color = "red", linewidth = 2,
                args = list(mean = pull(lizard_summaries_3, est_diff), 
                            sd = pull(lizard_summaries_3, se))) +
  annotate(geom = "label", x = 2.3, y = 0.2, label = "Bootstrap\ndistribution\nof means.", size = 6) +
  annotate(geom = "label", x = 3.5, y = 0.6, label = "Normal distribution\nmean = est_diff = 2.29\nsd = se_est_diff = 0.528.", size = 6, color = "red")
