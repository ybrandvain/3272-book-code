# Skeleton code for hyena homework
library(stringr)
library(dplyr)
library(readr)

# Load the data
link <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12q23HyenaGiggles.csv"
hyenas <- read_csv(link) %>%
  rename_all(str_remove, "IndividualGiggleVariation") %>% 
  mutate(subord_minus_dom = subordinate - dominant)

# To calculate the following, fill in the blanks:
  # - The number of pairs (n)
  # - The degrees of freedom (df)
  # - The mean difference between subordinate and dominant (mean_diff)
  # - The standard deviation of the differences (sd_diff)
  # - Cohen’s d (cohens_d)
  # - The standard error (se_diff)
  # - The t-value (t)

hyenas %>%
  summarise(
    n = n(),
    df = ___,
    mean_diff = mean(subord_minus_dom),
    sd_diff = __(__),
    cohens_d = __ / __,
    se_diff = __ / __,
    t = __ / __
  )

# For this two-tailed, 95% confidence interval, the critical t-value is:
_(p = __, df = __, lower.tail = __)

# Use pt() to find the p-value:
__ * pt(abs(__), df = __, lower.tail = __)

# Confirm your p-value with the t.test function:
t.test(x = pull(hyenas, __), mu = __)
           # Or use the paired option for a direct comparison:
t.test(x = pull(hyenas, __), y = pull(hyenas, __), paired = TRUE)
