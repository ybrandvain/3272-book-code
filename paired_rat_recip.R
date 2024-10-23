# Load necessary libraries
library(readr)      # For reading data from a CSV file
library(ggplot2)    # For creating visualizations
library(dplyr)      # For data manipulation
library(tidyr)


rat_link <- "http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q31RatReciprocity.csv"
rat_dat  <- read_csv(rat_link) %>%
  mutate(help_minus_nohelp = AfterHelp - AfterNoHelp,
         sign = case_when(help_minus_nohelp == 0 ~ "0",
                          help_minus_nohelp > 0   ~ "+",
                          help_minus_nohelp < 0   ~ "-"))

# Making some plots!
long_rat <-  rat_dat %>%
  pivot_longer(cols = contains("Help", ignore.case = FALSE),
               names_to = "treatment", values_to = "help_other", names_prefix = "After")

ggplot(long_rat,  aes(x = treatment, y = help_other, group = focalRat, color = sign)) +
  geom_point(size = 3, alpha = .4) +
  geom_line(alpha = .5)
