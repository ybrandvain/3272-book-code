library(dplyr)
library(readr)
library(ggplot2)
frogs <- read_csv("https://raw.githubusercontent.com/ybrandvain/biostat/master/data/Swierk_Langkilde_BEHECO_1.csv")

first_summary <- frogs     %>% 
  group_by(treatment)      %>%
  summarise(n = n(), 
            df = n - 1,
            mean_hatch = mean(hatched.eggs),
            var_hatch  = var(hatched.eggs))

print(first_summary)

first_summary %>%
  summarise(est_dif    = diff(mean_hatch),
            pooled_var = ,  # find this rom the equation in the book
            pooled_sd  = sqrt(pooled_var)
            cohensd    = ) # find this rom the equation in the book
