library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)

link <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12q29goatCalls.csv"
goats <- read_csv(link)

### Plot
goats %>%
  pivot_longer(cols = contains("kid"))%>%
  ggplot(aes(x = name, y = value))+
  geom_jitter(width = .1)+
#  stat_summary(fun.data = mean_cl_normal)
  stat_summary(aes(group = motherGoat), geom = "line")


### Paired t-test is one sample t-tes on the difference
goats <- goats %>%
  mutate(diff_kid = scoreFormerKid - scoreUnrelatedKid)

goats %>%
  summarise(sd = sd(diff_kid),
            se = sd/sqrt(n()),
            mean_diff_kid = mean(diff_kid),,
            t  = (mean_diff_kid- 0)/se,
            df =n()-1,
            crit_val = qt(p = .025,df=df,lower.tail = FALSE),
            lower_CI = mean_diff_kid  -  crit_val*se,
            upper_CI = mean_diff_kid  +  crit_val*se,
            p = 2*pt(abs(t), df = df, lower.tail = FALSE))

t.test(goats$scoreFormerKid, goats$scoreUnrelatedKid, paired = TRUE)
t.test(pull(goats,diff_kid), mu=0)
