library(dplyr)
library(forcats)
library(readr)
library(ggplot2)
crabs <- read_csv("https://raw.githubusercontent.com/ybrandvain/datasets/refs/heads/master/crabs.csv")

crabs <- crabs %>% 
  na.omit()%>%
  mutate(crabType = fct_relevel(crabType,   "female",   "male major removed", "male minor removed",  "intact male"))

ggplot(data= crabs, aes(x = crabType,  y = bodyTemperature, color = crabType))+
  geom_jitter(width = .2, size = 3, alpha = .7, show.legend = FALSE)+
  stat_summary(fun.data=mean_cl_normal, geom ="errorbar", width =.2, position = position_nudge(x=.4))+
  geom_text(data = tibble(crabType = c("female","male minor removed"), sig_group = c("A","C")),
              aes(y = 2.2,label = sig_group), size = 8)+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))+
  labs(x = "crab type", y = "body temperature")

aov(bodyTemperature ~ crabType, data = crabs) %>%
  TukeyHSD()
