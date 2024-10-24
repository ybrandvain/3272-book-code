library(dplyr)
library(readr)
library(ggplot2)

frogs <- read_csv("https://raw.githubusercontent.com/ybrandvain/biostat/master/data/Swierk_Langkilde_BEHECO_1.csv")

ggplot(frogs, aes( sample = hatched.eggs,  color =  treatment)) +
  geom_qq(show.legend = FALSE)+
  geom_qq_line(show.legend = FALSE)+
  facet_wrap(~treatment)
