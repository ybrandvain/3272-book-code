library(readr)
library(dplyr)
library(ggplot2)
library(broom)
crab <- read_csv("https://raw.githubusercontent.com/ybrandvain/datasets/refs/heads/master/crabs.csv")


### PLOT 1
ggplot(crab , aes(x= bodyTemperature))+
    geom_qq()+
    geom_qq_line()+
    facet_wrap(~crabType)
    
### PLOT 2
ggplot(crab , aes(x= bodyTemperature))+
    geom_qq()+
    geom_qq_line()
    
### PLOT 3
lm(bodyTemperature ~ crabType, data = crab) %>%
    augment()%>%
    ggplot(., aes(x= .fitted))+
    geom_qq()+
    geom_qq_line()+
    facet_wrap(~crabType)
    
### PLOT 4
lm(bodyTemperature ~ crabType, data = crab) %>%
    augment()%>%
    ggplot(., aes(x= .resid))+
    geom_qq()+
    geom_qq_line()+
    facet_wrap(~crabType)
    
    
        
    
    
