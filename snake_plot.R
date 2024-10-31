library(dplyr)
library(readr)
library(ggplot2)

snake_data <-  read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17q11RattlesnakeDigestion.csv")%>% 
  rename(meal_size = mealSize, body_temp = tempChange)

ggplot(snake_data, aes(x = meal_size, y = body_temp)) + 
  geom_point(alpha = .5, size =3)+
  labs(y = "\n\n temp change", x = "meal size")+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 15))+
  geom_smooth(method = "lm")
