library(ggplot2)           # ggplot2 for creating visualizations, 
library(dplyr)             # dplyr for data manipulation, 
library(palmerpenguins)    # palmerpenguins for the dataset


perm_slope <- replicate(1000, simplify = FALSE, {
  penguins %>%
    filter(!is.na(flipper_length_mm + body_mass_g)) %>%
    mutate(perm_body_mass_g =  sample(body_mass_g, replace = FALSE)) %>%
    select(species, flipper_length_mm, perm_body_mass_g)}) %>%
  bind_rows(.id = "replicate_id") %>%
  group_by(replicate_id) %>%
  summarise(cov_perm = cov(perm_body_mass_g, flipper_length_mm),
         r_perm   = cor(perm_body_mass_g, flipper_length_mm),
         b_perm   = cov(perm_body_mass_g, flipper_length_mm) / var(perm_body_mass_g))%>%
  ungroup()

ggplot(perm_slope, aes(x =b_perm  ))+
  geom_histogram() + 
  geom_vline(data = . %>% reframe(b_perm= quantile(b_perm, c(0.025,0.975))),
             aes(xintercept = b_perm), color = "red",lty = 2)+
  geom_vline(xintercept = 1.528e-02,color = "purple")+
  labs(title = "The permuted distribution of slopes",subtitle = "Flipper length as a function of body mass")+
  annotate(geom = "text", color = "white", x = 0, y = 100, label = "permuted\ndistribution")+
  annotate(geom = "text", color = "purple", x = 0.014, y = 100, label = "observed\nslope")
