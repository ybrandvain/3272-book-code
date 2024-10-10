library(ggplot2)
library(dplyr)
library(palmerpenguins)


##############################
###### Subsetting data #######
##############################

# Letstake only Adelie penguins of know sex from  Torgersen
my_penguins <- penguins %>% 
  filter(species == "Adelie", !is.na(sex),  island == "Torgersen")  

# Now have a glimpse() at the my_penguins dataset
____(___)  # Your code here 


##############################
##### Visualizing data #######
##############################

ggplot(data = my_penguins, aes(x = __, y = ___, color = sex))+  # Fill in x & y
  geom____()  +                                                 # Fill in the geom and any arguments
  stat_summary(fun.data = "mean_cl_boot", color = "black")      # Add means and bootstrapped 95% CIs 


##############################
##### Summarizing data #######
##############################

# Now lets estimate the sex difference in body_mass_g (male mass - female) from the data, 
# and the standard deviation within each sex.


# first find mean body_mass_g
my_penguins %>%
  group_by(  ___ ) %>%
  summarize(mean_body = __(body_mass_g)) 

# sex diff in mean body mass
my_penguins                                %>%
  group_by(  ___ )                         %>%    # 
  summarize(mean_body = __(___)) %>%    # find mean mass for each sex and assign it to mean_bbody
  mutate(sex_diff = __(__))              # find the sex difference in mean mass

# sd of each
my_penguins %>%
  group_by(  ____ ) %>%
  summarize(sd_body = ___(___)) 





##############################
### Estimate Uncertainty #####
##############################

one_penguin_boot <- my_penguins %>%
  dplyr::select(sex, body_mass_g) %>%
  group_by(sex) %>%
  slice_sample(prop = 1, replace = TRUE)   %>% ## resample with replacement to bootstrap
  group_by(sex) %>%
  summarize(mean_body = mean(body_mass_g)) 

one_penguin_boot   ## take a minute to look at the values by sex in this bootstrap

# get the sex diffrence in body mass from this bootstrap
one_penguin_boot %>%
  summarise(sex_diff = diff(mean_body))



# Build on the code above to find the sex difference in mean body mass across 1000 bootstraps and 
# quantify uncertainty in our estimate with a standard error and 95 percent confidence interval.
penguin_boot_sexdiff <- replicate(n = n_reps, simplify = FALSE, 
                                  expr = my_penguins %>%
                                    dplyr::select(sex, body_mass_g) %>%
                                    group_by(sex) %>%
                                    slice_sample(prop = 1, replace = TRUE)   %>% ## resample with replacement to bootstrap
                                    summarize(mean_body =___(___))%>%           #find mean body_mass_g for each sex
                                    summarize(sex_diff = ___(___)))  %>%       # use the answer above to find the sex difference in body mass
  bind_rows()

penguin_boot_sexdiff  %>%
  summarise(se       = __(__),
            lower_CI = __(__, prob = __),
            upper_CI = __(__, prob = __))





##############################
# Permuting to test the null #
##############################

# One permutation
my_penguins %>%
  dplyr::select(sex, body_mass_g) %>%
  mutate(perm_sex = sample(sex, replace = FALSE))         %>% # shuffle sex to permute
  group_by(perm_sex)                                    %>% # do things by permuted sex
  summarize(mean_body = mean(body_mass_g))              %>% # find mean mass for each permuted sex
  summarize(perm_sex_diff_mass = diff(mean_body))           # find the difference in mass for permuted sex

# Now do 1000 permutations
n_reps <- 1000
penguin_perm_sexdiff <- replicate(n = n_reps, simplify = FALSE, 
                                  expr = my_penguins %>%
                                    dplyr::select(sex, body_mass_g) %>%
                                    mutate(perm_sex = __(__, replace = __))     %>% # shuffle sex to permute
                                    group_by(perm_sex)                          %>% # do things by permuted sex
                                    summarize(mean_body = __(___))              %>% # find mean mass for each permuted sex
                                    summarize(perm_sex_diff_mass = ___(___)))   %>%  # find the difference in mass for permuted
  bind_rows()

# Build on the code above to find the sex difference in mean body mass across 1000 permutations and 
# quantify how surprising this result would be if the null were true.
# Remember as.numeric(FALSE) returns 0, and as.numeric(TRUE) returns 1, 
# and the p value is the probability that a random sample from the null
# sampling distirbution is as or more extreme than our observation
p.val <- penguin_perm_sexdiff  %>%
  mutate(as_or_more_extreme =  abs(perm_sex_diff_mass ) >= abs()) %>%   # for each permutaiton, see if our observed sex difference  is as or more extreme than the actual sex diffrence of 638.9493
  summarise(p_val = ____(as.numeric(___)))
            
print(p.val)
