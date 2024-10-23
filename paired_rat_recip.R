# Load necessary libraries
library(readr)      # For reading data from a CSV file
library(ggplot2)    # For creating visualizations
library(dplyr)      # For data manipulation
library(tidyr)



# Read the rat data from a CSV file and create a new column for the difference between the "AfterHelp" 
# and "AfterNoHelp" treatments. Also, create a column 'sign' to indicate whether the difference is 
# positive, negative, or zero.
rat_link <- "http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q31RatReciprocity.csv"
rat_dat  <- read_csv(rat_link) %>%
  mutate(help_minus_nohelp = AfterHelp - AfterNoHelp,    # Calculate the difference between treatments
         sign = case_when(help_minus_nohelp == 0 ~ "0",  # Categorize the difference as 0, +, or -
                          help_minus_nohelp > 0 ~ "+",
                          help_minus_nohelp < 0 ~ "-"))

# Reshape the data to a long format for plotting, pivoting the "Help" columns into a "treatment" column,
# with corresponding "help_other" values (number of pulls for other rats). Also, create a new column 
# 'Received help' to label whether the treatment corresponds to "Help" or "NoHelp."
long_rat <- rat_dat %>%
  pivot_longer(cols = contains("Help", ignore.case = FALSE), 
               names_to = "treatment", values_to = "help_other", names_prefix = "After") %>%
  mutate(`Received help` = ifelse(treatment == "Help", "Yes", "No"))  # Label whether help was received

# Create a scatter plot with lines connecting the same rats across both treatments.
# X-axis: "Received help" (Yes/No), Y-axis: Helpfulness score (number of pulls)
# Color points and lines based on the 'sign' column, indicating whether rats helped more or less after help.
ggplot(long_rat, aes(x = `Received help`, y = help_other, group = focalRat, color = sign)) +
  geom_point(size = 4, alpha = .4) +   # Plot points, with transparency
  geom_line(alpha = .5, lty = 2) +     # Connect points for the same rat with dashed lines
  labs(y = "Helpfulness score") +      # Add y-axis label
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12))  # Customize axis text
