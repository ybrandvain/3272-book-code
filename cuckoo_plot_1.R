# Load necessary libraries
library(readr)      # For reading CSV files
library(forcats)    # For handling and reordering factor levels
library(dplyr)      # For data manipulation
library(ggplot2)    # For creating plots

# Read in the data and reorder host_species by egg_length
cuckoo_eggs <- read_csv("https://raw.githubusercontent.com/ybrandvain/datasets/refs/heads/master/cuckooeggs.csv") %>%
  mutate(host_species = fct_reorder(host_species, egg_length))

# Create a scatter plot with jittered points and error bars
ggplot(cuckoo_eggs, aes(x = host_species, y = egg_length, color = host_species)) +
  geom_jitter(height = 0, width = .2, size = 3, alpha = .65, show.legend = FALSE) +  # Add jittered points to reduce overlap
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .2, position = position_nudge(x = .35)) +  # Add error bars for the mean and confidence intervals
  theme(axis.title = element_text(size = 15),  # Adjust axis title size
        axis.text = element_text(size = 12),   # Adjust axis text size
        legend.position = "NA") +              # Hide legend
  labs(x = "Host Species", y = "Egg length")   # Add axis labels
