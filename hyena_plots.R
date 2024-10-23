# Load necessary libraries
library(readr)       # For reading CSV files
library(ggplot2)     # For creating visualizations
library(tidyr)       # For data tidying (reshaping data)
library(dplyr)       # For data manipulation
library(patchwork)   # For combining ggplot2 plots
library(ggrepel)     # For repelling text labels in plots (you need this for geom_text_repel)
library(ggthemes)

# Define the URL to the CSV data file
link <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12q23HyenaGiggles.csv"

# Read the CSV file and clean up column names (removing "IndividualGiggleVariation" from column names)
# Also create new variables: 'subord_minus_dom' (difference between subordinate and dominant giggles)
# and 'sign' (indicates whether the difference is positive or negative)
hyenas <-  read_csv(link) %>%
  rename_all(str_remove, "IndividualGiggleVariation") %>%
  mutate(subord_minus_dom = subordinate - dominant,
         sign = case_when(subord_minus_dom > 0 ~ "+",  # Positive sign if subordinate giggles more
                          subord_minus_dom < 0 ~ "-")) # Negative sign if dominant giggles more

# Reshape data to long format for plotting, with columns for 'dom' (dominance status) and 'giggles' (giggle counts)
long_hyenas <-  hyenas %>% 
  pivot_longer(cols = c("subordinate", "dominant"), names_to = "dom", values_to = "giggles")

# Create a slope graph showing the change in giggles between subordinate and dominant pairs
# The lines connect the subordinate and dominant giggle counts for each pair, colored by 'sign'
slope_o_graph <- ggplot(long_hyenas, aes(x = dom, y = giggles, group = pair, 
                                         label = pair, color = sign)) +
  geom_text_repel(nudge_x = c(.1, -.1), show.legend = FALSE, size = 6, segment.colour = NA) + # Label pairs
  geom_line() +   # Add lines connecting subordinate and dominant giggles
  theme_clean() +  # Apply a minimalistic theme
  theme(axis.text.x = element_text(size = 12),  # Customize axis text and labels
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 15),
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.line.x = element_blank(),  # Remove x-axis line
        axis.line.y = element_blank(),  # Remove y-axis line
        panel.background = element_rect(fill = "grey98", color = "white"),  # Light background
        plot.background = element_blank()) +  # No outer background
  labs(x = "Dominance") +  # Label for x-axis
  theme(legend.position = "none")  # No legend

# Create a jitter plot showing the differences in giggles between subordinates and dominants
# Each point represents the difference in giggles, colored by 'sign'
diff_graph <- ggplot(hyenas, aes(x = 0, y = subord_minus_dom)) +
  geom_jitter(aes(color = sign), height = 0, width = .03, size = 5, alpha = .7, show.legend = FALSE) + # Jittered points
  geom_hline(yintercept = 0) +  # Horizontal line at y = 0 to represent no difference
  stat_summary(fun.data = "mean_cl_normal") +  # Add summary statistics (mean and confidence interval)
  theme_clean() +  # Apply a minimalistic theme
  theme(axis.text.x = element_blank(),  # Customize axis text and labels
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 15),
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.line.x = element_blank(),  # Remove x-axis line
        axis.line.y = element_blank(),  # Remove y-axis line
        panel.background = element_rect(fill="grey98", color = "white"),  # Light background
        plot.background = element_blank()) +  # No outer background
  labs(y = "Difference in giggling\n(Subordinate minus dominant)", x = "random jitter to show points") +  # Axis labels
  coord_cartesian(ylim = c(-.21, .21), xlim = c(-.14, .1)) +  # Set limits for the axes
  geom_label(data = tibble(sign = c("-", "+"),   # Add text labels indicating what the positive/negative signs mean
                           x = -.1, 
                           subord_minus_dom = c(-.1, .1),
                           label = c("Dominant\nlaughs\nmore", "Subordinate\nlaughs\nmore")),
             aes(label = label, x = x, color = sign), size = 6, show.legend = FALSE)

# Combine the two plots (slope graph and difference graph) side by side using patchwork
slope_o_graph + diff_graph
