# Load necessary libraries
library(readr)      # For reading data from a CSV file
library(ggplot2)    # For creating visualizations
library(ggthemes)   # For additional themes in ggplot
library(dplyr)      # For data manipulation

# Load the data from the CSV file and add new variables using mutate()
range_shift_file <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter11/chap11q01RangeShiftsWithClimateChange.csv"
range_shift <- read_csv(range_shift_file) %>%
  mutate(
    x = "",                           # Create a new variable 'x' with empty strings
    uphill = elevationalRangeShift > 0 # Create a logical variable 'uphill' (TRUE for uphill shifts, FALSE for downhill)
  )

# Create a ggplot
ggplot(range_shift, aes(x = 0, y = elevationalRangeShift)) +  # Map the range shift to y-axis and set x to 0
  geom_jitter(                                           # Add jittered points to show data spread
    aes(color = uphill),                                 # Color points based on 'uphill' (TRUE/FALSE)
    width = 0.05, height = 0, size = 5, alpha = 0.7,     # Adjust jitter width, size, and transparency
    show.legend = FALSE    ) +                                # Remove the legend
  geom_hline(yintercept = 0, lty = 2) +                  # Add a horizontal dashed line at y = 0 (reference line)
  stat_summary(fun.data = "mean_cl_normal") +            # Add summary (mean and confidence interval)
  theme(axis.title.x = element_blank()) +                # Remove x-axis title
  geom_label(											 # Add labels for 'uphill' and 'downhill' categories
    data = . %>%                                         # Summarize the data by 'uphill' variable
      group_by(uphill) %>% 
      summarise(elevationalRangeShift = mean(elevationalRangeShift)) %>% 
      mutate(x = -0.15, label = c("downhill", "uphill")),# Create labels and position them
    aes(label = label, x = x, color = uphill),           # Map labels and color based on 'uphill'
    show.legend = FALSE, size = 6 ) +                       # Remove legend and set label size
  coord_cartesian(xlim = c(-0.25, 0.25), ylim = c(-120, 120)) +  # Set limits for x and y axes
  theme_clean() +                                                 # Apply 'clean' theme
  theme(										# Customize theme for better visualization
    axis.text.x = element_blank(),              # Remove x-axis text
    axis.title.x = element_text(size = 12),     # Set x-axis title text size
    axis.text.y = element_text(size = 12),      # Set y-axis text size
    axis.title.y = element_text(size = 12),     # Set y-axis title text size
    axis.ticks.x = element_blank(),             # Remove x-axis ticks
    axis.line.x = element_blank(),              # Remove x-axis line
    axis.line.y = element_blank(),              # Remove y-axis line
    plot.background = element_blank()     ) +   # Remove plot background (including border) 
  labs(									   	    # Set axis labels for the plot
    y = "Elevational range shift (meters)",     # Label for the y-axis
    x = "random jitter to show points"   )        # Label for the x-axis
 
