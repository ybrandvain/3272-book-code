# Load necessary libraries
library(readr)      # For reading data from a CSV file
library(ggplot2)    # For creating visualizations using ggplot2
library(dplyr)      # For data manipulation (e.g., filtering, summarizing)
library(patchwork)  # For combining multiple ggplot2 plots into a single layout

# Load the data from the provided CSV file (URL) into a data frame
range_shift_file <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter11/chap11q01RangeShiftsWithClimateChange.csv"
range_shift <- read_csv(range_shift_file)  # Reads the CSV file from the URL

# Create individual plots:

# Histogram of elevationalRangeShift
a <- ggplot(range_shift, aes(x = elevationalRangeShift)) + 
  geom_histogram(bins = 12) +  # Create a histogram with 12 bins
  labs(title = "Histogram", subtitle = "Range shift data")  # Add title and subtitle to the plot

# Density plot of elevationalRangeShift
b <- ggplot(range_shift, aes(x = elevationalRangeShift)) + 
  geom_density(fill = "grey") +  # Create a density plot with a grey fill
  labs(title = "Density plot", subtitle = "Range shift data")  # Add title and subtitle to the plot

# QQ plot to assess normality of elevationalRangeShift
c <- ggplot(range_shift, aes(sample = elevationalRangeShift)) + 
  geom_qq() +  # Create a QQ plot (quantile-quantile plot)
  geom_qq_line() +  # Add a line to compare the data against a theoretical normal distribution
  labs(title = "QQ plot", subtitle = "Range shift data")  # Add title and subtitle to the plot

# Empirical cumulative distribution function (ECDF) of elevationalRangeShift
d <- ggplot(range_shift, aes(x = elevationalRangeShift)) + 
  stat_ecdf() +  # Create an ECDF (cumulative distribution function) plot
  labs(title = "CDF", subtitle = "Range shift data")  # Add title and subtitle to the plot

# Combine the four plots into one layout, with 4 columns (ncol = 4)
a + b + c + d + plot_layout(ncol = 4)  # Arranges the 4 plots in a single row
