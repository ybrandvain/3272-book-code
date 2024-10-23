# Load necessary libraries
library(readr)      # For reading data from a CSV file
library(ggplot2)    # For creating visualizations using ggplot2
library(ggthemes)   # For additional ggplot2 themes, such as theme_clean()
library(dplyr)      # For data manipulation, including mutate and filtering operations
library(plotly)     # For creating interactive plots from ggplot objects
library(tidyr)      # For data tidying functions, including 'separate()'

# Load the data from the provided CSV file (URL) into a data frame
range_shift_file <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter11/chap11q01RangeShiftsWithClimateChange.csv"
range_shift <- read_csv(range_shift_file)  # Reads the CSV file from the URL

# Data wrangling using dplyr and tidyr
range_data <- range_shift %>%
  # Separate the 'taxonAndLocation' column into two new columns: 'taxon' and 'location'
  separate(col = "taxonAndLocation", into = c("taxon", "location"), sep = "_") %>%
  
  # Create a ggplot object
  ggplot(aes(y = location, fill = taxon)) +  # Map 'location' to y-axis and 'taxon' to fill for bar colors
  geom_bar()  # Create a bar plot, showing the counts of 'location' and colored by 'taxon'

# Convert the ggplot into an interactive plot using ggplotly() from the plotly package
ggplotly(range_data)  # This allows users to interact with the bar plot (hover over, zoom in/out, etc.)
