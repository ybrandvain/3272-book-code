##############
# Goals:
# Simulate the Null Distribution: The code simulates the distribution of the proportion of vaccinated individuals among COVID-19 cases under the null hypothesis (no effect of vaccination on COVID-19 rates). 
     # This helps in understanding the expected variation due to random chance (sampling error) in a sample size of 196.
# Visualize Sampling Error: The histogram shows the distribution of possible sample outcomes if the vaccine had no effect. The key takeaway is to see how the proportion can fluctuate around the null expectation due to sampling variation.
# Compare to Real Data: The arrow points to an observed estimate from a Phase III trial, allowing for a visual comparison between the real observed data and the distribution expected under the null hypothesis. This can be used to assess whether the observed data significantly deviates from what would be expected due to chance.
# Teach Sampling Concepts: This visualization illustrates the role of sampling error and can be used to explain how deviations between expected and observed proportions in real-world data could arise simply from random sampling, rather than from actual effects of treatments (in this case, the vaccine).
##############


###########################################################################################
############# WARNING  THIS TAKES A WHILE AND A BUNCH OF MEMORY ###########################
###########################################################################################

library(ggplot2)
library(dplyr)
# Simulating null distribution under the assumption that vaccination has no effect on COVID-19 cases

# 'tibble()' is used to create a tibble (a modern version of a data frame in the tidyverse).
null_covid <- tibble(
  # Using 'replicate()' to repeat the process 100,000 times
  x = replicate(
    n = 100000,  # Number of simulations (100,000 samples)
    
    # 'sample()' is used to randomly assign individuals as "Unvaccinated" or "Vaccinated".
    # We draw a sample of size 196 and calculate the proportion of those who are "Vaccinated".
    sum(sample(c("Unvaccinated", "Vaccinated"), replace = TRUE, size = 196) == "Vaccinated") / 196
  )
)

# Visualizing the null sampling distribution using ggplot2
ggplot(null_covid, aes(x = x)) +
  # Creating a histogram to visualize the distribution of proportions
  geom_histogram(
    binwidth = 1 / 196,  # The bin width is set based on the sample size (196).
    color = "white",     # Setting the color of the histogram's outline.
    lwd = 0.1,           # Line width of the outline.
    aes(y = ..count.. / sum(..count..)),  # Normalizing the y-axis to represent probabilities.
    fill = "lightsalmon" # Fill color of the bars.
  ) +
  
  # Adding an arrow to indicate an observed estimate from a Phase III trial
  geom_segment(
    aes(x = 0.2, xend = 11 / 196, y = 0.03, yend = 0),  # Arrow pointing to an estimate of 11/196 (~5.6%).
    linewidth = 1, color = "steelblue",  # Arrow aesthetics: size and color.
    arrow = arrow(length = unit(0.1, "npc"))  # Arrowhead length.
  ) +
  
  # Limiting the x-axis to show proportions between 0 and 1
  xlim(c(0, 1)) +
  
  # Adding a label for the observed estimate
  annotate(
    geom = "text", x = 0.2, y = 0.045, 
    label = "Estimate from \nPhase III trial", 
    color = "steelblue"
  ) +
  
  # Adding a title and axis labels
  labs(
    title = "Null sampling distribution for\nModerna data.",  # Title of the plot.
    x = "Proportion of total covid cases observed\nin the vaccinated sample.",  # x-axis label.
    y = "Probability"  # y-axis label (normalized count).
  )
