# Goal: To generate and visualize the sampling distribution of a test statistic under the null hypothesis
# using a histogram.
#The goal of this code is to simulate a null sampling distribution of a test statistic (in this case, the mean of a sample of size 20 from a standard normal distribution) and visualize this distribution using a histogram. This helps to understand what the distribution of the test statistic looks like under the null hypothesis, which is essential for comparison with observed test statistics in hypothesis testing.

# Load the ggplot2 library for data visualization
library(ggplot2)

# Step 1: Generate a data frame with 50,000 estimates from the null model.
# Here, we generate random samples of size 20 from a normal distribution (mean=0, sd=1),
# calculate the mean of each sample, and store the results in a data frame.
sample.dist  <- data.frame(estimate = replicate(50000, mean(rnorm(20, 0, 1))))

# Step 2: Create a histogram of the sampling distribution of the test statistic.
ggplot(data = sample.dist, aes(x = estimate)) +
  geom_histogram(bins = 100, color = "white", lwd = 0.1, aes(y = ..count../sum(..count..)), fill = "lightsalmon") +    # Add a histogram layer with 100 bins, setting white borders between bars and customizing the fill color.
  labs(title = "A null distribution", y = "", x = "Test statistic") +   # Add labels for the title and x-axis (the y-axis label is left empty).
  annotate(geom = "text", x = 0, y = 0.015, label = "Null\nsampling\ndistribution", color = "black")   # Annotate the plot with a label indicating "Null sampling distribution" at the specified coordinates.
