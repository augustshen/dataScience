# 1 parameters and Estimates
# 1.1 Sampling Model Parameters and Estimates
library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)

# estimate: summary of the observed data that we think is informative 
#           about the parameter of interest
# population
# p  p parameter
# sample
# spread p - (1 - p) = 2p - 1

# 1.2 The Sample Average
# estimate p from sample

# 1.3 Polling versus Forecasting

# 1.4 Properties of Our Estimate
# E(N*avg(x)) = N * p
# E(avg(x)) = p
# SE(avg(x)) = sqrt(p*(1-p)/N)

# exercises
# E(S) = N * p
# SE(S) = sqrt(p*(1-p)*N)

# ex5
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p * (1 - p) / N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(p, se)

# ex6
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.

for ( N in sample_sizes) {
  se <- sqrt(p * (1 - p) / N)
  plot(p, se, ylim=c(0, 0.1))
}

# E[X¯−(1−X¯)]=E[2X¯−1] =2E[X¯]−1 =2p−1 =p−(1−p)
# SE[X¯−(1−X¯)]=SE[2X¯−1] =2SE[X¯] =2p(1−p)/N−−−−−−−−−√

# ex9
# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
2 * sqrt(p * (1 - p) / N)

