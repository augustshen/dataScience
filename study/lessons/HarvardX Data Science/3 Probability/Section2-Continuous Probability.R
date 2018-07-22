#2.1.1 Continuous Probability
# cumulative distribution function CDF
# empirical cumulative distribution function eCDF
library(tidyverse)
library(dslabs)
data("heights")
x <- heights %>% filter(sex=="Male") %>% .$height

F <- function(a) mean(x<=a)
1 - F(70)

#2.1.2 Theoretical Distribution
1 - pnorm(70.5, mean(x), sd(x))
plot(prop.table(table(x)), xlab="a = Height in inches", ylab="Pr(X - a)")

# the result is simular with normal distribution
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# the result is not simular with normal distribution
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

#2.1.3  Probability Density
#F(a) = Pr(X <= a)  F(a) is probability Density
avg <- mean(x)
s <- sd(x)
1 - pnorm(76, avg, s)
dnorm(76, avg, s)

#2.1.4 Monte Carlo Simulations
x <- heights %>% filter(sex=="Male") %>% .$height
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)
ds_theme_set()
data.frame(simulated_heights=simulated_heights) %>% ggplot(aes(simulated_heights)) + 
  geom_histogram(color="black", binwidth=2)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})
mean(tallest >= 84)

#2.1.5 Other Continuous Distributions
# t - student-t
# chi-squared
# exponential
# gamma
# beta
# d - density
# q - quantile
# p - probability density
# r - random
# norm - normal distribution
x <- seq(-4, 4, length.out = 100)
data.frame(x, f=dnorm(x)) %>% ggplot(aes(x,f)) + geom_line()

#2.1 exercises
#2.1 ex1
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is shorter than 5 feet. Print this value to the console.
pnorm(5 * 12, female_avg, female_sd)

#2.1 ex2
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is 6 feet or taller. Print this value to the console.
1 - pnorm(6*12, female_avg, female_sd)

#2.1 ex3
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.

# 2.1 ex4
# Assign a variable 'female_avg' as the average female height. Convert this value to centimeters.
female_avg <- 64*2.54

# Assign a variable 'female_sd' as the standard deviation for female heights. Convert this value to centimeters.
female_sd <- 3*2.54

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.
pnorm(67*2.54, female_avg, female_sd) - pnorm(61*2.54, female_avg, female_sd)

# 2.1 ex5
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# To a variable named 'taller', assign the value of a height that is one SD taller than average.
taller <- pnorm(female_avg + female_sd, female_avg, female_sd)

# To a variable named 'shorter', assign the value of a height that is one SD shorter than average.
shorter <- pnorm(female_avg - female_sd, female_avg, female_sd)

# Calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.
taller - shorter

# 2.1 ex7
# Assign a variable 'female_avg' as the average female height.
male_avg <- 69

# Assign a variable 'female_sd' as the standard deviation for female heights.
male_sd <- 3

# Determine the height of a man in the 99th percentile of the distribution.
qnorm(0.99, male_avg, male_sd)   # How tall is a male in the 99th percentile

# 2.1 ex8
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- max(rnorm(10000, 100, 15))

his <- replicate(B, highestIQ)
# Make a histogram of the highest IQ scores.
hist(his)
