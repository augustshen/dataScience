# The Central Limit Theorem in Practice
# Pr((avg(X) - E(avg(X)))/SE(avg(X)) <= ((p+0.01) - E(avg(X))) / SE(avg(X))) - Pr((avg(X) - E(avg(X)))/SE(avg(X)) <= ((p-0.01) - E(avg(X))) / SE(avg(X)))
# Pr(Z <= ((p+0.01) - E(avg(X))) / SE(avg(X))) - Pr(Z <= ((p-0.01) - E(avg(X))) / SE(avg(X))) 
# E(avg(X)) = p
# SE(avg(X)) = sqrt(p * (1 - p) / N)
# Pr(Z <= 0.01/sqrt(p * (1 - p) / N)) - Pr(Z <= -0.01/sqrt(p * (1 - p) / N))

# SE^(avg(X)) = sqrt(avg(X) * (1 - avg(X)) / N)
x_hat <- 0.48
se <- sqrt(x_hat * (1 - x_hat) / 25)
se
pnorm(0.01/se) - pnorm(-0.01/se)

# 2.2 Margin of Error
# Pr(abs(avg(X) - p) <= 2 * SE(avg(X)))
# Pr(avg(X) <= p + 2 * SE(avg(X))) - Pr(avg(X) <= p - 2 * SE(avg(X)))
# Pr((avg(X) - E(avg(X)))/SE(avg(X)) <= ((p + 2 * SE(avg(X))) - E(avg(X))) / SE(avg(X))) - Pr((avg(X) - E(avg(X)))/SE(avg(X)) <= ((p - 2 * SE(avg(X))) - E(avg(X))) / SE(avg(X)))
# Pr(Z <= 2 * SE(avg(X)) / SE(avg(X))) - Pr(Z <= -2 * SE(avg(X)) / SE(avg(X)))
# Pr(Z <= 2) - Pr(Z <= -2)
pnorm(2) - pnorm(-2)

# 2.3 A Monte Carlo Simulation for the CLT
p <- 0.45
B <- 10000
N <- 1000
X_hat <- replicate(B, {
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
  mean(X)
})
mean(X_hat)
sd(X_hat)

# not work above because there is no p
p <- 0.45
N <- 1000
X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
X_hat <- mean(X)

library(tidyverse)
library(gridExtra)
p1 <- data.frame((X_hat= X_hat)) %>% ggplot(aes(X_hat)) + 
  geom_histogram(binwidth = 0.005, color="black")
p2 <- data.frame(X_hat = X_hat) %>% ggplot(aes(sample = X_hat)) + 
  stat_qq(dparams = list(mean= mean(X_hat), sd=sd(X_hat))) + 
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow = 1)

# 2.4 The Spread

# 2.5 Bias: Why not run a very large Poll
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2 * sqrt(x * (1 - x) / N))
data.frame(p = p, SE = SE) %>% ggplot(aes(p, SE)) + geom_line()

# exercises
# ex1
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p, N) {
  s <- sample(c(1, 0), N, replace = TRUE, prob = c(p, 1 - p))
  mean(s)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p, N)

# ex2
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, take_sample(p, N))

# Calculate the mean of the errors. Print this value to the console.
mean(p - errors)

# ex4
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.

mean(abs(errors))

# ex5
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`
sqrt(mean(errors ^ 2))

# ex 6
# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p * (1 - p) / N)

# ex7
# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(1, 0), N, replace = TRUE, prob = c(p, 1 - p))

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar * (1 - X_bar) / N)

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
hist(se)
plot(N, se)

# ex11
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)

# ex12
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
1 - pnorm(0.5, p, sqrt(p * (1 - p) / N))

# ex13
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1 - pnorm(.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)

