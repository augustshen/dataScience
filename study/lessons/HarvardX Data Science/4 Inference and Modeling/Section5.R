# Bayesian Statistics
# hierarchical model
# 5.2 Bayes' Theorem
# Pr(A|B) = Pr(A and B) / Pr(B) = Pr(B | A) * Pr(A) / Pr(B)
prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1 - prev))
N_D <- sum(outcome == "Disease")
N_D
N_H <- sum(outcome == "Healthy")
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace = TRUE, prob = c(accuracy, 1 - accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace = TRUE, prob = c(accuracy, 1 - accuracy))
table(outcome, test)

# 5.3 Bayes in Practice

# 5.4 The Hierarchical Model
# posterior distribution
# E(p|y) = B*miu + (1 -B) * Y = miu + (1 - B)(Y - miu)
# B = sigma^2 / (sigma^2 + tau^2)
# miu is avg for all the players
# Y is observe for Jose

# exercises
# ex8
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results
sigma <- results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
tau <- 0.01
miu <- 0
B <- sigma^2 / (sigma^2 + tau^2)
B

# Calculate the expected value of the posterior distribution
miu + (1 - B) * (Y - miu)

# ex9
# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1 / (1 / sigma ^2 + 1 / tau ^2))

# ex10
# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
est <- B * mu + (1 - B) * Y
est
ci <- c(est - qnorm(0.975) * se, est + qnorm(0.975) * se)
ci

# ex11
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

# ex12
# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau) {
  B <- sigma ^ 2 / (sigma^2 + tau^2)
  se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  exp_value <- B * mu + (1 - B) * Y
  pnorm(0, exp_value, se)
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)


