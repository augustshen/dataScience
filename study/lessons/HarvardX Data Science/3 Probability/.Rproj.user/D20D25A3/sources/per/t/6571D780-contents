# 4.1.1 the big short: Interest Rates explained
library(dslabs)
library(dplyr)
library(ggplot2)
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample(c(0, 1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0, 1), n, prob = c(1-p, p), replace=TRUE)
  sum(defaults * loss_per_foreclosure)
})
mean(losses)
data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) + geom_histogram(binwidth = 0.6, col="black")

n * (p * loss_per_foreclosure + (1 - p) * 0)
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p * (1-p))

# lp + x(1-p) = 0
x <- -loss_per_foreclosure * p/(1-p)
# we want P(S < 0) = 0.01
# change to P((S - E[S]) / SE[S] < - E[S]/SE[S])
# (S - E[S]) / SE[S] name to Z
# P(Z < -{lp + x(1-p)}n / ((x-l) * sqrt(n*p*(1-p)))) = 0.01
# qnorm(0.01, avg, sd)  will tell the value of z
# x = -l * (np - z*sqrt(np(1-p))) / (n(1-p) + z*sqrt(np(1-p)))
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l * (n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
x

loss_per_foreclosure * p + x * (1 - p)

B <- 10000
profit <- replicate(B, {
  draws <- sample(c(x, loss_per_foreclosure), n, prob=c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit)
mean(profit < 0)

x / 180000

# 4.1.2 the big short
p <- 0.04
r <- 0.05
x <- r * 180000
loss_per_foreclosure * p + x * (1-p)  # these three lines seems not right

# E[S] = n * mu
# SE[S] = sqrt(n) * sigema
# z = -E[S]/SE[S] = - sqrt(n) * mu / sigema
z <- qnorm(0.01)
n <- ceiling( (z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2 )
n

n * (loss_per_foreclosure * p + x * (1-p))
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample(c(x, loss_per_foreclosure), n, prob=c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit < 0)
# seems good but fail, because the calculate is base on fomular
# SE[(X1+X2+...+Xn)/n] = sigema / sqrt(n)
# in this fomular the X should be independent

# when dependent, the model will like this
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, prob = c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})

mean(profit)
mean(profit < 0)
mean(profit < -10000000)

data.frame(losses_in_millions = profit/10^6) %>% ggplot(aes(losses_in_millions)) + geom_histogram(binwidth = 0.6, col="black")

# 4.1 exercises
# ex1
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1 - p_default, p_default))

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults * loss_per_foreclosure)

S

# ex2
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans
S <- replicate(B, {
  defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1 - p_default, p_default))
  sum(defaults * loss_per_foreclosure)
})




# Plot a histogram of 'S'
hist(S)

# ex3
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Calcualte the expected loss due to default out of 10,000 loans
p_default * n * loss_per_foreclosure

# ex4
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Compute the standard error of the sum of 10,000 loans
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p_default * (1 - p_default))

# ex5
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x <- - p_default * loss_per_foreclosure / (1 - p_default)

# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x / 180000

# ex6
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure * (n*p_default - z*sqrt(n*p_default*(1-p_default))) / (n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x / 180000

