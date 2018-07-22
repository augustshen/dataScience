# 3.1 Random Variables & Sampling Models
# 3.1.1 Random Variables
beads <- rep(c("red", "blue"), times = c(2, 3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)
X
ifelse(sample(beads, 1) == "blue", 1, 0)

# 3.1.2 Sampling Models
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

#define in one line code
X <- sample(c(-1, 1), n, replace = TRUE, prob=c(9/19, 10/19))
S <- sum(X)
S

n <- 1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
  sum(X)
})
mean(S < 0)
mean(S)
sd(S)

s <- seq(min(S), max(S), length=100)
normal_density <- data.frame(s = s, f=dnorm(s, mean(S), sd(S)))
data.frame(S=S) %>% ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") + geom_line(data=normal_density, mapping=aes(s, f), color="blue")

#3.1.3 distributions versus Probability Distributions
avg <- sum(x) / length(x)
s <- sqrt(sum((x -avg)^2) / length(x))

# 3.1.4 Notation for Random Variables

# 3.1 exercises
# 3.1 ex1
# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green + black + red)

# Print the variable `p_green` to the console
p_green

#3.1 ex2
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

#Create a model to predict the random variable `X`, your winnings from betting on green.
X <- sample(c(-1, 17), 10000, replace=TRUE, prob=c(p_not_green, p_green))

# Print the value of `X` to the console
X

# 3.1 ex3
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green

X <- sample(c(-1, 17), 10000, replace=TRUE, prob=c(p_not_green, p_green))
mean(X)
sd(X)
p_green * 17 - p_not_green

# 3.1 ex4
#Given two possible outcomes, a and b, with the probability of a equal to p, the standard error of the random variable is given by the following equation:
#  ∣b−a∣p(1−p)−−−−−−−√
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Compute the standard error of the random variable
abs(17-(-1)) * sqrt(p_green * p_not_green)

#3.1 ex5
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X <- sample(c(-1, 17), n, replace=TRUE, prob=c(p_not_green, p_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)

# Print the value of 'S' to the console
S

# 3.1 ex6
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
(17 * p_green - p_not_green) * n

#3.1 ex7
#Assume two possible outcomes, a and b, with the probability of a equal to p. When spins are independent, the standard error of the sum of outcomes is given by the following equation:
#  number of spins −−−−−−−−−−−−−√×∣b−a∣p(1−p)−−−−−−−√
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Compute the standard error of the sum of 1,000 outcomes
abs(17-(-1)) * sqrt(n) * sqrt(p_green * p_not_green)

#3.2 central limit theorem
# expected value E[X] = mu
B <- 10^6
X <- sample(c(-1,1), B, replace = TRUE, prob=c(9/19, 10/19))
mean(X)
# a*p + b*(1-p)
# standard error (SE) SE[X]
# sqrt(number of draws) * standard deviation of the numbers in the urn
# standard deviation  = abs(b-a)*sqrt(p*(1-p))
n <- 1000
sqrt(n) * (1 - -1) * sqrt(90)/19

n * (20 - 18)/38  #expert value
sqrt(n) * 2 * sqrt(90)/19  # standard error
mean(S)
sd(S)

mu <- n * (20 - 18)/38
se <- sqrt(n) * 2 * sqrt(90)/19
pnorm(0, mu, se)
mean(S < 0)

#3.2.2 averages and Proportions
# property1: the expected value of the sum of random variables
#            is the sum of the expected values of the individual
#            random variables.
# E[X1+X2+...+Xn] = E[X1] + E[X2] + ... + E[Xn]
# E[X1+X2+...+Xn] = n * mu

#property2: the expected value of a random variable times a non-random
#           constant is the expected value times that non-random constant
# E[a*X] = a * E[X]
# E[(X1+X2+...+Xn)/n] = E[X1+X2+...+Xn]/n = n*mu/n = mu

#property 3: the square of the standard error of the sum of independent
#            random variables is the sum of the square of the standard
#            error of each random variable
# SE[X1+X2+...+Xn] = sqrt(SE[x1]^2 + SE[x2]^2 + ... + E[Xn]^2)

#property 4: the standard erro of a random variable times a non-random
#            constant is the standard error times the non-random constant.
# SE[aX] = aSE[X]

# the standard error of the average of independent draws from the 
# same urn is the standard deviation of the urn, xigema, divided by
# the square root of n
# SE[(X1+X2+...+Xn)/n] = SE[X1+X2+...+Xn] / n
#                      = sqrt(SE[x1]^2 + SE[x2]^2 + ... + E[Xn]^2) / n
#                      = sqrt(sigema^2 + sigema^2 +...+ sigema^2) / n
#                      = sqrt(n*sigema^2) / n
#                      = sigema / sqrt(n)

# property 5: if X is a normally distributed random variable, 
#             then if a and b are non-random contants, aX + b is 
#             also a normally distributed random variable.

# 3.2.3 law of large numbers

# 3.2.4 how large is large in CLT?
# when the probability of success is very low, Poisson solution is more appropriate

# 3.2 exercises
# ex1
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1 - pnorm(0, avg, se)

# ex2
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B, {
  ohs <- sample(c(17, -1), n, replace = TRUE, prob=c(p_green, p_not_green))
  sum(ohs)
})

# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

# ex3
# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S > 0)

# ex5
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
X <- sample(c(-1, 17), n, replace = TRUE, prob = c(p_not_green, p_green))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y <- mean(X)
Y

# ex6 
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
(17 * p_green - 1 * p_not_green)

#ex 7
# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
abs(17 - -1) * sqrt(p_green * p_not_green) / sqrt(n)

# ex8
# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green

# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Given this average and standard error, determine the probability of winning more than $0. Print the result to the console.
1 - pnorm(0, avg, se)

# ex9
# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
p_green <- 2 / 38
p_not_green <- 1 - p_green

S <- replicate(B, {
  ttt <- sample(c(-1, 17), n, replace = TRUE, prob = c(p_not_green, p_green))
  mean(ttt)
})

# Compute the average of `S`
mean(S)

# Compute the standard deviation of `S`
sd(S)

# ex10
# Compute the proportion of outcomes in the vector 'S' where you won more than $0
#1 - pnorm(0, mean(S), sd(S))
mean(S > 0)
