# Confidence Intervals
library(tidyverse)
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() + 
  ggtitle("Average Yearly Temperatures in the New Haven")

p <- 0.45
N <- 1000
X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat * (1 - X_hat) / N)
c(X_hat -  2 * SE_hat, X_hat + 2 * SE_hat)

# Pr(X_hat - 2SE_hat <= p <= X_hat + 2SE_hat)
# Pr(-2 <= (X_hat - p)/SE_hat <= 2)
# Pr(-2 <= Z <= 2)

z <- qnorm(0.995)
z
pnorm(qnorm(0.995))
pnorm(1 - qnorm(0.995))
pnorm(z) - pnorm(-z)
qnorm(0.975)


# 3.2 A Monte Carlo Simulation for Confidence Intervals
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat * (1 - X_hat) / N)
  between(p, X_hat - 2 * SE_hat, X_hat + 2 * SE_hat)
})
mean(inside)

# 3.3 the correct language
# p is not random, so say p is 95% in the interval is wrong

# 3.4 power 
N <- 25
X_hat <- 0.48
(2 * X_hat - 1) + c(-2, 2) * 2 * sqrt(X_hat * (1 - X_hat) / sqrt(N))
# power is the probability of detecting a spread different from 0

# 3.5 p-Values
# null hypothesis: in this case, that the spread is 0
# p-value: how likely is it to see a value this large when the null hypothesis is true?
# Pr(abs(X_hat - 0.5) > 0.02)
# under the null hypothesis:  sqrt(N) * (X_hat - 0.5) / sqrt(0.5 * (1 - 0.5)) is standard normal
# Pr(sqrt(N) * (X_hat - 0.5) / sqrt(0.5 * (1 - 0.5)) > sqrt(N) * 0.02 / sqrt(0.5 * (1 - 0.5)))
# Pr(sqrt(N) * (X_hat - 0.5) / sqrt(0.5 * (1 - 0.5)) > Z)

N <- 100
z <- sqrt(N) * 0.02 / 0.5
1 - (pnorm(z) - pnorm(-z))  
# about 0.69 mean if you see 52 blue in 100, there is 69% chance that the red & blue is same

# if a 95% confidence interval of the spread does not include 0,
# we know that the p-value must be smaller than 0.05

# exercises
# ex1
# Load the data
library(dslabs)
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate >= '2016-10-31' & state=='U.S.')
names(polls)

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1] / 100
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat * (1 - X_hat) / N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(qnorm(0.025, X_hat, se_hat), qnorm(0.975, X_hat, se_hat))
ci

# ex2
# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, lower confidence interval, and upper confidence interval for each poll.
#names(polls)

pollster_results <- polls %>% mutate(X_hat=rawpoll_clinton / 100) %>% mutate(se_hat=sqrt(X_hat * (1 - X_hat) / samplesize)) %>% mutate(lower = qnorm(0.025, X_hat, se_hat)) %>% mutate(upper = qnorm(0.975, X_hat, se_hat)) %>% select(pollster, enddate, X_hat, se_hat, lower, upper)
pollster_results

# ex3
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
#pollster_results <- pollster_results %>% mutate(hit = lower <= 0.482 & upper >= 0.482)
avg_hit <- pollster_results %>% mutate(hit = lower <= 0.482 & upper >= 0.482) %>% summarise(mean = mean(hit))
avg_hit






# ex5
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
polls <- polls %>% mutate(d_hat = (rawpoll_clinton - rawpoll_trump) / 100)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N

# For the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1] 
d_hat

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat  + 1) / 2
X_hat

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2 * sqrt(X_hat * (1 - X_hat) / N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975) * se_hat, d_hat + qnorm(0.975) * se_hat)
ci

# ex6
# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>% mutate(X_hat = (d_hat + 1) / 2) %>% mutate(se_hat = 2 * sqrt(X_hat * (1 - X_hat) / samplesize)) %>% mutate(lower = d_hat - qnorm(0.975) * se_hat) %>% mutate(upper = d_hat + qnorm(0.975) * se_hat) %>% select(pollster, enddate, d_hat, lower, upper)
pollster_results

# ex 7
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit=lower <= 0.021 & upper >= 0.021) %>% summarize(mean(hit))

# ex8
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls %>% mutate(error = d_hat - 0.021) %>% ggplot(aes(x = pollster, y = error)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ex9
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls %>% mutate(error = d_hat - 0.021) %>% group_by(pollster) %>% filter(n() >= 5) %>% ggplot(aes(x = pollster, y = error)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

