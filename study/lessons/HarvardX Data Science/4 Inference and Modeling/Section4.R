# Statistical Models
# 4.1 Poll Aggregators
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

confidence_intervals <- sapply(Ns, function(N) {
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat * (1 - X_hat) / N)
  2 * c(X_hat, X_hat - 2 * SE_hat, X_hat + 2 * SE_hat) - 1
})

polls <- data.frame(poll=1:ncol(confidence_intervals),
                    t(confidence_intervals),
                    sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

sum(polls$sample_size)

d_hat <- polls %>% summarize(avg = sum(estimate * sample_size) / sum(sample_size)) %>%
  .$avg
p_hat <- (1 + d_hat) / 2
moe <- 2 * 1.96 * sqrt(p_hat * (1 - p_hat) / sum(polls$sample_size))
moe

round(d_hat* 100, 1)
round(moe * 100, 1)

# 4.2 Pollsters and Multilevel Models

# 4.3 Poll Data and Pollster Bias
data("polls_us_election_2016")
names(polls_us_election_2016)

polls <- polls_us_election_2016 %>% filter(state == "U.S." & enddate >= "2016-10-31" &
                                             (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
polls <- polls %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump / 100)

d_hat <- polls %>% summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
p_hat <- (d_hat + 1) / 2
moe <- 1.96 * 2 * sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
moe

polls %>% ggplot(aes(spread)) + geom_histogram(color="black", binwidth = .01)

polls %>% group_by(pollster) %>% summarize(n())

polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

polls %>% group_by(pollster) %>% filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1 - p_hat) / median(samplesize)))

# 4.4 Data-Driven Models
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>% ungroup()
one_poll_per_pollster %>% ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# in new model d and sigema are both unknown
sd(one_poll_per_pollster$spread)
results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96 * se, end = avg + 1.96 * se)
round(results * 100, 1)

# exercises
# ex1
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

# ex2 
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

# ex4
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
X_hat <- mean(X)
se_hat <- sd(X)
se <- se_hat / sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(qnorm(0.025, mean(X), se), qnorm(0.975, mean(X), se))

# ex5
# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  X_hat <- mean(X)
  se_hat <- sd(X)
  se <- se_hat / sqrt(N)
  interval <- c(qnorm(0.025, mean(X), se) , qnorm(0.975, mean(X), se))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

# ex6
# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster, spread)) + geom_boxplot() + geom_point()

# ex13
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
polls %>% group_by(pollster)
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))

# Print the contents of sigma to the console
sigma

# ex15
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)
names(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread), s = sd(spread), N=n())
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- max(res$avg) - min(res$avg)
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)

# ex16
# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
2 * (1 - pnorm(estimate / se_hat, 0, 1))

# ex17
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread))
var


