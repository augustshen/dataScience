
sample(200, 100)   # create random numbers

#1.1.2 monte carlo simulations
beads <- rep( c("red", "blue"), times= c(2,3))
beads
sample(beads, 1)

B <- 10000
events <- replicate(B, sample(beads, 1))
events
tab <- table(events)
tab
prop.table(tab)

event2 <- sample(beads, B, replace = TRUE)
prop.table((table(event2)))

#1.1.3 Probability Distributions

#1.1.4 Independence
x <- sample(beads, 5)
x[2:5]

#1.2.1 Combinations and Permutations
number <- "Three"
suit <- "Hearts"
paste(number, suit)

paste(letters[1:5], as.character((1:5)))

expand.grid(pants=c("blue", "black"), shirt=c("white", "grey", "plaid"))

paste(c("blue", "black"), c("white", "grey", "plaid"))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)
deck

kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)

permutations(5, 2)

all_phone_numbers <- permutations(10, 7, v=0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index, ]

hands <- permutations(52, 2, v=deck)
hands

first_card <- hands[,1]
second_card <- hands[,2]

sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck)
hands
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard))

hand <- sample(deck, 2)
hand

b <- 10000
#if next 2 in one line, will cause error
results <- replicate(b, {hand <-sample(deck,2) 
(hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)})
mean(results)

#1.2.2 Birthday Problem
n <- 50
bdays <- sample(1:365, n, replace = TRUE)

duplicated(c(1,2,3,1,4,3,5))
any(duplicated(bdays))
b <- 10000
results <- replicate(b, {
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
})
mean(results)

#1.2.3 sapply
compute_prob <- function(n, B=10000) {same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
})
  mean(same_day)
}
n <- seq(1, 60)

x <- 1:10
sqrt(x)

compute_prob(n)  #not work 
prob <- sapply(n, compute_prob)
plot(n, prob)
# the way above is using the mentl carlo simulate, a little slow

exact_prob <- function(n) {
  prob_unique <- seq(365, 365 - n + 1)/365
  1 - prod(prob_unique)
}
eprob <- sapply(n, exact_prob)
eprob
plot(n, prob)
lines(n, eprob, col="red")

#1.2.4 how many monte carlo experiments are enough?
B <- 10^seq(1, 5, len = 100)
compute_prob <- function(B, n =22) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
prob <- sapply(B, compute_prob)
plot(log10(B), prob)
line(log10(B), prob, col="red")

#1.2 exercises
#ex5
# This line of sample code simulates four random games where the Celtics either lose or win. Each game is independent of other games.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
simulated_games

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that first replicates the sample code generating the variable called `simulated_games` for `B` iterations and then tallies the number of simulated series that contain at least one win for the Celtics.

celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games == "win") })

# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
mean(celtic_wins)

#1.3 Addition Rule and Monty Hall
#1.3.1 The Addition Rule
#1.3.2 The Monty Hall Problem
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})
mean(switch)

#1.3 exercises
#1.3 ex1
# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable 'l' to a list of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs. 
l <- c(0:1)


# Create a data frame named 'possibilities' that contains all possible outcomes for the remaining games.
possibilities <- expand.grid(l,l,l,l,l,l)
possibilities <- expand.grid(l,rep(6))
possibilities
names(possibilities)
type(possibilities)
rowSums(possibilities)
possibilities <- cbind(l,l,l,l,l,l)

combn(l, n, replace=TRUE)
possibilities
expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
            sex = c("Male","Female"))
x <- seq(0, 10, length.out = 100)
y <- seq(-1, 1, length.out = 20)
d1 <- expand.grid(x = x, y = y)
d2 <- expand.grid(x = x, y = y, KEEP.OUT.ATTRS = FALSE)
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x
df <- data.frame(possibilities)
results <- rowSums(df); 
mean(results >= 4)
colSums(x)

#1.3 ex1 final
# Assign a variable 'n' as the number of remaining games.
n <- c(1:6)

# Assign a variable 'l' to a list of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs. 
l <- c(0:1)


# Create a data frame named 'possibilities' that contains all possible outcomes for the remaining games.
temp <- expand.grid(l, l, l, l, l, l)
possibilities <- data.frame(temp)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities)

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results >= 4)

#1.3 ex2
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates the sample code for `B` iterations and tallies the number of simulated series that contain at least four wins for the Cavs.
results <- replicate(B, {
  n <- 6
  one <- sample(0:1, n, replace = TRUE)
  sum(one) >= 4
})
mean(results)



# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.

#1.3 ex3
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)

#1.3 ex4
# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N <- seq(1, 25, 2)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr <- sapply(N, prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)
