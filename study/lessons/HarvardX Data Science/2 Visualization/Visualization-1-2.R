library(dslabs)
data("heights")
prop.table(table(heights$sex))

x <- heights$height[heights$sex == "Male"]
average <- sum(x) / length(x)
SD <- sqrt(sum((x - average)^2) / length(x))
SD <- sd(x)
z <- scale(x)
mean(abs(z) < 2)
