library(dslabs)
data("heights")

mean(x <= 69.5)

p <- seq(0.05, 0.95, 0.05)

observed_quantiles <- quantile(x, p)

theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))

plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

observed_quantiles <- quantile(z, p)

theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

