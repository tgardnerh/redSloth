rm(list = ls())
cat("\014")
library(tidyverse)

# consistency of beta
set.seed(42069)
f <- function(n) {
n <- n
b <- 4

x <- runif(n,0,100)
e <- rnorm(n,0,10)

y <- b*x + e

bhat <- coef(lm(y~x))[2]
bdiff <- bhat - b

return(bdiff)
}

g <- function(n) {
  v <- c(NULL)
  for (i in c(1:n)) {
    v <- c(v, f(i))
  }
  return (v)
}

N <- 200
n <- c(1:N)

ggplot() + geom_line(aes(n[-1],g(N)[-1])) + ylab("") + xlab("Number of observations") + labs(title = "The OLS Estimator is Consistent")

# convergence in distribution

set.seed(42069)
f <- function(n) {
  n <- n
  b <- 4
  
  bhat <- c()
  for (i in c(1:1000)) {
  x <- runif(n,0,100)
  e <- rnorm(n,0,10)
  y <- b*x + e
  
  bhat <- c(bhat, coef(lm(y~x))[2])
  }
  return(bhat)
}

ggplot() + geom_density(aes(f(100))) + geom_density(aes(f(1000))) + geom_density(aes(f(10000))) + xlab("") + ylab("Density") + labs(title = "The OLS Estimator Converges Approximately to a Normal Distribution")