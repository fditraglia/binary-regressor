dgp <- function(a0, a1, b = 1, n = 1000, d = 0.15, rho = 0.5){
  n_treat <- ceiling(n/2)
  n_control <- n - n_treat
  z <- c(rep(0, n_control), rep(1, n_treat)) # offer of treatment
  errors <- rmvnorm(n, sigma = matrix(c(1, rho, rho, 1), 2, 2, byrow = TRUE))
  g0 <- qnorm(d) 
  g1 <- qnorm(1 - d) - qnorm(d) 
  Tstar <- as.numeric(g0 + g1 * z + errors[,2] > 0) #select into treatment
  y <- b * Tstar + errors[,1]
  #mis-classification
  Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1) 
  return(data.frame(Tobs, Tstar, y, z))
}

set.seed(1027394)

beta_true <- 1
a0_true <- 0.1
a1_true <- 0.2
library(mvtnorm)
dat <- dgp(a0_true, a1_true, b = beta_true, n = 1e7)

F0 <- ecdf(with(dat, y[z == 0]))
F1 <- ecdf(with(dat, y[z == 1]))
F10 <- ecdf(with(dat, y[Tobs == 1 & z == 0]))
F11 <- ecdf(with(dat, y[Tobs == 1 & z == 1]))

p0 <- with(dat, mean(Tobs[z == 0]))
p1 <- with(dat, mean(Tobs[z == 1]))

D <- function(x) F1(x) - F0(x)
D1 <- function(x) p1 * F11(x) - p0 * F10(x)

MC_truth <- function(x){
  LHS <- D1(x + beta_true) - D1(x) 
  RHS <- a0_true * D(x + beta_true) - (1 - a1_true) * D(x)
  LHS - RHS
}

quantile(dat$y, 1:19/20)
range(dat$y)
tau <- seq(-5, 6, 0.01)
plot(tau, MC_truth(tau), type = 'l')

plot(tau, D(tau), type = 'l')
plot(tau, D1(tau), type = 'l')
