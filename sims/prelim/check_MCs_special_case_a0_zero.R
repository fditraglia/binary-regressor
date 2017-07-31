# Check the simpler just-identified moment conditions that arise when a0 = 0
library(mvtnorm)
dgp <- function(a1, b = 1, n = 1000, d = 0.15, rho = 0.5){
  a0 <- 0
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
  return(data.frame(Tstar, Tobs, y, z))
}

est <- function(dat){
 with(dat, 2 * cov(y * Tobs, z) / cov(Tobs, z) - cov(y^2, z) / cov(y, z)) 
}

est(dgp(a1 = 0.0, b = 2, n = 1000000))
est(dgp(a1 = 0.1, b = 2, n = 1000000))
est(dgp(a1 = 0.2, b = 2, n = 1000000))
est(dgp(a1 = 0.3, b = 2, n = 1000000))
est(dgp(a1 = 0.4, b = 2, n = 1000000))
est(dgp(a1 = 0.5, b = 2, n = 1000000))

est(dgp(a1 = 0.0, b = 1, n = 1000000))
est(dgp(a1 = 0.1, b = 1, n = 1000000))
est(dgp(a1 = 0.2, b = 1, n = 1000000))
est(dgp(a1 = 0.3, b = 1, n = 1000000))
est(dgp(a1 = 0.4, b = 1, n = 1000000))
est(dgp(a1 = 0.5, b = 1, n = 1000000))

est(dgp(a1 = 0.0, b = 0.5, n = 1000000))
est(dgp(a1 = 0.1, b = 0.5, n = 1000000))
est(dgp(a1 = 0.2, b = 0.5, n = 1000000))
est(dgp(a1 = 0.3, b = 0.5, n = 1000000))
est(dgp(a1 = 0.4, b = 0.5, n = 1000000))
est(dgp(a1 = 0.5, b = 0.5, n = 1000000))

est(dgp(a1 = 0.0, b = 0.25, n = 1000000))
est(dgp(a1 = 0.1, b = 0.25, n = 1000000))
est(dgp(a1 = 0.2, b = 0.25, n = 1000000))
est(dgp(a1 = 0.3, b = 0.25, n = 1000000))
est(dgp(a1 = 0.4, b = 0.25, n = 1000000))
est(dgp(a1 = 0.5, b = 0.25, n = 1000000))
