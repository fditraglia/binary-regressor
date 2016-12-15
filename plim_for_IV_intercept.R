dgp_intercept <- function(a0, a1, c = 0, b = 1, n = 1000, d = 0.15, rho = 0.5){
  n_treat <- ceiling(n/2)
  n_control <- n - n_treat
  z <- c(rep(0, n_control), rep(1, n_treat)) # offer of treatment
  errors <- rmvnorm(n, sigma = matrix(c(1, rho, rho, 1), 2, 2, byrow = TRUE))
  g0 <- qnorm(d)
  g1 <- qnorm(1 - d) - qnorm(d)
  Tstar <- as.numeric(g0 + g1 * z + errors[,2] > 0) #select into treatment
  y <- c + b * Tstar + errors[,1]
  #mis-classification
  Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1)
  return(data.frame(Tstar, Tobs, y, z))
}

set.seed(637)
a0_true <- 0.25
a1_true <- 0.07
c_true <- -1
b_true <- 1.3
foo <- dgp_intercept(a0_true, a1_true, c_true, b_true, 1e7)
Z_tilde <- cbind(rep(1, nrow(foo)), foo$z)
T_tilde <- cbind(rep(1, nrow(foo)), foo$Tobs)
IV <- solve(crossprod(Z_tilde, T_tilde)) %*% crossprod(Z_tilde, foo$y)
c_plim <- c_true - a0_true * b_true / (1 - a0_true - a1_true)
b_plim <- b_true / (1 - a0_true - a1_true)

IV[1,] - c_plim
IV[2,] - b_plim
