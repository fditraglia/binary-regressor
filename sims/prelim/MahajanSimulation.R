set.seed(7298)
n <- 100000
epsilon <- rnorm(n)
beta <- 1.2
cc <- -0.5
a0 <- 0.2
a1 <- 0.4

n0 <- ceiling(n / 2)
z <- c(rep(0, n0), rep(1, n - n0))
p0 <- 0.2
p1 <- 0.7
Tstar <- (1 - z) * rbinom(n, 1, p0) + z * rbinom(n, 1, p1)
y <- cc + beta * Tstar + epsilon
Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1)

# check dgp
mean(Tstar[z == 0]) # p0
mean(Tstar[z == 1]) # p1
mean(Tobs[Tstar == 0]) # a0
1 - mean(Tobs[Tstar == 1]) # a1


theta1 <- beta / (1 - a0 - a1)
k1 <- cc - a0 * theta1
theta2 <- cc + theta1 * (1 - a1)
k2 <- -theta1 * a0 * (1 - a1)

# Check moment conditions
theta1_est <- cov(y, z) / cov(z, Tobs)
k1_est <- mean(y - theta1_est * Tobs)
c(theta1, theta1_est)
c(k1, k1_est)

theta2_est <- cov(y * Tobs, z) / cov(z, Tobs)
c(theta2, theta2_est)
k2_est <- mean(y * Tobs - theta2_est * Tobs)
c(k2, k2_est)

# Check a different way just to be extra safe!
mean(y - theta1 * Tobs) # should equal k1
k1

mean((y - k1 - theta1 * Tobs) * z) # should equal zero

mean(y * Tobs - theta2 * Tobs) # should equal k2
k2

mean((y * Tobs - k2 - theta2 * Tobs) * z) # should equal zero

# Check the relationship between parameters
c(theta2 - k1, theta2_est - k1_est, theta1 * (1 + a0 - a1))

c((theta2_est - k1_est) / theta1_est, (1 - a1) + a0)

# Estimator for beta
sqrt((theta2_est - k1_est)^2 + 4 * theta1_est * k2_est)
beta

# Simple simulation study to see if there's a weak identification problem
BBS_dgp <- function(a0, a1, beta, cc = 0, p0 = 0.15, p1 = 0.85, n = 1000) {
  epsilon <- rnorm(n)
  n0 <- ceiling(n / 2)
  z <- c(rep(0, n0), rep(1, n - n0))
  Tstar <- (1 - z) * rbinom(n, 1, p0) + z * rbinom(n, 1, p1)
  y <- cc + beta * Tstar + epsilon
  Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1)
  out <- data.frame(y = y, Tobs = Tobs, z = z, Tstar = Tstar, epsilon = epsilon)
  return(out)

}

BBS_est <- function(dat){
  y <- dat$y
  Tobs <- dat$Tobs
  z <- dat$z
  theta1_est <- cov(y, z) / cov(z, Tobs)
  k1_est <- mean(y - theta1_est * Tobs)
  theta2_est <- cov(y * Tobs, z) / cov(z, Tobs)
  k2_est <- mean(y * Tobs - theta2_est * Tobs)
  b_est <- sign(theta1_est) * sqrt((theta2_est - k1_est)^2 + 4 * theta1_est * k2_est)
  b_upper <- theta1_est
  b_lower <- cov(y, z)
  out <- c(est = b_est, lower = b_lower, upper = b_upper)
  return(out)
}

simple_sim <- function(beta) {
  dat <- BBS_dgp(0.1, 0.2, beta, n = 500)
  return(BBS_est(dat))
}

set.seed(10272)
n_reps <- 5000
beta_true <- 0.1
results <- t(replicate(n_reps, simple_sim(beta_true)))
results <- as.data.frame(results)
hist(results$est)
mean(results$est, na.rm = TRUE)

