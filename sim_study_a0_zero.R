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

b_true <- 4
a1_true <- 0.1

set.seed(1983)
dat <- dgp(a1_true, b_true, 10000000)
n <- nrow(dat)
q <- with(dat, mean(z))
p <- with(dat, mean(Tobs))
mu <- with(dat, mean(y))
s <- with(dat, mean(y^2))
r <- with(dat, mean(y * Tobs))

s_T_z <- with(dat, cov(Tobs, z))
s_y_z <- with(dat, cov(y, z))
s_y2_z <- with(dat, cov(y^2, z))
s_yT_z <- with(dat, cov(y * Tobs, z))

b <- 2 * s_yT_z / s_T_z - s_y2_z / s_y_z
a1 <- 1 - 2 * s_yT_z / s_y_z + s_y2_z * s_T_z / s_y_z^2
W <- s_y_z / s_T_z

G_theta <- matrix(c(-s_T_z / (1 - a1), 
                    -b * s_T_z / (1 - a1)^2,
                    2 * (b * s_T_z - s_yT_z) / (1 - a1), 
                    (b^2 * s_T_z - 2 * b * s_yT_z) / (1 - a1)^2), 
                  nrow = 2, ncol = 2, byrow = TRUE)

G_gamma <- matrix(c(p * b / (1 - a1) - mu, 
                    q * b / (1 - a1),
                    -q, 0, 0,
                    (b / (1 - a1)) * (2 * r - b * p) - s,
                    -q * b^2 / (1 - a1) , 
                    0, -q,
                    2 * b * q / (1 - a1)), nrow = 2, ncol = 5, byrow = TRUE) 

G_theta_inv <- solve(G_theta)
FF_g <- cbind(G_theta_inv, G_theta_inv %*% G_gamma)
FF_h <- cbind(matrix(0, nrow = 5, ncol = 2), diag(-1, 5))
FF <- rbind(FF_g, FF_h)

g1 <- with(dat, (z * y - q * mu) - (b / (1 - a1)) * (z * Tobs - q * p))
g2 <- with(dat, (z * y^2 - q * s) -
             2 * (b / (1 - a1)) * (z * y * Tobs - q * r) +
             (b^2 / (1 - a1)) * (z * Tobs - q * p))
h1 <- with(dat, z - q) 
h2 <- with(dat, Tobs - p)
h3 <- with(dat, y - mu)
h4 <- with(dat, y^2 - s)
h5 <- with(dat, y * Tobs - r)
f_hat <- cbind(g1, g2, h1, h2, h3, h4, h5)
Omega <- crossprod(f_hat) / n

V <- FF %*% Omega %*% t(FF)
V_theta <- V[1:2, 1:2]

est <- function(dat){
  s_T_z <- with(dat, cov(Tobs, z))
  s_y_z <- with(dat, cov(y, z))
  s_y2_z <- with(dat, cov(y^2, z))
  s_yT_z <- with(dat, cov(y * Tobs, z))
  b <- 2 * s_yT_z / s_T_z - s_y2_z / s_y_z
  a1 <- 1 - 2 * s_yT_z / s_y_z + s_y2_z * s_T_z / s_y_z^2
  return(c('b' = b, 'a1' = a1))
}

sample_size <- 1000
sim_draws <- t(replicate(10000, est(dgp(a1_true, b_true, sample_size))))
qqnorm(sim_draws[,1])
sd(sim_draws[,1])
sqrt(V_theta[1, 1] / sample_size) 

qqnorm(sim_draws[,2])
sd(sim_draws[,2])
sqrt(V_theta[2, 2] / sample_size) 
