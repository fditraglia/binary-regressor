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


get_V_true <- function(a1, b, d = 0.15, n_sims = 1000000){

  dat <- dgp(a1, b, n_sims, d)
  n <- nrow(dat)
  q <- 0.5
  p1_star <- 1 - d
  p0_star <- d
  p_star <- p1_star * q + p0_star * (1 - q)
  p <- (1 - a1) * p_star
  mu <- b * p_star
  s <- with(dat, mean(y^2))
  r <- with(dat, mean(y * Tobs))

  s_T_z <- (1 - a1) * (p1_star - p0_star) * q * (1 - q)
  s_y_z <- b * (p1_star - p0_star) * q * (1 - q)
  s_y2_z <- with(dat, cov(y^2, z))
  s_yT_z <- with(dat, cov(y * Tobs, z))

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
  return(V_theta)
}


est <- function(dat){
  #reg_RF <- lm(y ~ z, dat)
  #summary_RF <- summary(RF)
  #RF <- summary_RF$coefficients[2, 1]
  #RF_SE <- summary_RF$coefficients[2, 2]

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
  b_SE <- sqrt(V_theta[1, 1] / n)
  a1_SE <- sqrt(V_theta[2, 2] / n)
  return(c('b' = b, 'b_SE' = b_SE, 'a1' = a1, 'a1_SE' = a1_SE))
}

plot_dist <- function(a1, b, n, d = 0.15, rho = 0.5){
  V_true <- get_V_true(a1, b, d)
  b_SE_true <- sqrt(V_true[1, 1] / n)
  a1_SE_true <- sqrt(V_true[2, 2] / n)
  RF_SE_true <- sqrt(4 / n)
  sim_draws <- as.data.frame(t(replicate(10000, est(dgp(a1, b, n, d)))))
  par(mfrow = c(1, 2))
  par(oma = c(0, 0, 2, 0))

  MASS::truehist(sim_draws$b, col = 'lightskyblue', xlab = expression(beta))
  b_bias_text <- paste0('Bias = ', round(mean(sim_draws$b) - b, 3))
  b_SD_text <- paste0('SD = ', round(sd(sim_draws$b), 3))
  title(paste(b_bias_text, ',', b_SD_text), font.main = 1, cex.main = 1)
  b_seq <- seq(from = min(sim_draws$b), to = max(sim_draws$b), length.out = 1000)
  b_true_density <- dnorm(b_seq, mean = b, sd = b_SE_true)
  points(b_seq, b_true_density, type = 'l', lwd = 3, lty = 1)
  abline(v = b, lty = 1, lwd = 3, col = 'firebrick')

  MASS::truehist(sim_draws$a1, col = 'lightskyblue', xlab = expression(alpha[1]))
  a1_bias_text <- paste0('Bias = ', round(mean(sim_draws$a1) - a1, 3))
  a1_SD_text <- paste0('SD = ', round(sd(sim_draws$a1), 3))
  title(paste(a1_bias_text, ',', a1_SD_text), font.main = 1, cex.main = 1)
  a1_seq <- seq(from = min(sim_draws$a1), to = max(sim_draws$a1), length.out = 1000)
  a1_true_density <- dnorm(a1_seq, mean = a1, sd = a1_SE_true)
  points(a1_seq, a1_true_density, type = 'l', lwd = 3, lty = 1)
  abline(v = a1, lty = 1, lwd = 3, col = 'firebrick')
  mytitle <- substitute(paste(beta, ' = ', my_beta, ', ', alpha[1],
                               ' = ', my_alpha1, ', ',
                              delta, ' = ', my_delta,
                              ', ', n, ' = ', my_n),
                        list(my_beta = b, my_alpha1 = a1, my_n = n,
                             my_delta = d))
  title(main = mytitle, outer = T)
  par(mfrow = c(1, 1))
  par(oma = c(0, 0, 0, 0))
}

set.seed(1983)
plot_dist(a1 = 0.1, b = 3, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 2, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 1, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 0.9, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 0.8, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 0.7, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 0.6, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 0.5, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 0.4, n = 1000, d = 0.15)
plot_dist(a1 = 0.1, b = 0.3, n = 1000, d = 0.15)

