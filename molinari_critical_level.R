# Special case in which a0 = 0 and k1 and k2 are known to be 0 and 1 respectively
constraint_function <- function(a1, beta, dat, Chi, confidence_level = 0.05,
                                bias = 0.01, tol = 0.01){
  theta1 <- beta / (1 - a1)
  theta2 <- beta^2 / (1 - a1)
  B <- ncol(Chi)
  
  Tobs <- dat$Tobs
  y <- dat$y
  z <- dat$z
  q <- mean(z) # treat this as fixed in repeated sampling
  n <- nrow(dat)
  
  # Pretend for now that we know the intercepts
  k1 <- 0
  k2 <- 1
  
  get_u1 <- function(a1, beta) {
    theta1 <- beta / (1 - a1)
    y - k1 - theta1 * Tobs
  }
  get_u2 <- function(a1, beta) {
    theta1 <- beta / (1 - a1)
    theta2 <- beta^2 / (1 - a1)
    y^2 - k2 - theta1 * 2 * y * Tobs + theta2 * Tobs
  }
  
  m1 <- Tobs * (1 - z) / (1 - q) - (1 - a1)  
  m2 <- Tobs * z / q - (1 - a1)  
  m1_bar <- mean(m1)
  m2_bar <- mean(m2)
  s1 <- sd(m1)
  s2 <- sd(m2)
  
  m3 <- get_u1(a1, beta) * z
  m4 <- get_u2(a1, beta) * z
  m3_bar <- mean(m3)
  m4_bar <- mean(m4)
  s3 <- sd(m3)
  s4 <- sd(m4)
  
  eta1 <- (m1 - m1_bar) / s1
  eta2 <- (m2 - m2_bar) / s2
  eta3 <- (m3 - m3_bar) / s3
  eta4 <- (m4 - m4_bar) / s4
  eta <- cbind(eta1, eta2, eta3, eta4, -eta3, -eta4)
  G <- crossprod(eta, Chi) / sqrt(n)
  
  m3_over_s3 <- function(params) {
    a1 <- params[1]
    beta <- params[2]
    u1 <- get_u1(a1, beta)
    numerator <- mean(u1 * z)
    denominator <- sd(u1 * z)
    return(numerator / denominator)
  }
  
  m4_over_s4 <- function(params) {
    a1 <- params[1]
    beta <- params[2]
    u2 <- get_u2(a1, beta)
    numerator <- mean(u2 * z)
    denominator <- sd(u2 * z)
    return(numerator / denominator)
  }
  
  D1 <- c(1 / s1, 0)
  D2 <- c(1 / s2, 0)
  D3 <- numDeriv::grad(m3_over_s3, x = c(a1, beta))
  D4 <- numDeriv::grad(m4_over_s4, x = c(a1, beta))
  D <- rbind(D1, D2, D3, D4, -D3, -D4)
  
  # Moment selection
  kappa_n <- sqrt(log(n))
  xi1 <- sqrt(n) * m1_bar / (kappa_n * s1)
  xi2 <- sqrt(n) * m2_bar / (kappa_n * s2)
  xi <- c(xi1, xi2)
  phi <- matrix(c(ifelse(xi >= -1, 0, -Inf), 0, 0, 0, 0), 6, 1)
  keepRows <- which(is.finite(drop(phi)))
  G_gms <- G[keepRows,]
  D_gms <- D[keepRows,]
  
  # Calculate boundaries of "rho-box"
  r <- -qnorm(0.5 * (1 - (1 - bias)^(1/6)))
  
  # Re-center lambda so that lambda_tilde >= 0
  shift <- r * D_gms[,1] 
  bootRHS_gms <- apply(G_gms, 2, function(column) shift - column)
  
  
  
  # These are fixed!
  f.con <- matrix(c(D_gms[,1], 1), ncol = 1)
  f.dir <- rep('<=', nrow(f.con))
  f.obj <- rep(1, ncol(f.con))
  
  # This changes with each bootstrap draw and depends on c_t  
  solve_LP <- function(c_t, boot_indices){
    unlist(lapply(boot_indices, function(i)
      lpSolve::lp("max", f.obj, f.con, f.dir, c(c_t + bootRHS_gms[,i], 2 * r))$status))
  }
  
  # Starting values
  c_L <- 0
  c_U <- qnorm(1 - confidence_level / 6) 
  p_L <- 0
  p_U <- 1
  c_current <- mean(c(c_U, c_L))
  check_me <- 1:B
  
  n_bisect <- ceiling(log2((c_U - c_L) / tol))
  
  for(i in 1:n_bisect){
    status <- solve_LP(c_current, check_me)
    p_current <- sum(status == 0) / B + p_L 
    if(p_current < 1 - confidence_level) {
      p_L <- p_current
      c_L <- c_current
      check_me <- check_me[status == 2] # status == 2 if the LP is infeasible
    } else {
      p_U <- p_current
      c_U <- c_current
      check_me <- check_me[status == 0] # status == 0 if the LP is feasible
    }
    c_current <- mean(c(c_U, c_L))
  }
  
  m_bar <- c(mean(m1), mean(m2), mean(m3), mean(m4), -mean(m3), -mean(m4))
  constraint <- sqrt(n) * m_bar - c_current
  return(list(constraint = constraint, c_hat = c_current, p = p_current, 
              a1 = a1, beta = beta))
}

library(mbereg)
set.seed(71234)
n <- 1000
sim_dat <- dgp(a0 = 0, a1 = 0.1, b = 0.25, n = n)

B <- 2001
normal_sims <- rnorm(B * n)
normal_sims <- matrix(normal_sims, nrow = n, ncol = B)

constraint_function(a1 = 0.1, beta = 0.25, dat = sim_dat, Chi = normal_sims)
constraint_function(a1 = 0.7, beta = 2, dat = sim_dat, Chi = normal_sims)
