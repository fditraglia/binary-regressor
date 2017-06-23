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

#bbin - Types of Variables
#0 = Continuous, 1 = Integer, 2 = Categorical, 3 = Binary

#bbout - Description of objective and constraints
#   Constraints that are not simply bounds are are imposed 
#   by feeding in a function that returns several values.
#   We need to indicate which of these values is the objective
#   functions and which are the constraints. We also need to 
#   indicate what kind of constraints we are using.
#
#   In the NOMAD documentation, these are indicated by:
#     OBJ = objective function
#     EB  = extreme barrier
#     PB  = progressive barrier
#   So far as I can tell from the examples, snomadr uses
#   the convention:
#     0 = OBJ
#     1 = EB
#     2 = PB
snomadr.default <- function(f){
  snomadr(eval.f = f,
          n = 2,
          x0 = c(0, 0),
          bbin = c(0, 0),
          bbout = c(0, rep(2, 6)),
          lb = c(0, -100),
          ub = c(0.9, 100),
          opts = list("MIN_MESH_SIZE" = 0.000001),
          #when enclosed in a function itself, snomadr gets 
          #confused by closures and needs to be explicitly
          #told to look in the calling environment
          snomadr.environment = parent.frame())
}



sim_CI <- function(a1_true, b_true, boot_samples){
  sim_dat <- dgp(a0 = 0, a1 = a1_true, b = b_true)
  #p0 <- with(sim_dat, mean(Tobs[z == 0]))
  #p1 <- with(sim_dat, mean(Tobs[z == 1]))
  #a1_upper <- with(sim_dat, min(1 - p0, 1 - p1))
  #Wald <- with(sim_dat, cov(y, z) / cov(Tobs, z))
  #Rf <- with(sim_dat, cov(y, z))
  
  f_lower <- function(params){
    a1 <- params[1]
    b <- params[2]
    results <- constraint_function(a1, b, sim_dat, boot_samples)
    return(c(results$beta, results$constraint))
  }
  f_upper <- function(params){
    a1 <- params[1]
    b <- params[2]
    results <- constraint_function(a1, b, sim_dat, boot_samples)
    return(c(-results$beta, results$constraint))
  }
  
  Lower <- snomadr.default(f = f_lower)
  Upper <- snomadr.default(f = f_upper)
                               
  out <- c(lower = Lower$solution[2], upper = Upper$solution[2])
  return(out)
}


library(mbereg)
library(crs)
set.seed(7234)
n <- 1000
B <- 2001
normal_sims <- rnorm(B * n)
normal_sims <- matrix(normal_sims, nrow = n, ncol = B)


sim_CIs <- replicate(2, sim_CI(0.1, 0.25, normal_sims))


