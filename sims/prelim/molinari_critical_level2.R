# Compared to molinari_critical_level3.R, this version pretends that we know k1
# (which is equal to zero in this case) and hence avoide preliminary estimation.
# Like the other version, this script uses only first moment information so that
# beta cannot be identified. It also sets a0 = 0.
library(mbereg)
library(nloptr)
#----------------------------------------------------------------------------
# Kaido, Molinari & Stoye (2016) constraint function
#----------------------------------------------------------------------------
constraint_function_fast <- function(a1, beta, dat, normal_draws, 
                                     confidence_level = 0.05, bias = 0.01, tol = 0.01){
  theta1 <- beta / (1 - a1)
  B <- ncol(normal_draws)
  
  Tobs <- dat$Tobs
  y <- dat$y
  z <- dat$z
  q <- mean(z) # treat this as fixed in repeated sampling
  n <- nrow(dat)
  
  x1 <- Tobs * z
  x2 <- y * z
  X <- cbind(x1, x2)
  w1 <- Tobs
  w2 <- y
  W <- cbind(w1, w2)
   
  k1 <- 0
  m1 <- Tobs * z / q - (1 - a1)
  m1_bar <- mean(m1)
  m2 <- (y - k1 - theta1 * Tobs) * z
  m2_bar <- mean(m2)
  
  Sigma <- var(cbind(m1, m2, -m2))
  s <- sqrt(diag(Sigma))
  Omega <- cov2cor(Sigma)
  
  # Calculate G (limit bootstrap analogue of empirical process)
  Omega_sqrt <- sqrtm(Omega)
  G <- Omega_sqrt %*% normal_draws
  
  # Calculate D_n (derivative matrix) 
  D1 <- c(q / sd(Tobs * z), 0)
  M2 <- var(X - cbind(0, k1 * z))
  mu <- colMeans(X - cbind(0, k1 * z))
  nu <- c(-theta1, 1)
  M2_norm <- t(nu) %*% M2 %*% nu
  D_g <- (t(mu) - drop(crossprod(nu, mu) / M2_norm) * crossprod(nu, M2)) / sqrt(drop(M2_norm))
  D_nu <- matrix(c(-beta / ((1 - a1)^2), -1 / (1 - a1), 
                   0, 0), 2, 2, byrow = TRUE) 
  D2 <- D_g %*% D_nu 
  D <- rbind(D1, D2, -D2)
  
  # Moment selection
  kappa_n <- sqrt(log(n))
  xi <- sqrt(n) * m1_bar / (kappa_n * s[1])
  phi <- matrix(c(ifelse(xi >= -1, 0, -Inf), 0, 0), 3, 1)
  keepRows <- which(is.finite(drop(phi)))
  G_gms <- G[keepRows,]
  D_gms <- D[keepRows,]
  
  # Calculate boundaries of "rho-box"
  d <- 2
  J1 <- 1
  J2 <- 1
  pow <- 1 / (d * choose(J1 + J2, d))
  r <- -qnorm(0.5 * (1 - (1 - bias)^(pow)))
  
  # Re-center lambda so that lambda_tilde >= 0
  shift <- r * D_gms[,1] 
  bootRHS_gms <- apply(G_gms, 2, function(column) shift - column)
  
  # Test feasibility of LP "by hand" simultaneously for all bootstrap samples
  LHS <- c(D_gms[,1], 1)
  lower <- sign(LHS) == -1
  LHS_inverse <- diag(1 / LHS)
  
  check_feasible <- function(c_t, boot_indices){
    stopifnot(length(boot_indices) > 0)
    RHS_mat <- rbind(c_t + bootRHS_gms[, boot_indices, drop = FALSE], 
                     rep(2 * r, length(boot_indices)))
    bounds_mat <- LHS_inverse %*% RHS_mat
    if(any(lower)){
      GLBs <- apply(bounds_mat[lower,, drop = FALSE], 2, function(x) max(c(x, 0)))
    } else {
      GLBs <- rep(0, length(boot_indices))
    }
    LUBs <- apply(bounds_mat[!lower,, drop = FALSE], 2, min)
    return(GLBs <= LUBs)
  }
  # Starting values
  c_L <- 0
  c_U <- qnorm(1 - confidence_level / (J1 + 2  * J2)) 
  p_L <- 0
  p_U <- 1
  c_current <- mean(c(c_U, c_L))
  check_me <- 1:B
  
  n_bisect <- ceiling(log2((c_U - c_L) / tol))
  
  for(i in 1:n_bisect){
    if(length(check_me > 1)){
      feasible <- check_feasible(c_current, check_me)
      p_current <- sum(feasible) / B + p_L 
      if(p_current < 1 - confidence_level) {
        p_L <- p_current
        c_L <- c_current
        check_me <- check_me[!feasible] 
      } else {
        p_U <- p_current
        c_U <- c_current
        check_me <- check_me[feasible] 
      }
      c_current <- mean(c(c_U, c_L))
    }
  }
  constraint <- sqrt(n) * c(m1_bar, m2_bar, -m2_bar) / s - c_current
  return(list(constraint = constraint, c_hat = c_current, p = p_current, 
              a1 = a1, beta = beta))
}


set.seed(7234)
B <- 4001
J <- 3
normal_sims <- rnorm(J * B)
normal_sims <- matrix(normal_sims, nrow = J, ncol = B)

eval_f_lower <- function(x) x[2]
eval_f_upper <- function(x) -x[2]


simCI <- function(a1, beta, n = 1000, confidence_level = 0.05){
  sim_dat <- dgp(a0 = 0, a1, beta, n) 
  Wald <- with(sim_dat, cov(y, z) / cov(z, Tobs))
  Rf <- with(sim_dat, cov(y, z) / var(z))
  p1 <- with(sim_dat, mean(Tobs[z == 1]))
  p0 <- with(sim_dat, mean(Tobs[z == 0]))
  q <- with(sim_dat, mean(z))
  SE <- with(sim_dat, sd(Tobs * z / q)) / sqrt(nrow(sim_dat))  
  c_bar <- qnorm(1 - confidence_level / 3)
  a1_bar <- (1 - p1) + c_bar * SE
  eval_g0 <- function(x) constraint_function_fast(x[1], x[2], sim_dat, normal_sims,
                                                  confidence_level = confidence_level)$constraint 
  system.time(lower <- nloptr(x0 = c(1 - p1, Rf * p1 / (p1 - p0)), 
                  eval_f = eval_f_lower, 
                  lb = c(-0.02, -Inf),
                  ub = c(a1_bar, Inf), eval_g_ineq = eval_g0,
                  opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-4)))
  #$solution[2]
  system.time(upper <- nloptr(x0 = c(0, Wald), 
                  eval_f = eval_f_upper, 
                  lb = c(-0.02, -Inf),
                  ub = c(a1_bar, Inf), eval_g_ineq = eval_g0,
                  opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-4)))
  #$solution[2]
  return(c(lower = lower, upper = upper))
}


library(parallel)
a1_true <- 0.1
beta_true <- 1
n_reps <- 100
simCI(a1_true, beta_true)
baz <- replicate(n_reps, simCI(a1_true, beta_true, n = 100000))
#baz <- mclapply(1:n_reps, function(i) simCI(a1_true, beta_true, n = 100000), mc.cores = 8)
baz <- do.call(rbind, baz)
baz <- as.data.frame(baz)

# Note that this identified set is specific to one-sided mis-classification!
d <- 0.15
p0_star <- d
p1_star <- 1 - d
RF <- beta_true * (p1_star - p0_star)
Wald <- beta_true / (1 - a1_true)
I_lower <- RF * p1_star / (p1_star - p0_star)
I_upper <- Wald

median(abs(baz$upper - baz$lower))
baz_cover <- function(b) mean((baz$lower <= b) & (baz$upper >= b))
V_baz_cover <- Vectorize(baz_cover)
b_seq <- seq(I_lower - 0.1, I_upper + 0.1, 0.001)
cover <- V_baz_cover(b_seq)
plot(b_seq, cover, type = 'l')
abline(v = I_lower, lty = 2)
abline(v = I_upper, lty = 2)
abline(h = 0.95, lty = 2)



setwd("~/binary-regressor/sims/")
sim_CIs <- replicate(500, sim_CI(0.1, 0.25, normal_sims))
sim_CIs <- t(sim_CIs)
save(sim_CIs, file = "molinari_CIs_old_2017_07_04.RData")
rm(list = ls())



