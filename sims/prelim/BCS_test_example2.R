# Compared to BCS_test_example.R, here we incorporate the second moment equality

BCS_test <- function(beta_null, dat, zeta){
  n <- nrow(dat)
  q <- with(dat, mean(z))
  x1 <- with(dat, Tobs * z)
  s1 <- sd(x1)
  x2 <- with(dat, y * z)
  x3 <- with(dat, 2 * y * Tobs * z)
  x4 <- with(dat, (y^2 - 1) * z)
  X <- cbind(x1, x2, x3, x4)
  M_hat <- var(X)
  X_bar <- colMeans(X)
  kappa_n <- sqrt(log(n))
  Xi <- crossprod(zeta, scale(X, scale = FALSE, center = TRUE)) / sqrt(n)
  Xi_tilde <- Xi + matrix(X_bar * sqrt(n) / kappa_n, B, ncol(X), byrow = TRUE)
  
  Qn <- function(a1){
    m1 <- q * (1 - a1) - X_bar[1]
    term1 <- (m1 < 0) * m1^2 / M_hat[1,1]
    theta1 <- beta_null / (1 - a1)
    nu2 <- c(-theta1, 1, 0, 0)
    term2 <- drop(crossprod(nu2, X_bar))^2 / drop(crossprod(nu2, M_hat) %*% nu2)
    theta2 <- beta_null * theta1
    nu3 <- c(theta2, 0, -theta1, 1)
    term3 <- drop(crossprod(nu3, X_bar))^2 / drop(crossprod(nu3, M_hat) %*% nu3)
    return(n * (term1 + term2 + term3))
  }
  
  # We never optimize over this function, so I calculate the values for all
  # xi simultaneously
  Qn_DR <- function(a1){
    drop_ineq <- (sqrt(n) * (q * (1 - a1) - X_bar[1]) / (s1 * kappa_n)) > 1
    if(drop_ineq){
      term1 <- 0
    } else {
      term1 <- pmin(-Xi[,1] / s1, 0)^2
    }
    theta1 <- beta_null / (1 - a1)
    nu2 <- c(-theta1, 1, 0, 0)
    term2 <- drop(Xi %*% nu2)^2 / drop(crossprod(nu2, M_hat) %*% nu2)
    theta2 <- beta_null * theta1
    nu3 <- c(theta2, 0, -theta1, 1)
    term3 <- drop(Xi %*% nu3)^2 / drop(crossprod(nu3, M_hat) %*% nu3)
    return(term1 + term2 + term3)
  }
  
  # Since we will optimize this for each bootstrap draw, I have written this 
  # differently from Qn_DR so that we evaluate for a fixed 
  Qn_PR <- function(a1, xi_tilde){
    term1 <- min(0, (sqrt(n) * q * (1 - a1) / kappa_n - xi_tilde[1]) / s1)^2
    theta1 <- beta_null / (1 - a1)
    nu2 <- c(-theta1, 1, 0, 0)
    term2 <- sum(nu2 * xi_tilde)^2 / drop(crossprod(nu2, M_hat) %*% nu2)
    theta2 <- beta_null * theta1
    nu3 <- c(theta2, 0, -theta1, 1)
    term3 <- sum(nu3 * xi_tilde)^2 / drop(crossprod(nu3, M_hat) %*% nu3)
    return(term1 + term2 + term3)
  }
  
  if(isTRUE(all.equal(0, beta_null))){
    Tn <- Qn(0)
    
    nu2 <- c(0, 1, 0, 0)
    term2 <- drop(Xi %*% nu2)^2 / drop(crossprod(nu2, M_hat) %*% nu2)
    nu3 <- c(0, 0, 0, 1)
    term3 <- drop(Xi %*% nu3)^2 / drop(crossprod(nu3, M_hat) %*% nu3)
    Tn_DR <- Tn_PR <- term2 + term3
  } else {
    Qn_optimization <- optimize(Qn, lower = 0, upper = 0.99)
    Tn <- Qn_optimization$objective
    Tn_DR <- Qn_DR(Qn_optimization$minimum)
    Tn_PR <- apply(Xi_tilde, 1, function(x) optimize(function(a1) Qn_PR(a1, x), 
                                                     lower = 0, upper = 0.99)$objective)
  }
  Tn_MR <- pmin(Tn_DR, Tn_PR)
  return(mean(Tn_MR >= Tn))
}

library(mbereg)

# True parameter values
a1_true <- 0.1
beta_true <- 0.25
d <- 0.15
p0_star <- d
p1_star <- 1 - d
RF <- beta_true * (p1_star - p0_star)
Wald <- beta_true / (1 - a1_true)
b_lower <- RF * p1_star / (p1_star - p0_star)
b_upper <- Wald

set.seed(72349)
n <- 1000
B <- 5001
myzeta <- matrix(rnorm(n * B), n, B)

sim_test_null <- function(){
  mydat <- dgp(a0 = 0, a1 = a1_true, b = beta_true, n = n)
  p_lower <- BCS_test(b_lower, mydat, myzeta)
  p_true <- BCS_test(beta_true, mydat, myzeta)
  p_upper <- BCS_test(b_upper, mydat, myzeta)
  out <- c(lower = p_lower, true = p_true, upper = p_upper)
  return(out)
}

sim_test_alternative <- function(){
  mydat <- dgp(a0 = 0, a1 = a1_true, b = beta_true, n = n)
  p_0 <- BCS_test(0, mydat, myzeta)
  p_hundredth <- BCS_test(0.01, mydat, myzeta)
  p_tenth <- BCS_test(0.1, mydat, myzeta)
  out <- c(zero = p_0, hundredth = p_hundredth, tenth = p_tenth)
  return(out)
}

library(parallel)
RNGkind("L'Ecuyer-CMRG")
set.seed(12871)

n_reps <- 320
#results <- replicate(n_reps, sim_test_alternative())
#results <- replicate(n_reps, sim_test_null())
system.time(results <- parallel::mclapply(1:n_reps, function(i) sim_test_alternative(), 
                                          mc.cores = 8))
results <- as.data.frame(do.call(rbind, results))
#results <- as.data.frame(t(results))

apply(results, 2, function(pvalue) mean(pvalue <= 0.05))
