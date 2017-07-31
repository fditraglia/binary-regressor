# This is a simple example to test the procedure from Bugni, Canay & Shi (2017)
# We examine the partially identified case using a single moment equality, the
# IV estimate, and a single moment inequality, the upper bound for a1 based on 
# p1. This assumes that a0 is zero and we know in advance which of the two 
# inequality constraints for a1 is the tighter.

BCS_test <- function(beta_null, dat, zeta){
  n <- nrow(dat)
  q <- with(dat, mean(z))
  x1 <- with(dat, Tobs * z)
  s1 <- sd(x1)
  x2 <- with(dat, y * z)
  X <- cbind(x1, x2)
  M_hat <- var(X)
  X_bar <- colMeans(X)
  kappa_n <- sqrt(log(n))
  Xi <- crossprod(zeta, scale(X, scale = FALSE, center = TRUE)) / sqrt(n)
  Xi_tilde <- Xi + matrix(X_bar * sqrt(n) / kappa_n, B, 2, byrow = TRUE)
  
  Qn <- function(a1){
    m1 <- q * (1 - a1) - X_bar[1]
    term1 <- (m1 < 0) * m1^2 / M_hat[1,1]
    nu <- c(-beta_null / (1 - a1), 1)
    term2 <- drop(crossprod(nu, X_bar))^2 / drop(crossprod(nu, M_hat) %*% nu)
    return(n * (term1 + term2))
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
    nu <- c(-beta_null / (1 - a1), 1)
    term2 <- drop(Xi %*% nu)^2 / drop(crossprod(nu, M_hat) %*% nu)
    return(term1 + term2)
  }
  
  # Since we will optimize this for each bootstrap draw, I have written this 
  # differently from Qn_DR so that we evaluate for a fixed 
  Qn_PR <- function(a1, xi_tilde){
    term1 <- min(0, (sqrt(n) * q * (1 - a1) / kappa_n - xi_tilde[1]) / s1)^2
    nu <- c(-beta_null / (1 - a1), 1)
    term2 <- sum(nu * xi_tilde)^2 / drop(crossprod(nu, M_hat) %*% nu)
    return(term1 + term2)
  }
  
  if(isTRUE(all.equal(0, beta_null))){
    Tn <- Qn(0)
    nu <- c(0, 1)
    Tn_DR <- Tn_PR <- drop(Xi %*% nu)^2 / drop(crossprod(nu, M_hat) %*% nu)
  } else {
    Qn_optimization <- optimize(Qn, lower = 0, upper = 0.99)
    Tn <- Qn_optimization$objective
    
    Qn_0 <- Qn(0)
    tol <- 1 / sqrt(log(n))
    
    #if(abs(Qn_0 - Tn) < tol){
      #Tn_DR <- pmin(Qn_DR(Qn_optimization$minimum), Qn_DR(0))
    #} else {
      Tn_DR <- Qn_DR(Qn_optimization$minimum)
    #}
    
    Tn_PR <- apply(Xi_tilde, 1, function(x) optimize(function(a1) Qn_PR(a1, x), 
                                                     lower = 0, upper = 0.99)$objective)
  }
  Tn_MR <- pmin(Tn_DR, Tn_PR)
  return(mean(Tn_MR >= Tn))
}

library(mbereg)

# True parameter values
a1_true <- 0.1
beta_true <- 0.01
#beta_true <- 0.25
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

n_reps <- 1000
#results <- replicate(n_reps, sim_test_alternative())
#results <- replicate(n_reps, sim_test_null())
system.time(results <- parallel::mclapply(1:n_reps, function(i) sim_test_alternative(), 
                                          mc.cores = 8))
results <- as.data.frame(do.call(rbind, results))
#results <- as.data.frame(t(results))

apply(results, 2, function(pvalue) mean(pvalue <= 0.05))

qqunif <- function(x){
  unif_quantiles <- seq(0, 1, 0.001)
  x_quantiles <- quantile(x, unif_quantiles)
  plot(unif_quantiles, x_quantiles, main = "Uniform Q-Q Plot", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  points(unif_quantiles, unif_quantiles, type = 'l')
}





