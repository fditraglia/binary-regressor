library(mbereg)
set.seed(23469)

a0_true <- 0.2
a1_true <- 0.3
b_true <- -2

nB <- 10000
set.seed(23469)
normal_sims1 <- matrix(rnorm(6 * nB), nrow = 6, ncol = nB)

set.seed(23469)
normal_sims2 <- matrix(rnorm(10 * nB), nrow = 10, ncol = nB)


sim_test1 <- function(a0_null, a1_null){
  sim_dat <- dgp(a0 = a0_true, a1 = a1_true, b = b_true)
  pvalue <- GMS_test_alphas(a0_null, a1_null, sim_dat, normal_sims1)
}

sim_test2 <- function(a0_null, a1_null){
  sim_dat <- dgp(a0 = a0_true, a1 = a1_true, b = b_true)
  pvalue <- GMS_test_alphas2(a0_null, a1_null, sim_dat, normal_sims2)
}

system.time(sim_test1(a0_true, a1_true))
system.time(sim_test2(a0_true, a1_true))

n_reps <- 5000
set.seed(8979)
#system.time(pvalues1 <- replicate(n_reps, sim_test1(a0_true, a1_true)))
#hist(pvalues1)

set.seed(8979)
#system.time(pvalues2 <- replicate(n_reps, sim_test2(a0_true, a1_true)))
#hist(pvalues2)


set.seed(7286)
dat <- dgp(a0_true, a1_true, b_true, n = 1000)

pvalues_grid <- function(a0_grid, a1_grid, one = TRUE){
  grid <- expand.grid(a0_grid, a1_grid)
  names(grid) <- c('a0', 'a1')
  grid <- subset(grid, a0 + a1 < 1)
  if(one){
    get_p_value_i <- function(i) GMS_test_alphas(grid$a0[i], grid$a1[i], dat, normal_sims1)
  } else {
    get_p_value_i <- function(i) GMS_test_alphas2(grid$a0[i], grid$a1[i], dat, normal_sims2)
  }
  pvalues <- sapply(1:nrow(grid), get_p_value_i)
  return(cbind(grid, pvalues))
}

get_alphas_CI <- function(delta1, coarse_inc = 0.05, fine_inc = 0.01, ...){
  coarse_grid <- seq(0, 1, coarse_inc)
  coarse <- pvalues_grid(coarse_grid, coarse_grid, ...)
  coarse_CI <- subset(coarse, pvalues >= delta1)
  a0_fine <- seq(max(min(coarse_CI$a0) - coarse_inc, 0),
                 max(coarse_CI$a0) + coarse_inc,
                 fine_inc)
  a1_fine <- seq(max(min(coarse_CI$a1) - coarse_inc, 0),
                 max(coarse_CI$a1) + coarse_inc,
                 fine_inc)
  fine <- pvalues_grid(a0_fine, a1_fine, ...)
  return(subset(fine, pvalues >= delta1))
}

get_s_CI <- function(delta1, ...){
  fine_CI <- get_alphas_CI(delta1 = delta1, ...)
  return(c(with(fine_CI, min(1 - a0 - a1)),
           with(fine_CI, max(1 - a0 - a1))))
}



# The following is a 95% Bonferroni interval
significance_level <- 0.05 # overall level
delta <- 0.025 # level for the wald interval

iv <- AER::ivreg(y ~ Tobs | z, data = dat)
wald <- coefficients(iv)[2]
se <- (summary(iv)$coefficients)[2,2]
UCL <- wald + qnorm(1 - delta / 2) * se
LCL <- wald - qnorm(1 - delta / 2) * se

#system.time(s_CI <- get_s_CI(significance_level - delta, coarse_inc = 0.01,
#                             fine_inc = 0.001))

#bonf_LCL <- min(LCL * min(s_CI), LCL * max(s_CI))
#bonf_UCL <- max(UCL * min(s_CI), UCL * max(s_CI))

#b_true
#wald
#wald + c(-1, 1) * qnorm(1 - significance_level / 2) * se
#c(LCL, UCL)
#c(bonf_LCL, bonf_UCL)

first <- get_alphas_CI(significance_level - delta)[,1:2]
second <- get_alphas_CI(significance_level - delta, one = FALSE)[,1:2]
plot(first, col = 'red', pch = 20, xlim = c(0, 1), ylim = c(0, 1))
points(second, col = 'blue', xlim = c(0, 1), ylim = c(0, 1))
points(a0_true, a1_true, col = 'white', pch = 20)
