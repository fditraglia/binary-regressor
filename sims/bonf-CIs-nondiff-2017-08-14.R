library(mbereg)
library(parallel)

#b_seq <- c(0, 0.25, 0.5, 1, 2)
b_seq <- c(2)
#n_seq <- c(1000, 2000)
n_seq <- c(2000)
#a0_seq <- a1_seq <- c(0, 0.1, 0.2, 0.3)
a0_seq <- a1_seq <- c(0.1)
d_seq <- c(0.15)
rho_seq <- c(0.5)
cc_seq <- c(0)

sim_params <- expand.grid(b = b_seq,
                          n = n_seq,
                          a0 = a0_seq,
                          a1 = a1_seq,
                          d = d_seq,
                          rho = rho_seq,
                          cc = cc_seq)
set.seed(619283)
nB <- 5000
normal_sims <- matrix(rnorm(nB * 14), nrow = 14, ncol = nB)

get_bonf_CI_i <- function(i) {
  sim_bonf_CI(true_params = sim_params[i,], normal_sims, ncores = 8,
              nreps = 1000, delta1 = 0.05, delta2 = 0.05,
              test_alphas = GMS_test_alphas_nondiff)
}
CIs_bonf <- lapply(1:nrow(sim_params), get_bonf_CI_i)

results <- list(params = sim_params, CIs_bonf = CIs_bonf)
setwd('~/binary-regressor/sims/')
save(results, file = 'bonf-CIs-nondiff-2017-08-14.Rd')


