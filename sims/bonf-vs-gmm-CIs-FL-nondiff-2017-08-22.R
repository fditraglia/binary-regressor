library(mbereg)
library(parallel)

b_seq <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.5, 1)
n_seq <- c(1000, 2000)
a0_seq <- a1_seq <- c(0, 0.1, 0.2, 0.3)
d_seq <- c(0.15)
rho_seq <- c(0) # Exogenous regressor!
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

get_CIs_i <- function(i) {
  sim_bonf_vs_gmm(true_params = sim_params[i,], normal_sims = normal_sims,
                  ncores = 36, nreps = 2000, delta1 = 0.025, delta2 = 0.025,
                  test_alphas = GMS_test_alphas_nondiff_FL, get_gmm = GMM_exog)
}

CIs <- lapply(1:nrow(sim_params), get_CIs_i)

results <- list(params = sim_params, CIs = CIs)
setwd('~/binary-regressor/sims/')
save(results, file = 'bonf-vs-gmm-CIs-FL-nondiff-2017-08-22.Rd')

