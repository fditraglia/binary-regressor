library(mbereg)

b_seq <- c(0, 0.25, 0.5, 1, 2)
n_seq <- c(1000, 2000)
a0_seq <- a1_seq <- c(0, 0.1, 0.2, 0.3)
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

get_GMM_CI_i <- function(i) {
  sim_GMM_endog(true_params = sim_params[i,], delta = 0.05, ncores = 8, nreps = 5000)
}
CIs_GMM <- lapply(1:nrow(sim_params), get_GMM_CI_i)

results <- list(params = sim_params, CIs_GMM = CIs_GMM)

setwd('~/binary-regressor/sims/')
save(results, file = 'unconstrained-GMM-CIs-2017-08-16.Rd')
