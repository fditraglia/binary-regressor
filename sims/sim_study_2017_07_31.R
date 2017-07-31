library(mbereg)
library(parallel)

#b_seq <- c(0, 0.2, 0.5, 1, 2)
b_seq <- c(0, 0.2)
#n_seq <- c(1000, 5000)
n_seq <- c(1000)
#a0_seq <- a1_seq <- c(0, 0.1, 0.2, 0.3)
a0_seq <- a1_seq <- c(0, 0.1)
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
normal_sims <- matrix(rnorm(nB * 6), nrow = 6, ncol = nB)

get_size_alpha_i <- function(i) {
  sim_alphas_size(true_params = sim_params[i,], normal_sims)
}
size_alpha <- mclapply(1:nrow(sim_params), get_size_alpha_i, mc.cores = 8)

get_bonf_CI_i <- function(i) {
  sim_bonf_CI(true_params = sim_params[i,], normal_sims,
              delta1 = 0.05, delta2 = 0.05, nreps = 10)
}
CIs_bonf <- mclapply(1:nrow(sim_params), get_bonf_CI_i, mc.cores = 8)

results <- list(size_alpha = size_alpha, CIs_bonf = CIs_bonf)

setwd('~/binary-regressor/sims/')
save(results, file = 'results_2017_07_31.Rd')


