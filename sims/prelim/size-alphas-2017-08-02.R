library(mbereg)
library(parallel)

b_seq <- c(0, 0.1, 0.2, 0.5, 1, 2)
n_seq <- c(250, 500, 1000, 2000)
a0_seq <- a1_seq <- c(0, 0.1, 0.2, 0.3, 0.4)
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
  unlist(sim_alphas_size(true_params = sim_params[i,], normal_sims, ncores = 8,
                         nreps = 10000))
}

size_alpha <- lapply(1:nrow(sim_params), get_size_alpha_i)

results <- list(params = sim_params, size_alpha = size_alpha)
setwd('~/binary-regressor/sims/')
save(results, file = 'size-alphas-2017-08-02.Rd')


