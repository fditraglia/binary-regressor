library(mbereg)
library(parallel)

b_seq <- c(0, 0.25, 0.5, 1)
n_seq <- c(1000)
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
nB <- 5000
normal_sims <- matrix(rnorm(nB * 6), nrow = 6, ncol = nB)

get_bonf_CI_i <- function(i) {
  sim_bonf_CI(true_params = sim_params[i,], normal_sims, ncores = 8,
              nreps = 1000, delta1 = 0.05, delta2 = 0.05)
}
#system.time(CIs_bonf <- mclapply(1:nrow(sim_params), get_bonf_CI_i, mc.cores = 8))
system.time(CIs_bonf <- lapply(1:nrow(sim_params), get_bonf_CI_i))

setwd('~/binary-regressor/sims/')
save(CIs_bonf, file = paste0('bonf-CIs-', Sys.Date(), '.Rd'))


