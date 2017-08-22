library(mbereg)

b_seq <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.5, 1)
n_seq <- c(1000, 2000)
a0_seq <- a1_seq <- c(0, 0.1, 0.2, 0.3)
d_seq <- c(0.15)
rho_seq <- c(0)
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
  sim_GMM_exog(true_params = sim_params[i,], delta = 0.05, ncores = 8, nreps = 400)
}
CIs_GMM <- lapply(1:nrow(sim_params), get_GMM_CI_i)

results <- list(params = sim_params, CIs_GMM = CIs_GMM)

h <- function(i) {
  CIs <- do.call(rbind, lapply(results$CIs_GMM[[i]], function(x) x$b))
  b_true <- results$params$b[i]
  cover <- get_coverage(b_true, CIs, NAempty = FALSE)
  prop_na <- get_prop_CIs_na(CIs)
  return(data.frame(cover = cover, prop_na = prop_na))
}

summary_results <- cbind(results$params, t(sapply(1:nrow(results$params), h)))
subset(summary_results, n == 1000 & prop_na > 0)
subset(summary_results, n == 2000 & prop_na > 0)
subset(summary_results, n == 1000 & cover < 92)
subset(summary_results, n == 2000 & cover < 92)
