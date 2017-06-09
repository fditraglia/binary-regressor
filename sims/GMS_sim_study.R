library(mbereg)

# Make normal draws for GMS test
set.seed(19295)
R <- 5000
normal_sims <- matrix(rnorm(11 * R), 11, R)

GMS_sim_draw <- function(a0, a1, b, n = 1000, d = 0.15, rho = 0.5, cc = 0) {
  RF <- b * (1 - 2 * d)
  Wald <- b / (1 - a0 - a1)
  p0_true <- d * (1 - a0 - a1) + a0
  p1_true <- (1 - d) * (1 - a0 - a1) + a0
  sim_dat <- dgp(a0 = a0, a1 = a1, b = b, n = n, d = d, rho = rho, cc = cc)
  pval_null <- GMS_test(a0 = a0, a1 = a1, beta = b, dat = sim_dat,
                     normal_sims = normal_sims)
  pval_Wald <- GMS_test(a0 = 0, a1 = 0, b = Wald, dat = sim_dat,
                     normal_sims = normal_sims)
  pval_RF <- GMS_test(a0 = min(p0_true, p1_true),
                      a1 = min(1 - p0_true, 1 - p1_true),
                      beta = RF, dat = sim_dat, normal_sims = normal_sims)
  pvals <- c(null = pval_null, RF = pval_RF, Wald = pval_Wald)
  return(pvals)
}

GMS_sim <- function(a0, a1, b, n_reps = 5000, signif_level = 0.05) {
  p_values <- t(replicate(n_reps, GMS_sim_draw(a0 = a0, a1 = a1, b = b)))
  apply(p_values, 2, function(p_value) mean(p_value <= signif_level))
}

a0_vals <- a1_vals <- c(0, 0.1, 0.2) 
beta_vals <- seq(0, 2, 0.2)
sim_params <- expand.grid(a0_vals, a1_vals, beta_vals)
names(sim_params) <- c('a0', 'a1', 'b')
sim_params <- as.data.frame(sim_params)

results <- parallel::mcMap(GMS_sim, a0 = sim_params$a0, a1 = sim_params$a1,
                       b = sim_params$b, mc.cores = 8)
results <- do.call(rbind, results)
results <- cbind(sim_params, results)

setwd("~/binary-regressor/sims/")
save(results, file = "GMS_sim_results_2017_06_07.RData")

#rm(list = ls())
