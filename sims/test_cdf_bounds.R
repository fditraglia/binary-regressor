library(mbereg)

set.seed(27398)
nB <- 5000
normal_sims_full <- matrix(rnorm(nB * 30), 30, nB)
normal_sims_small <- normal_sims_full[-c(5:28),]

set.seed(72983)
a0_true <- 0.1
a1_true <- 0.2
b_true <- 1
dat <- dgp(a0_true, a1_true, b_true)
with(dat, hist(y))

a0_null <- 0.2
a1_null <- 0.3
GMS_test_alphas_cdf(a0_null, a1_null, dat, normal_sims_small)
GMS_test_alphas_cdf(a0_null, a1_null, dat, normal_sims_full, tau = c(-1, 0, 1))
GMS_test_alphas_cdf(a0_null, a1_null, dat, normal_sims_full, tau = c(-2, 0, 2))
