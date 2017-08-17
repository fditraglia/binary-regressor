library(mbereg)
set.seed(1639)

nB <- 10000
normal_sims <- matrix(rnorm(nB * 14), 14, nB)

a0_true <- 0
a1_true <- 0.2
b_true <- 1
dat <- dgp(a0_true, a1_true, b_true, n = 5000, rho = 0)

p0 <- with(dat, mean(Tobs[z == 0]))
p1 <- with(dat, mean(Tobs[z == 1]))
a0_max <- min(p0, p1)
a1_max <- min(1 - p0,  1 - p1)

mult <- 5
a0_seq <- seq(0, a0_max * mult, 0.01)
a1_seq <- seq(0, a1_max * mult, 0.01)
alphas <- expand.grid(a0 = a0_seq, a1 = a1_seq)
alphas <- subset(alphas, a0 + a1 < 1)

get_p_values <- function(alphas, mytest, normal_sims) {
  pvalues <- rep(NA_real_, nrow(alphas))
  for(i in 1:nrow(alphas)) {
    a0_null <- alphas$a0[i]
    a1_null <- alphas$a1[i]
    pvalues[i] <- mytest(a0_null, a1_null, dat, normal_sims)
  }
  return(pvalues)
}

system.time(pvalues_nondiff <- get_p_values(alphas, GMS_test_alphas_nondiff_FL,
                                            normal_sims))
system.time(pvalues_weak <- get_p_values(alphas, GMS_test_alphas_FL,
                                         normal_sims[c(1:4, 13:14),]))
system.time(pvalues_noineq <- get_p_values(alphas, GMS_test_alphas_noineq_FL,
                                         normal_sims[13:14,]))

CI_nondiff <- alphas[pvalues_nondiff > 0.05,]
CI_weak <- alphas[pvalues_weak > 0.05,]
CI_noineq <- alphas[pvalues_noineq > 0.05,]

plot(CI_nondiff, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = 'red')
plot(CI_weak, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = 'red')
plot(CI_noineq, xlim = c(0, 1), ylim = c(0, 1), pch = 20, col = 'red')
points(CI_weak, xlim = c(0, 1), col = 'blue')

with(CI_nondiff, min(1 - a0 - a1))
with(CI_weak, min(1 - a0 - a1))
with(CI_noineq, min(1 - a0 - a1))

with(CI_nondiff, max(1 - a0 - a1))
with(CI_weak, max(1 - a0 - a1))
with(CI_noineq, max(1 - a0 - a1))
