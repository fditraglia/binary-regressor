library(crs)
library(mbereg)

set.seed(8162)
nB <- 10000
normal_sims <- matrix(rnorm(nB * 7), 7, nB)
normal_sims_alphas <- normal_sims[-5,] # Omit the "IV" row

bonf_CI <- function(dat, normal_sims, delta1 = 0.025, delta2 = 0.025,
                    inc_coarse = 0.1, inc_fine = 0.01){
  coarse <- seq(0, 1, inc_coarse)
  coarse <- expand.grid(a0 = coarse, a1 = coarse)
  coarse <- subset(coarse, a0 + a1 < 1)
  p_coarse <- sapply(1:nrow(coarse),
                    function(i) GMS_test_alphas(coarse$a0[i], coarse$a1[i],
                                                dat, normal_sims))

  # If the coarse CI is empty, try a finer grid.
  if(all(p_coarse < delta1)){
    coarse <- seq(0, 1, inc_coarse / 4)
    coarse <- expand.grid(a0 = coarse, a1 = coarse)
    coarse <- subset(coarse, a0 + a1 < 1)
    p_coarse <- sapply(1:nrow(coarse),
                      function(i) GMS_test_alphas(coarse$a0[i], coarse$a1[i],
                                                  dat, normal_sims))
  }

  if(all(p_coarse < delta1)) return(c(NA, NA))

  if(any(p_coarse < delta1)) {

    CI_coarse <- subset(coarse, p_coarse >= delta1)
    a0_fine <- seq(max(min(CI_coarse$a0) - inc_coarse, 0),
                   max(CI_coarse$a0) + inc_coarse, inc_fine)
    a1_fine <- seq(max(min(CI_coarse$a1) - inc_coarse, 0),
                   max(CI_coarse$a1) + inc_coarse, inc_fine)
    fine <- expand.grid(a0 = a0_fine, a1 = a1_fine)
    fine <- subset(fine, a0 + a1 < 1)
    p_fine <- sapply(1:nrow(fine),
                      function(i) GMS_test_alphas(fine$a0[i], fine$a1[i],
                                                  dat, normal_sims))
    CI_fine <- subset(fine, p_fine >= delta1)
    s_CI <- with(CI_fine, range(1 - a0 - a1))

    iv <- AER::ivreg(y ~ Tobs | z, data = dat)
    wald <- coefficients(iv)[2]
    se <- (summary(iv)$coefficients)[2,2]
    UCL <- wald + qnorm(1 - delta2 / 2) * se
    LCL <- wald - qnorm(1 - delta2 / 2) * se

    bonf_LCL <- min(LCL * min(s_CI), LCL * max(s_CI))
    bonf_UCL <- max(UCL * min(s_CI), UCL * max(s_CI))
    out <- list(a0 = with(CI_fine, range(a0)),
                a1 = with(CI_fine, range(a1)),
                theta1 = c(LCL, UCL),
                s = s_CI,
                b = c(bonf_LCL, bonf_UCL))
    return(out)
  } else {
    return(NA) # Empty confidence set for (a0, a1)
  }
}




b_true <- 0.5
a0_true <- 0
a1_true <- 0

set.seed(720)
dat <- dgp(a0 = a0_true, a1 = a1_true, b = b_true)

system.time(foo <- bonf_CI(dat, normal_sims_alphas))

# Now try Projection
a0_start <- mean(foo$a0)
a1_start <- mean(foo$a1)
b_start <- mean(foo$b)

# Constraint function: inside CI if p-value >= delta
g_lower <- function(x) {
  pvalue <- GMS_test(a0 = x[1], a1 = x[2], beta = x[3],
                     dat = dat, normal_sims = normal_sims)
  return(c(x[3], 0.05 - pvalue)) # constraint is g(x) <= 0
}

g_upper <- function(x) {
  pvalue <- GMS_test(a0 = x[1], a1 = x[2], beta = x[3],
                     dat = dat, normal_sims = normal_sims)
  return(c(-x[3], 0.05 - pvalue)) # constraint is g(x) <= 0
}

lower <- crs::snomadr(eval.f = g_lower,
                      n = 3,
                      x0 = c(a0_start, a1_start, b_start),
                      bbin = c(0, 0, 0), # Continuous inputs
                      bbout = c(0, 1), # 1st output of eval.f is OBJ, 2nd is EB
                      lb = c(0, 0, -10),
                      ub = c(0.9, 0.9, 10))

upper <- crs::snomadr(eval.f = g_upper,
                      n = 3,
                      x0 = c(a0_start, a1_start, b_start),
                      bbin = c(0, 0, 0), # Continuous inputs
                      bbout = c(0, 1), # 1st output of eval.f is OBJ, 2nd is EB
                      lb = c(0, 0, -10),
                      ub = c(0.9, 0.9, 10))

bar <- c(lower$solution[3], upper$solution[3])

foo$b #Bonferroni
bar #Projection
