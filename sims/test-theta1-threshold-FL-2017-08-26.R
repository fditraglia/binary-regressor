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
  true_params <- sim_params[i,]
  delta <- 0.05
  sim_rep <- function(){
    sim_dat <- dgp(a0 = true_params$a0,
                   a1 = true_params$a1,
                   b = true_params$b,
                   n = true_params$n,
                   d = true_params$d,
                   rho = true_params$rho,
                   cc = true_params$cc)

    theta1 <- with(sim_dat, cov(y, z) / cov(Tobs, z))
    kappa1 <- with(sim_dat, mean(y - theta1 * Tobs))
    epsilon_hat <- with(sim_dat, y - kappa1 - theta1 * Tobs)
    s_epsilon <- sqrt(sum(epsilon_hat^2) / (length(epsilon_hat) - 2))
    se <- with(sim_dat, (s_epsilon / sqrt(length(epsilon_hat)) * sd(z) / cov(Tobs, z)))
    tstat <- theta1 / se

    gmm <- GMM_exog(sim_dat)
    if((!is.na(gmm$est$b)) & (!is.na(gmm$SE$b))){
      b <- gmm$est$b
      SE <- gmm$SE$b
      ME <- SE * qnorm(1 - delta / 2)
      LCL <- b - ME
      UCL <- b + ME
      return(list(b = c(lower = LCL, upper = UCL), tstat = tstat))
    } else {
      return(list(b = c(lower = NA, upper = NA), tstat = tstat))
    }
  }
  nreps <- 400
  ncores <- 8
  return(parallel::mclapply(1:nreps, function(i) sim_rep(), mc.cores = ncores))
}
system.time(CIs_GMM <- lapply(1:nrow(sim_params), get_GMM_CI_i))

results <- list(params = sim_params, CIs_GMM = CIs_GMM)

get_coverage_notNA <- function(x, CIs){
  keep <- apply(CIs, 1, function(x) all(!is.na(x)))
  CIs <- CIs[keep,,drop = FALSE]
  if(any(keep)) {
    lower <- apply(CIs, 1, min)
    upper <- apply(CIs, 1, max)
    return(sapply(x, function(x)  100 * round(mean((lower < x) & (upper > x)), 2)))
  } else {
    return(NA)
  }
}

h <- function(i) {
  cutoff <- 3
  CIs <- do.call(rbind, lapply(results$CIs_GMM[[i]], function(x) x$b))
  tstats <- do.call(rbind, lapply(results$CIs_GMM[[i]], function(x) x$tstat))
  above <- abs(tstats) > cutoff
  prop_above <- 100 * mean(above)
  b_true <- results$params$b[i]
  cover <- get_coverage_notNA(b_true, CIs)
  if(any(above)) {
    CIs_above <- CIs[above,,drop = FALSE]
    cover_above <- get_coverage_notNA(b_true, CIs_above)
  } else {
    cover_above <- NA
  }
  prop_na <- get_prop_CIs_na(CIs)
  return(data.frame(cover = cover, cover_above = cover_above,
                    prop_above = prop_above, prop_na = prop_na))
}

summary_results <- cbind(results$params, t(sapply(1:nrow(results$params), h)))
#subset(summary_results, n == 1000 & prop_na > 0)
#subset(summary_results, n == 2000 & prop_na > 0)
#subset(summary_results, n == 1000 & cover < 92)
#subset(summary_results, n == 2000 & cover < 92)
