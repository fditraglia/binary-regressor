library(mbereg)
a0_true <- 0.15
a1_true <- 0.1
b_true <- 0.5
n_true <- 1000

set.seed(61234)
nB <- 5000
normal_sims <- matrix(rnorm(nB * 14), 14, nB)


get_bonf <- function(dat) {
  bonf_CI(dat, normal_sims, GMS_test_alphas_nondiff,
          delta1 = 0.025, delta2 = 0.025)$b
}

get_gmm <- function(dat) {
  gmm <- GMM_endog(dat)
    if((!is.na(gmm$est$b)) & (!is.na(gmm$SE$b))){
      b <- gmm$est$b
      SE <- gmm$SE$b
      ME <- SE * qnorm(1 - 0.05 / 2)
      LCL <- b - ME
      UCL <- b + ME
      return(c(LCL, UCL))
    } else {
      return(c(NA, NA))
    }
}

RF_Fstat <- function(dat) {
  reg <- lm(y ~ z, dat)
  (summary(reg)$coefficients[2,3])^2
}

GMM_in_bonf <- function(CIs_GMM, CIs_bonf) {
 CIs_GMM[is.na(CIs_GMM[,1]),1] <- -Inf
 CIs_GMM[is.na(CIs_GMM[,2]),2] <- +Inf
 use_GMM <- (CIs_GMM[,1] > CIs_bonf[,1]) & (CIs_GMM[,2] < CIs_bonf[,2])
 lower <- ifelse(use_GMM, CIs_GMM[,1], CIs_bonf[,1])
 upper <- ifelse(use_GMM, CIs_GMM[,2], CIs_bonf[,2])
 out <- cbind(lower, upper)
 return(out)
}

CI_sim <- function() {
  dat <- dgp(a0 = a0_true, a1 = a1_true, b = b_true, n = n_true)
  Fstat <- RF_Fstat(dat)
  GMM <- get_gmm(dat)
  bonf <- get_bonf(dat)
  out <- list(GMM = GMM, bonf = bonf)
  return(out)
}

nreps <- 3 * 80
set.seed(3827)
system.time(results <- parallel::mclapply(1:nreps, function(i) CI_sim(), mc.cores = 8))

CIs_bonf <- do.call(rbind, lapply(results, function(x) x$bonf))
CIs_GMM <- do.call(rbind, lapply(results, function(x) x$GMM))
CIs_2step <- GMM_in_bonf(CIs_GMM, CIs_bonf)

get_coverage(b_true, CIs_GMM, NAempty = TRUE)
get_coverage(b_true, CIs_GMM[apply(CIs_GMM, 1, function(x) all(!is.na(x))),])
get_coverage(b_true, CIs_GMM, NAempty = FALSE)
get_coverage(b_true, CIs_bonf)
get_coverage(b_true, CIs_2step)

get_median_width(CIs_bonf)
get_median_width(CIs_2step)
get_median_width(CIs_GMM, NAempty = TRUE)
get_median_width(CIs_GMM[apply(CIs_GMM, 1, function(x) all(!is.na(x))),])
get_median_width(CIs_GMM, NAempty = FALSE)



















