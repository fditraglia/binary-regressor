library(mvtnorm)
library(parallel)
n_cores <- detectCores()
setwd("~/binary-regressor/sims/")
set.seed(382)

params <- expand.grid("n" = c(500, 1000, 5000),
                      "b" = c(0.5, 1, 1.5, 2, 2.5),
                      "d" = c(0.1, 0.2, 0.3), 
                      "a0" = 0,
                      "a1" = c(0, 0.1, 0.2, 0.3, 0.4))



dgp <- function(a0, a1, b = 1, n = 1000, d = 0.15, rho = 0.5){
  n_treat <- ceiling(n/2)
  n_control <- n - n_treat
  z <- c(rep(0, n_control), rep(1, n_treat)) # offer of treatment
  errors <- rmvnorm(n, sigma = matrix(c(1, rho, rho, 1), 2, 2, byrow = TRUE))
  g0 <- qnorm(d) 
  g1 <- qnorm(1 - d) - qnorm(d) 
  xstar <- as.numeric(g0 + g1 * z + errors[,2] > 0) #select into treatment
  y <- b * xstar + errors[,1]
  #mis-classification
  x <- (1 - xstar) * rbinom(n, 1, a0) + xstar * rbinom(n, 1, 1 - a1) 
  return(data.frame(x, y, z))
}

est <- function(inData){
  OLS <- with(inData, cov(x,y)/var(x))
  IV <- with(inData, cov(z,y)/cov(z,x))
  dat0 <- subset(inData, z == 0)
  dat1 <- subset(inData, z == 1)
  p1_hat <- mean(dat1$x) 
  p0_hat <- mean(dat0$x) 
  W <- IV
  Dy2 <- mean(dat1$y^2) - mean(dat0$y^2)
  DyT <- mean(dat1$y * dat1$x) - mean(dat0$y * dat0$x)
  R <- (Dy2 - 2 * W * DyT) / (W * (p1_hat - p0_hat))
  a1_a0 <- 1 + R/W 
  Dy3 <- mean(dat1$y^3) - mean(dat0$y^3)
  Dy2T <- mean(dat1$y^2 * dat1$x) - mean(dat0$y^2 * dat0$x)
  S <- (Dy3 - 3 * W * (Dy2T + R * DyT)) / (W * (p1_hat - p0_hat))
  term1 <- -0.5 * R/W
  disc <- sqrt(3 * R^2 - 2 * S)
  term2 <- 0.5 * disc/W
  root1 <- term1 + term2
  root2 <- term1 - term2
  BBS <- max(root1, root2) - min(root1, root2)
  b <- W * BBS
  out <- c("OLS" = OLS, "IV" = IV, "a1_a0" = a1_a0, "root1" = root1, 
           "root2" = root2, "BBS" = BBS, "bhat" = b)
  return(out)
}

simDraw <- function(a0, a1, b, n, d, rho = 0.5, nreps = 500){
  results <- t(replicate(nreps, est(dgp(a0, a1, b, n, d, rho))))
  results <- as.data.frame(results)
  results$root1 <- NULL
  results$root2 <- NULL
  Lower5 <- apply(results, 2, quantile, probs = c(0.05), na.rm = TRUE)
  Median <- apply(results, 2, quantile, probs = c(0.5), na.rm = TRUE)
  Upper5 <- apply(results, 2, quantile, probs = c(0.95), na.rm = TRUE)
  nans <- apply(results, 2, function(col) mean(is.na(col)))
  return(list("Lower5" = Lower5, "Median" = Median, "Upper5" = Upper5,
              "nans" = nans))
}


results <- mcMap(simDraw, a0 = params$a0, a1 = params$a1, b = params$b, 
               n = params$n, d = params$d, rho = 0.5, nreps = 10000, 
               mc.cores = n_cores)

Lower5 <- do.call("rbind", lapply(results, function(x) x$Lower5))
Median <- do.call("rbind", lapply(results, function(x) x$Median))
Upper5 <- do.call("rbind", lapply(results, function(x) x$Upper5))
nans <- do.call("rbind", lapply(results, function(x) x$nans))

Lower5 <- as.data.frame(cbind(params, Lower5))
Median <- as.data.frame(cbind(params, Median))
Upper5 <- as.data.frame(cbind(params, Upper5))
nans <- as.data.frame(cbind(params, nans))

full_results <- list("Lower5" = Lower5, "Median" = Median, 
                     "Upper5" = Upper5, "nans" = nans)

save(full_results, file = "CI_sim_results.Rdata")

rm(list = ls())
