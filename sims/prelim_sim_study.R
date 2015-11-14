library(mvtnorm)
library(parallel)
n_cores <- detectCores()
setwd("~/binary-regressor/sims/")
set.seed(382)

params <- expand.grid("n" = c(500, 1000, 5000),
                      "b" = c(0.5, 1, 2),
                      "d" = c(0.1, 0.2, 0.3), 
                      "aSum" = c(0, 0.25, 0.5, 0.75),
                      "aDiff" = c(0, 0.1, 0.2, 0.3))


params$a0 <- with(params, (aSum - aDiff) / 2)
params$a1 <- with(params, aDiff + a0)
params <- subset(params, (0 <= a0) & (a0 <= 1))





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
           "root2" = root2, "BBS" = BBS, "b" = b)
  return(out)
}

simDraw <- function(a0, a1, b = 1, n, d, rho = 0.5, nreps = 500){
  results <- t(replicate(nreps, est(dgp(a0, a1, b, n, d, rho))))
  results <- as.data.frame(results)
  results$root1 <- NULL
  results$root2 <- NULL
  truth <- c(b, b, a1 - a0, 1 - a0 - a1, b)
  bias <- rowMeans(apply(results, 1, function(row) row - truth), na.rm = TRUE)
  variance <- apply(results, 2, var, na.rm = TRUE)
  nans <- apply(results, 2, function(col) mean(is.na(col)))
  return(list("bias" = bias, "var" = variance, "nans" = nans))
}


results <- mcMap(simDraw, a0 = params$a0, a1 = params$a1, b = params$b, 
               n = params$n, d = params$d, rho = 0.5, nreps = 10000, 
               mc.cores = n_cores)

bias <- do.call("rbind", lapply(results, function(x) x$bias))
variance <- do.call("rbind", lapply(results, function(x) x$var))
rmse <- sqrt(bias^2 + variance)
nans <- do.call("rbind", lapply(results, function(x) x$nans))

bias <- as.data.frame(cbind(params, bias))
variance <- as.data.frame(cbind(params, variance))
rmse <- as.data.frame(cbind(params, rmse))
nans <- as.data.frame(cbind(params, nans))

full_results <- list("bias" = bias, "var" = variance, 
                     "rmse" = rmse, "nans" = nans)

save(full_results, file = "prelim_sim_results.Rdata")

rm(list = ls())
# results <- t(replicate(5000, est(dgp(0, 0))))
# #head(results)
# colMeans(results, na.rm = TRUE)
# results <- data.frame(results)
# 
# hist(results$a1_a0)
# quantile(results$a1_a0, c(0.025, 0.975))
# hist(results$BBS)
# hist(results$b)
# quantile(results$b, c(0.025, 0.975), na.rm = TRUE)

