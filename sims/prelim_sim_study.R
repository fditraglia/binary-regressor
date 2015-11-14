library(mvtnorm)

dgp <- function(a0, a1, b = 1, n = 1000, rho = 0.3){
  n_treat <- ceiling(n/2)
  n_control <- n - n_treat
  z <- c(rep(0, n_control), rep(1, n_treat)) # offer of treatment
  errors <- rmvnorm(n, sigma = matrix(c(1, rho, rho, 1), 2, 2, byrow = TRUE))
  g0 <- qnorm(0.15) # 15% of controls take up treatment
  g1 <- qnorm(0.85) - qnorm(0.15) # 85% of treated take up treatment
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

set.seed(382)
results <- t(replicate(5000, est(dgp(0.1, 0.15))))
head(results)
colMeans(results, na.rm = TRUE)
results <- data.frame(results)

hist(results$a1_a0)
quantile(results$a1_a0, c(0.025, 0.975))
hist(results$BBS)
hist(results$b)
quantile(results$b, c(0.025, 0.975), na.rm = TRUE)

