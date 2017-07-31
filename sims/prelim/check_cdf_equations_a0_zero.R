binDGP <- function(a0, a1, b = 1, c = 0, n = 1000, d = 0.15, rho = 0.5){
  n_treat <- ceiling(n/2)
  n_control <- n - n_treat
  z <- c(rep(0, n_control), rep(1, n_treat)) # offer of treatment
  errors <- MASS::mvrnorm(n, mu = c(0, 0),
                          Sigma = matrix(c(1, rho, rho, 1), 2, 2, byrow = TRUE))
  g0 <- qnorm(d)
  g1 <- qnorm(1 - d) - qnorm(d)
  Tstar <- as.numeric(g0 + g1 * z + errors[,2] > 0) #select into treatment
  y <- c + b * Tstar + errors[,1]
  #mis-classification
  Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1)
  return(data.frame(Tobs, y, z, Tstar))
}

set.seed(102794)

dat <- binDGP(0, 0.15, n = 1000000, b = 1)
F0 <- ecdf(with(dat, y[z == 0]))
F1 <- ecdf(with(dat, y[z == 1]))
F10 <- ecdf(with(dat, y[Tobs == 1 & z == 0]))
F11 <- ecdf(with(dat, y[Tobs == 1 & z == 1]))
p0 <- with(dat, mean(Tobs[z == 0]))
p1 <- with(dat, mean(Tobs[z == 1]))

D <- function(x) F1(x) - F0(x)
D1 <- function(x) p1 * F11(x) - p0 * F10(x)

MC_a0_zero <- function(b, tau1, tau2){
  term1 <- (D1(tau1) - D1(tau1 + b)) / D(tau1)
  term2 <- (D1(tau2) - D1(tau2 + b)) / D(tau2)
  return(term1 - term2)
}

ITT <- with(dat, mean(y[z == 1] - mean(y[z == 0])))
Wald <- ITT / (p1 - p0)

tauplot <- function(tau1, tau2){
  f <- function(b){
   MC_a0_zero(b, tau1, tau2)
  }
  beta <- seq(ITT, Wald, 0.001)
  MCbeta <- f(beta)
  plot(beta, MCbeta, type = 'l', lwd = 2, col = "red")
  abline(h = 0, lty = 2, lwd = 2)
  abline(v = 1, lwd = 2, col = "blue")
}

library(manipulate)
manipulate(tauplot(tau1, tau2), tau1 = slider(-2, 2, -1, step = 0.1),
           tau2 = slider(-2, 2, 1, step = 0.1))

est <- function(inData){
  OLS <- with(inData, cov(Tobs,y)/var(Tobs))
  IV <- with(inData, cov(z,y)/cov(z,Tobs))
  dat0 <- subset(inData, z == 0)
  dat1 <- subset(inData, z == 1)
  p1_hat <- mean(dat1$Tobs) 
  p0_hat <- mean(dat0$Tobs) 
  W <- IV
  Dy2 <- mean(dat1$y^2) - mean(dat0$y^2)
  DyT <- mean(dat1$y * dat1$Tobs) - mean(dat0$y * dat0$Tobs)
  R <- (Dy2 - 2 * W * DyT) / (W * (p1_hat - p0_hat))
  a1_a0 <- 1 + R/W 
  Dy3 <- mean(dat1$y^3) - mean(dat0$y^3)
  Dy2T <- mean(dat1$y^2 * dat1$Tobs) - mean(dat0$y^2 * dat0$Tobs)
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

est(dat)