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

a0_true <- 0.1
a1_true <- 0.2
b_true <- 1

dat <- binDGP(a0_true, a1_true, n = 10000000, b = b_true)
F0 <- ecdf(with(dat, y[z == 0]))
F1 <- ecdf(with(dat, y[z == 1]))
F10 <- ecdf(with(dat, y[Tobs == 1 & z == 0]))
F11 <- ecdf(with(dat, y[Tobs == 1 & z == 1]))
p0 <- with(dat, mean(Tobs[z == 0]))
p1 <- with(dat, mean(Tobs[z == 1]))

D <- function(x) F1(x) - F0(x)
D1 <- function(x) p1 * F11(x) - p0 * F10(x)


MC_a0_known <- function(b, tau1, tau2){
  term1 <- (a0_true * D(tau1 + b) + D1(tau1) - D1(tau1 + b)) / D(tau1)
  term2 <- (a0_true * D(tau2 + b) + D1(tau2) - D1(tau2 + b)) / D(tau2)
  return(term1 - term2)
}

MC_a1_known <- function(b, tau1, tau2){
  term1 <- (D1(tau1 + b) - D1(tau1) + (1 - a1_true) * D(tau1)) / D(tau1 + b)
  term2 <- (D1(tau2 + b) - D1(tau2) + (1 - a1_true) * D(tau2)) / D(tau2 + b)
  return(term1 - term2)
}


# Only consider values for beta between the ITT and the Wald
ITT <- with(dat, mean(y[z == 1]) - mean(y[z == 0]))
Wald <- ITT / (p1 - p0)

library(manipulate)

plot_a0_known <- function(q1, q2){
  tau1 <- quantile(dat$y, q1)
  tau2 <- quantile(dat$y, q2)
  f <- function(b){
   MC_a0_known(b, tau1, tau2)
  }
  beta <- seq(ITT, Wald, 0.001)
  MCbeta <- f(beta)
  plot(beta, MCbeta, type = 'l', lwd = 2, col = "red", main = 'a0 Known')
  abline(h = 0, lty = 2, lwd = 2)
  abline(v = 1, lwd = 2, col = "blue")
}

manipulate(plot_a0_known(q1, q2), q1 = slider(0.1, 0.9, 0.25, step = 0.05),
           q2 = slider(0.1, 0.9, 0.75, step = 0.05))

plot_a1_known <- function(q1, q2){
  tau1 <- quantile(dat$y, q1)
  tau2 <- quantile(dat$y, q2)
  f <- function(b){
   MC_a1_known(b, tau1, tau2)
  }
  beta <- seq(ITT, Wald, 0.001)
  MCbeta <- f(beta)
  plot(beta, MCbeta, type = 'l', lwd = 2, col = "red", main = 'a1 Known')
  abline(h = 0, lty = 2, lwd = 2)
  abline(v = 1, lwd = 2, col = "blue")
}

manipulate(plot_a1_known(q1, q2), q1 = slider(0.1, 0.9, 0.25, step = 0.05),
           q2 = slider(0.1, 0.9, 0.75, step = 0.05))

# Now try the case where a0 and a1 are both unknown
q_tau <- 0.35
q_nu <- 0.45
tau <- quantile(dat$y, q_tau)
tau_prime <- uniroot(function(x) D(x) - D(tau), c(tau + 0.01, max(dat$y)))$root
nu <- quantile(dat$y, q_nu)
nu_prime <- uniroot(function(x) D(x) - D(nu), c(nu + 0.01, max(dat$y)))$root

curve(D, min(dat$y), max(dat$y))
abline(v = nu, col = 'blue')
abline(v = nu_prime, col = 'blue')
abline(v = tau, col = 'red')
abline(v = tau_prime, col = 'red')

# Presumably we want to stay away from values that make D1 close to zero
curve(D1, min(dat$y), max(dat$y))
abline(v = nu, col = 'blue')
abline(v = nu_prime, col = 'blue')
abline(v = tau, col = 'red')
abline(v = tau_prime, col = 'red')

MC <- function(b){
  term1 <- (D1(tau + b) - D1(tau_prime + b)) / (D(tau + b) - D(tau_prime + b))
  term2 <- (D1(nu + b) - D1(nu_prime + b)) / (D(nu + b) - D(nu_prime + b))
  return(term1 - term2)
}

# Something is wrong here: this function asymptotes to zero...
b_seq <- seq(0.5, 2, 0.001)
MC_b <- MC(b_seq)
plot(b_seq, MC_b, type = 'l')

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