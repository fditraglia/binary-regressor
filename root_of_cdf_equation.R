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
b_true <- 2

dat <- binDGP(a0_true, a1_true, n = 50000, b = b_true)
F0 <- ecdf(with(dat, y[z == 0]))
F1 <- ecdf(with(dat, y[z == 1]))
F10 <- ecdf(with(dat, y[Tobs == 1 & z == 0]))
F11 <- ecdf(with(dat, y[Tobs == 1 & z == 1]))
p0 <- with(dat, mean(Tobs[z == 0]))
p1 <- with(dat, mean(Tobs[z == 1]))


par(mfrow = c(1,2))
D <- function(x) F1(x) - F0(x)
curve(D, min(dat$y), max(dat$y), n = 1001, xlab = expression(tau), ylab = '',
      main = expression(Delta(tau)))
D1 <- function(x) p1 * F11(x) - p0 * F10(x)
curve(D1, min(dat$y), max(dat$y), n = 1001, xlab = expression(tau), ylab = '',
      main = expression(tilde(Delta)[1](tau)))
par(mfrow = c(1, 1))


# Only consider values for beta between the ITT and the Wald
ITT <- with(dat, mean(y[z == 1]) - mean(y[z == 0]))
Wald <- ITT / (p1 - p0)

library(manipulate)

cdf_plot <- function(q_tau, q_nu){
  tau <- quantile(dat$y, q_tau)
  tau_prime <- uniroot(function(x) D(x) - D(tau), c(tau + 0.01, max(dat$y)))$root
  nu <- quantile(dat$y, q_nu)
  nu_prime <- uniroot(function(x) D(x) - D(nu), c(nu + 0.01, max(dat$y)))$root
  
  MC <- function(b){
    term1_num <- (D1(tau + b) - D1(tau)) - (D1(tau_prime + b) - D1(tau_prime)) 
    term1_denom <- D(tau + b) - D(tau_prime + b)
    term2_num <- (D1(nu + b) - D1(nu)) - (D1(nu_prime + b) - D1(nu_prime)) 
    term2_denom <- D(nu + b) - D(nu_prime + b)
    term1 <- term1_num / term1_denom
    term2 <- term2_num / term2_denom
    return(term1 - term2)
  }
  
  b_seq <- seq(ITT, Wald, 0.001)
  MC_b <- MC(b_seq)
  plot(b_seq, MC_b, type = 'l')
  abline(v = b_true, col = 'blue')
  abline(h = 0, lty = 2, col = 'red')
}

manipulate(cdf_plot(q_tau, q_nu),
           q_tau = slider(min = 0, max = 0.5, initial = 0.2, step = 0.05),
           q_nu = slider(min = 0, max = 0.5, initial = 0.4, step = 0.05))