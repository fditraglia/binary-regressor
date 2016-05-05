library(binivdoctr)
set.seed(1627)

plotAlphaBounds(binDGP(0.2, 0.2, b = 0.1, n = 10000), j = 200)

# Test the moment conditions when a0 = 0
set.seed(716)
dat <- binDGP(0, 0.25, n = 1000000, b = 1)
F0 <- ecdf(with(dat, y[z == 0]))
F1 <- ecdf(with(dat, y[z == 1]))
F10 <- ecdf(with(dat, y[Tobs == 1 & z == 0]))
F11 <- ecdf(with(dat, y[Tobs == 1 & z == 1]))
p0 <- with(dat, mean(Tobs[z == 0]))
p1 <- with(dat, mean(Tobs[z == 1]))

A1 <- function(tau, b){
  p1 * (F11(tau + b) - F11(tau))
}

A0 <- function(tau, b){
  p0 * (F10(tau + b) - F10(tau))
}

A <- function(tau, b){
  A1(tau, b) - A0(tau, b)
}

C <- function(tau){
 F1(tau) - F0(tau)
}

MC <- function(tau1, tau2, b){
  A(tau1, b) / C(tau1) - A(tau2, b) / C(tau2)
}

Wnum <- with(dat, mean(y[z == 1] - mean(y[z == 0])))
Wdenom <- with(dat, mean(Tobs[z == 1] - mean(Tobs[z == 0])))
W <- Wnum / Wdenom

a0Upper <- min(p0, p1)
a1Upper <- min(1 - p0, 1 - p1)
bLower <- W * (1 - a0Upper - a1Upper)
bUpper <- W

tauplot <- function(tau1, tau2){
  f <- function(b){
   MC(tau1, tau2, b)
  }
  beta <- seq(bLower * 0.9, bUpper * 1.1, 0.01)
  MCbeta <- f(beta)
  plot(beta, MCbeta, type = 'l', lwd = 2, col = "red")
  abline(h = 0, lty = 2, lwd = 2)
  abline(v = 1, lwd = 2, col = "blue")
  abline(v = bLower, lwd = 2, col = "black")
  abline(v = bUpper, lwd = 2, col = "black")
}

library(manipulate)
manipulate(tauplot(tau1, tau2), tau1 = slider(-2, 2, -1, step = 0.1),
           tau2 = slider(-2, 2, 1, step = 0.1))

