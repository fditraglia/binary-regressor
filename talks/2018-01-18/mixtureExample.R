library(mbereg)

mu_L <- -2
mu_H <- 2
p_true <- 0.3
trueDensity <- function(x){
  (1 - p_true) * dnorm(x, mu_L) + p_true * dnorm(x, mu_H)
}


set.seed(1234)
n <- 1e6
sims_L <- rnorm(n, mu_L)
sims_H <- rnorm(n, mu_H)
sims_mode <- rbinom(n, 1, p_true)
sims <- ifelse(sims_mode, sims_H, sims_L)
plot(density(sims))


lowerDensity <- function(x, p) {
  xupper <- quantile(sims, p)
  ifelse(x < xupper, trueDensity(x) / p, 0)
}

upperDensity <- function(x, p) {
  xlower <- quantile(sims, 1 - p)
  ifelse(x > xlower, trueDensity(x) / p, 0)
}


densityPlot <- function(p) {
  xseq <- seq(-5, 5, length.out = 500)
  ftrue <- trueDensity(xseq)
  flower <- lowerDensity(xseq, p)
  fupper <- upperDensity(xseq, p)
  #fmax <- max(c(ftrue, flower, fupper))
  par(mfrow = c(3, 1))
  qupper <- quantile(sims, p)
  mlower <- mean(sims[sims < qupper])
  qlower <- quantile(sims, 1 - p)
  mupper <- mean(sims[sims > qlower])
  plot(xseq, flower, type = 'l', #ylim = c(0, fmax),
       xlab = '', ylab = '', col = 'red')
  text(4, max(flower) * 0.9, '\\underline{h}(x)', col = 'red')
  abline(v = mlower, lty = 2, col = 'red')
  plot(xseq, ftrue, type = 'l', #ylim = c(0, fmax),
       xlab = '', ylab = '')
  polygon(x = c(xseq[xseq < qupper], rev(xseq[xseq < qupper])),
          y = c(rep(0, sum(xseq < qupper)), rev(ftrue[xseq < qupper])),
          density = 20, col = 'red')
  polygon(x = c(xseq[xseq < qupper], rev(xseq[xseq < qupper])),
          y = c(rep(0, sum(xseq < qupper)), rev(ftrue[xseq < qupper])),
          density = 20, col = 'red')
  polygon(x = c(xseq[xseq < qupper], rev(xseq[xseq < qupper])),
          y = c(rep(0, sum(xseq < qupper)), rev(ftrue[xseq < qupper])),
          density = 135, col = 'blue')
  abline(v = qlower, col = 'blue')
  abline(v = qupper, col = 'red')
  text(0, max(ftrue) * 0.9, 'f(x) = 0.7 \\phi(x + 2) + 0.3 \\phi(x - 2)')
  plot(xseq, fupper, type = 'l', #ylim = c(0, fmax),
       xlab = '', ylab = '',
       col = 'blue')
  text(-4, max(fupper) * 0.9, '\\bar{h}(x)', col = 'blue')
  abline(v = mupper, lty = 2, col = 'blue')
  par(mfrow = c(1, 1))

}

# Should make it clear how the quantiles in the middle plot lead to the
# upper and lower densities


get_mu_lower <- function(p) {
  mean(sims[sims < quantile(sims, p)])
}

get_mu_upper <- function(p) {
  mean(sims[sims > quantile(sims, 1 - p)])
}

Vget_mu_lower <- Vectorize(get_mu_lower)
Vget_mu_upper <- Vectorize(get_mu_upper)

pseq <- seq(0.005, 1, 0.005)
mu_lower <- Vget_mu_lower(pseq)
mu_upper <- Vget_mu_upper(pseq)
plot(pseq, mu_lower, type = 'l', ylim = c(min(mu_lower), max(mu_upper)))
points(pseq, mu_upper, type = 'l')

densityPlot(0.5)
