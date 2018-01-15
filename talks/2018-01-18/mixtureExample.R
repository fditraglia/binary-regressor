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
  par(mfrow = c(3, 1),
      mar = c(3, 3, 2, 1),
      mgp = c(2, 0.7, 0),
      tck = -0.01)
  qupper <- quantile(sims, p)
  mlower <- mean(sims[sims < qupper])
  qlower <- quantile(sims, 1 - p)
  mupper <- mean(sims[sims > qlower])
  plot(xseq, flower, type = 'l', #ylim = c(0, fmax),
       xlab = '', ylab = '', col = 'red')
  text(5.4, max(flower) * 0.9, pos = 2,
       '$\\underline{h}(x) = p^{-1}f(x) 1\\{x < F^{-1}(p)\\}$',
       col = 'red')
  abline(v = mlower, lty = 2, col = 'red')
  text(mlower, 0, pos = 2, paste0('$\\underline{\\mu} = ', round(mlower, 2), '$'),
       col = 'red')
  plot(xseq, ftrue, type = 'l', #ylim = c(0, fmax),
       xlab = '', ylab = '')
  polygon(x = c(xseq[xseq < qupper], rev(xseq[xseq < qupper])),
          y = c(rep(0, sum(xseq < qupper)), rev(ftrue[xseq < qupper])),
          density = 20, col = 'red')
  text(-4, 0.06, paste0('$p = ', p, '$'), pos = 2, col = 'red')
  polygon(x = c(xseq[xseq > qlower], rev(xseq[xseq > qlower])),
          y = c(rep(0, sum(xseq > qlower)), rev(ftrue[xseq > qlower])),
          density = 20, angle = 135, col = 'blue')
  text(4, 0.04, paste0('$p = ', p, '$'), pos = 4,  col = 'blue')
  text(-1, max(ftrue) * 0.9, pos = 4,
       '$f(x) = 0.7 \\phi(x + 2)+0.3\\phi(x - 2)$')
  plot(xseq, fupper, type = 'l', #ylim = c(0, fmax),
       xlab = '', ylab = '',
       col = 'blue')
  text(-5.4, max(fupper) * 0.9, pos = 4,
       '$\\bar{h}(x)=p^{-1}f(x)1\\{x > F^{-1}(1-p)\\}$',
       col = 'blue')
  abline(v = mupper, lty = 2, col = 'blue')
  text(mupper, 0, pos = 4, paste0('$\\bar{\\mu} = ', round(mupper, 2), '$'),
       col = 'blue')
  par(mfrow = c(1, 1),
      mar = c(5.1, 4.1, 4.1, 2.1),
      mgp = c(3, 1, 0),
      tck = NA)
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

par(mar = c(3, 3, 2, 1),
    mgp = c(2, 0.7, 0),
    tck = -0.01)
plot(pseq, mu_lower, type = 'l', ylim = c(min(mu_lower), max(mu_upper)),
     xlab = '$p$', ylab = '$\\mu$')
points(pseq, mu_upper, type = 'l')
polygon(x = c(0, pseq, rev(pseq), 0),
        y = c(-10, mu_lower, rev(mu_upper), 10),
        density = 20,
        col = 'blue')
par(mar = c(5.1, 4.1, 4.1, 2.1),
    mgp = c(3, 1, 0),
    tck = NA)

