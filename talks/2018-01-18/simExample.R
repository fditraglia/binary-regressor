library(mbereg)
library(tikzDevice)

set.seed(4321)
dat <- dgp(a0 = 0.1, a1 = 0.2, b = 1, n = 5000)

ivreg <- function(x, y, z, alpha = 0.05) {
  iv_slope <- cov(y, z) / cov(x, z)
  iv_intercept <- mean(y - iv_slope * x)
  epsilon_hat <- y - iv_intercept - iv_slope * x
  s_epsilon <- sqrt(sum(epsilon_hat^2) / (length(epsilon_hat) - 2))
  se <- s_epsilon / sqrt(length(epsilon_hat)) * sd(z) / cov(x, z)
  UCL <- iv_slope + qnorm(1 - alpha / 2) * se
  LCL <- iv_slope - qnorm(1 - alpha / 2) * se
  out <- list(b = iv_slope, lower = LCL, upper = UCL)
  return(out)
}


unobserved <- ivreg(x = dat$Tstar, y = dat$y, z = dat$z)
observed <- ivreg(x = dat$Tobs, y = dat$y, z = dat$z)

tikz('~/binary-regressor/talks/2018-01-18/histograms_Tstar.tex',
     width = 4, height = 4)
par(mfrow = c(2, 2),
    mar = c(3, 3, 2, 1), 
    mgp = c(2, 0.7, 0))
with(dat, MASS::truehist(y[Tstar == 0 & z == 0], xlab = '', main = '$y|T^*=0,z=0$'))
with(dat, MASS::truehist(y[Tstar == 0 & z == 1], xlab = '', main = '$y|T^*=0,z=1$'))
with(dat, MASS::truehist(y[Tstar == 1 & z == 0], xlab = '', main = '$y|T^*=1,z=0$'))
with(dat, MASS::truehist(y[Tstar == 1 & z == 1], xlab = '', main = '$y|T^*=1,z=1$'))
par(mfrow = c(1, 1),
    mar = c(5.1, 4.1, 4.1, 2.1),
    mgp = c(3, 1, 0))
dev.off()

tikz('~/binary-regressor/talks/2018-01-18/histograms_Tobs.tex',
     width = 4, height = 4)
par(mfrow = c(2, 2),
    mar = c(3, 3, 2, 1), 
    mgp = c(2, 0.7, 0))
with(dat, MASS::truehist(y[Tobs == 0 & z == 0], xlab = '', main = '$y|T=0,z=0$'))
with(dat, MASS::truehist(y[Tobs == 0 & z == 1], xlab = '', main = '$y|T=0,z=1$'))
with(dat, MASS::truehist(y[Tobs == 1 & z == 0], xlab = '', main = '$y|T=1,z=0$'))
with(dat, MASS::truehist(y[Tobs == 1 & z == 1], xlab = '', main = '$y|T=1,z=1$'))
par(mfrow = c(1, 1),
    mar = c(5.1, 4.1, 4.1, 2.1),
    mgp = c(3, 1, 0))
dev.off()

set.seed(619283)
nB <- 5000
normal_sims <- matrix(rnorm(nB * 14), nrow = 14, ncol = nB)

#alpha_grid <- seq(0, 1, 0.01)
alpha_grid <- expand.grid(a0 = seq(0, 0.15, 0.0025), a1 = seq(0.17, 0.27, 0.0025))
#alpha_grid <- expand.grid(a0 = alpha_grid, a1 = alpha_grid)
alpha_grid <- subset(alpha_grid, a0 + a1 < 1)
test_alphas <- GMS_test_alphas_nondiff
pvalues <- sapply(1:nrow(alpha_grid),
                  function(i) test_alphas(alpha_grid$a0[i], alpha_grid$a1[i],
                                          dat, normal_sims))

delta1 <- 0.025
alphaCI <- subset(alpha_grid, pvalues > delta1)

tikz('~/binary-regressor/talks/2018-01-18/GMS_CI.tex',
     width = 4, height = 4)
plot(alphaCI, pch = 15, col = 'red', xlim = c(0, 0.3), ylim = c(0, 0.3),
     xlab = '$\\alpha_0$', ylab = '$\\alpha_1$', pty = 's', 
     main = '97.5\\% GMS Confidence Region for $(\\alpha_0, \\alpha_1)$',
     las = 1)
alphaSums <- with(alphaCI, range(a0 + a1))
abline(alphaSums[1], -1, lty = 2, lwd = 2)
text(0.12, 0.05, pos = 2, 
     labels = paste0('$\\alpha_0 + \\alpha_1 = ', round(alphaSums[1], 2), '$'))
abline(alphaSums[2], -1, lty = 2, lwd = 2)
text(0.17, 0.20, pos = 4, 
     labels = paste0('$\\alpha_0 + \\alpha_1 = ', round(alphaSums[2], 2), '$'))
dev.off()

BBS_CI <- with(alphaCI, range(1 - a0 - a1))

theta1 <- ivreg(x = dat$Tobs, y = dat$y, z = dat$z, alpha = 0.025)
UCL <- theta1$upper
LCL <- theta1$lower
s_CI <- with(CI_alphas, range(1 - a0 - a1))
bonf_LCL <- min(LCL * min(s_CI), LCL * max(s_CI))
bonf_UCL <- max(UCL * min(s_CI), UCL * max(s_CI))
bonf_CI <- c(bonf_LCL, bonf_UCL)


