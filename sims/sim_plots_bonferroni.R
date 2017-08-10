setwd('~/binary-regressor/sims/')
#load('size-alphas-2017-08-02.Rd')
load('bonf-CIs-2017-08-02.Rd')

CI_plot <- function(x, CIs){
  lower <- apply(CIs, 1, min)
  lower[is.na(lower)] <- Inf
  upper <- apply(CIs, 1, max)
  upper[is.na(upper)] <- -Inf
  coverage <- sapply(x, function(x)  mean((lower < x) & (upper > x)))
  plot(x, coverage, type = 'l', xlab = '')
}

#i <- 12
#i <- 16
#i <- 13
i <- 14
true_params <- results$params[i,]

b_true <- true_params$b
a0_true <- true_params$a0
a1_true <- true_params$a1
s_true <- 1 - a0_true - a1_true
n_true <- true_params$n
theta1_true <- b_true / s_true
d_true <- true_params$d
RF_true <- b_true * (1 - 2 * d_true)

CIs <- results$CIs_bonf[[i]]

theta1_CIs <- do.call(rbind, lapply(CIs, function(x) x$theta1))
s_CIs <- do.call(rbind, lapply(CIs, function(x) x$s))
b_CIs <- do.call(rbind, lapply(CIs, function(x) x$b))


b_seq <- seq(0, 2, 0.01)
CI_plot(b_seq, b_CIs)
abline(v = b_true, col = 'red', lty = 2)
abline(v = theta1_true, col = 'blue', lty = 2)
abline(v = RF_true, col = 'blue', lty = 2)
abline(h = 0.90, col = 'black', lty = 2)

s_seq <- seq(0, 0.99, 0.01)
CI_plot(s_seq, s_CIs)
abline(v = s_true, col = 'red', lty = 2)
abline(h = 0.95, col = 'blue', lty = 2)

theta1_seq <- seq(-1, 1, 0.01)
CI_plot(theta1_seq, theta1_CIs)
abline(v = theta1_true, col = 'red', lty = 2)
abline(h = 0.95, col = 'blue', lty = 2)

