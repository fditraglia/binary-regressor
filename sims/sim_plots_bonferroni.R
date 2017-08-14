setwd('~/binary-regressor/sims/')

load('bonf-CIs-2017-08-07.Rd')
results_weak <- results
rm(results)

load('bonf-CIs-2017-08-06.Rd')
results_weak_d1small <- results
rm(results)

load('bonf-CIs-nondiff-2017-08-13.Rd')
results_nondiff <- results
rm(results)

get_coverage <- function(x, CIs){
  lower <- apply(CIs, 1, min)
  lower[is.na(lower)] <- Inf
  upper <- apply(CIs, 1, max)
  upper[is.na(upper)] <- -Inf
  sapply(x, function(x)  mean((lower < x) & (upper > x)))
}

CI_plot <- function(results, i, param = 'b', mult = 1, nx = 500, nominal = 0.9){
  stopifnot(param %in% c('b', 'theta1', 's'))

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
  CIs <- lapply(CIs, function(x) getElement(x, param))
  CIs <- do.call(rbind, CIs)

  if(param == 's'){
    x <- seq(0, 0.999, length.out = nx / 4)
  } else {
    xmin <- min(RF_true, theta1_true)
    xmax <- max(RF_true, theta1_true)
    xwidth <- max(xmax - xmin, 1)
    x <- seq(xmin - mult * xwidth, xmax + mult * xwidth, length.out = nx)
  }
  coverage <- get_coverage(x, CIs)

  if(param == 'b') {
    myxlab <- '$\\beta$'
    param_true <- b_true
  } else if(param == 'theta1') {
    myxlab <- '$\\theta_1$'
    param_true <- theta1_true
  } else {
    myxlab <- '$s$'
    param_true <- s_true
  }
  mymain <- paste0('$\\beta = ', b_true,
                   ', \\alpha_0 = ', a0_true,
                   ', \\alpha_1 = ', a1_true,
                   ', n = ', n_true, '$')
  plot(x, coverage, type = 'l', xlab = myxlab, ylab = 'Coverage',
       main = mymain)
  abline(v = param_true, lty = 2)
  abline(h = nominal, lty = 2)

  if(param == 'b' & (RF_true != theta1_true)){
    abline(v = RF_true, col = 'red')
    abline(v = theta1_true, col = 'blue')
  } else if(param == 'theta1') {
    abline(v = b_true, col = 'blue')
  } else {
    #abline(v = 1 - (2 * d_true), col = 'red')
    #abline(v = 1, col = 'blue')
  }
}

CI_plot(results_nondiff, 27, param = 'b')
CI_plot(results_weak, 68, param = 'b')
CI_plot(results_weak_d1small, 68, param = 'b')
CI_plot(results_weak, 80, param = 'b')
CI_plot(results_weak_d1small, 80, param = 'b')
CI_plot(results_weak, 115, param = 'b')
CI_plot(results_weak_d1small, 115, param = 'b')
CI_plot(results_weak, 35, param = 'b')
CI_plot(results_weak_d1small, 35, param = 'b')
CI_plot(results_weak, 31, param = 'b', mult = 1)
CI_plot(results_weak_d1small, 31, param = 'b', mult = 1)

