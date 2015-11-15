setwd("~/binary-regressor/sims/")
load("CI_sim_results.Rdata")

params <- full_results$Lower5[,1:5]
a1_a0 <- data.frame("Lower5" = full_results$Lower5$a1_a0,
                    "Median" = full_results$Median$a1_a0,
                    "Upper5" = full_results$Upper5$a1_a0)
a1_a0 <- cbind(params, a1_a0)


bhat <- data.frame("Lower5" = full_results$Lower5$bhat,
                   "Median" = full_results$Median$bhat,
                   "Upper5" = full_results$Upper5$bhat)
bhat <- cbind(params, bhat)

b_pane <- function(b_val, n_val, d_val){
  foo <- subset(bhat, (b == b_val) & (n == n_val) & (d == d_val))
  y_min <- min(foo$Lower5)
  y_max <- max(foo$Upper5)
  with(foo, plot(a1, Median, ylim = c(y_min, y_max), pch = 19, 
                 col = "indianred3",
                 xlab = expression(alpha[1]), 
                 ylab = expression(hat(beta)), cex.lab = 1.5))
  abline(h = foo$b, lty = 5, col = "lightblue")
  arrows(x0 = foo$a1, y0 = foo$Lower5, x1 = foo$a1, y1 = foo$Upper5, 
         angle = 90, code = 3, col = "indianred3", lwd = 2)
  legend(x = "topleft", legend = bquote(beta==.(foo$b)), 
         cex = 1.5, xjust = 0)
}

b_pane(1.5, 500, 0.2)

a_pane <- function(a_val, n_val, d_val){
  foo <- subset(a1_a0, (a1 == a_val) & (n == n_val) & (d == d_val))
  y_min <- min(foo$Lower5)
  y_max <- max(foo$Upper5)
  with(foo, plot(b, Median, ylim = c(y_min, y_max), pch = 19, 
                 col = "indianred3",
                 xlab = expression(beta), 
                 ylab = expression(hat(alpha)[1] - hat(alpha)[0]),
                 cex.lab = 1.5))
  abline(h = foo$a1, lty = 5, col = "lightblue")
  arrows(x0 = foo$b, y0 = foo$Lower5, x1 = foo$b, y1 = foo$Upper5, 
         angle = 90, code = 3, col = "indianred3", lwd = 2)
  abline(h = 0)
  legend(x = "topright", legend = bquote(alpha[1]==.(foo$a1)), 
         cex = 1.5, xjust = 0)
}

a_pane(0.3, 500, 0.3)

