setwd("~/binary-regressor/sims/")
library(tikzDevice)
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

b_pane <- function(b_val, n_val, d_val, title = TRUE, TeX = FALSE){
  foo <- subset(bhat, (b == b_val) & (n == n_val) & (d == d_val))
  if(TeX){
    my_xlab <- "$\\alpha_1$"
    my_ylab <- "$\\widehat{\\beta}$"
    my_main <- paste0("$N=", n_val, ", \\; d=", d_val, "$")
    my_legend <- paste0("$\\beta=", b_val, "$")
  }else{
    my_xlab <- expression(alpha[1])
    my_ylab <- expression(hat(beta))
    my_main <- bquote(N==.(n_val) ~ "," ~ d==.(d_val))
    my_legend <- bquote(beta==.(foo$b) * phantom(0.0))
  }
  x_step <- max(diff(foo$a1))
  x_min <- min(foo$a1) - 0.2 * x_step
  x_max <- max(foo$a1) + 0.2 * x_step
  y_min <- b_val - 1
  y_max <- b_val + 1
  #y_min <- min(foo$Lower5)
  #y_max <- max(foo$Upper5)
  with(foo, plot(a1, Median, ylim = c(y_min, y_max), 
                 xlim = c(x_min, x_max),  type = "n", yaxt = "n",
                 xlab = my_xlab, ylab = my_ylab, cex.lab = 1))
  axis(side = 2, at = c(y_min, y_min + 0.5, b_val, y_max - 0.5, y_max), 
       labels = c(y_min, "", b_val, "", y_max))
  if(title){
    title(main = my_main, cex.main = 1.5)
  }
  legend(x = "topleft", bty = "n", legend = my_legend, cex = 1, xjust = 0)
  abline(h = foo$b, lty = 5, col = "lightblue", lwd = 2)
  arrows(x0 = foo$a1, y0 = foo$Lower5, x1 = foo$a1, y1 = foo$Upper5, 
         angle = 90, length = 0.1, code = 3, col = "indianred3", lwd = 2)
  with(foo, points(a1, Median, ylim = c(y_min, y_max), pch = 19, 
                 col = "indianred3"))
}


b_panel <- function(b_vals, n_val, d_val, TeX = FALSE){
  par(mfrow = c(length(b_vals), 1),
      mar = c(4, 5, 3, 2) + 0.1)
  b_pane(b_vals[1], n_val, d_val, title = FALSE, TeX)
  for(i in 2:length(b_vals)){
    b_pane(b_vals[i], n_val, d_val, title = FALSE, TeX)
  }
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
}




a_pane <- function(a_val, n_val, d_val, title = TRUE, TeX = FALSE){
  foo <- subset(a1_a0, (a1 == a_val) & (n == n_val) & (d == d_val))
  if(TeX){
    my_xlab <- "$\\beta$"
    my_ylab <- "$\\widehat{\\alpha}_1 - \\widehat{\\alpha}_0$"
    my_main <- paste0("$N=", n_val, ", \\; d=", d_val, "$")
    my_legend <- paste0("$\\alpha_1=", a_val, "$")
  }else{
   my_xlab <- expression(beta)
   my_ylab <- expression(hat(alpha)[1] - hat(alpha)[0])
   my_main <- bquote(N==.(n_val) ~ "," ~ d==.(d_val))
   my_legend <- bquote(alpha[1]==.(foo$a1) * phantom(0.0))
  }
  x_step <- max(diff(foo$b))
  x_min <- min(foo$b) - 0.2 * x_step
  x_max <- max(foo$b) + 0.2 * x_step
  y_min <- -1
  y_max <- 1
  #y_min <- min(foo$Lower5)
  #y_max <- max(foo$Upper5)
  with(foo, plot(b, Median, ylim = c(y_min, y_max), 
                 xlim = c(x_min, x_max), type = "n", yaxt = "n", cex.lab = 1,
                 xlab = my_xlab, ylab = my_ylab))
  axis(side = 2, at = c(-1, -0.5, 0, 0.5, 1), labels = c("-1", "", "0", "", "1"))
  if(title){
    title(main = my_main, cex.main = 1.5)
  }
  legend(x = "top", legend = my_legend, bty = "n", cex = 1, xjust = 0)
  abline(h = foo$a1, lty = 5, col = "lightblue", lwd = 2)
  abline(h = 0)
  arrows(x0 = foo$b, y0 = foo$Lower5, x1 = foo$b, y1 = foo$Upper5, 
         angle = 90, code = 3, length = 0.1, col = "indianred3", lwd = 2)
  with(foo, points(b, Median, ylim = c(y_min, y_max), pch = 19, 
                 col = "indianred3"))
}
 
   
a_panel <- function(a_vals, n_val, d_val, TeX = FALSE){
  par(mfrow = c(length(a_vals), 1),
      mar = c(4, 5, 3, 2) + 0.1)
  a_pane(a_vals[1], n_val, d_val, title = FALSE, TeX)
  for(i in 2:length(a_vals)){
    a_pane(a_vals[i], n_val, d_val, title = FALSE, TeX)
  }
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
}

setwd("~/binary-regressor/fig")

#------------------------

tikz(file = "a_talk_N500_d10.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 500, 0.1, TeX = TRUE)
dev.off()

tikz(file = "a_talk_N1000_d10.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 1000, 0.1, TeX = TRUE)
dev.off()

tikz(file = "a_talk_N5000_d10.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 5000, 0.1, TeX = TRUE)
dev.off()

#------------------------

tikz(file = "a_talk_N500_d20.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 500, 0.2, TeX = TRUE)
dev.off()

tikz(file = "a_talk_N1000_d20.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 1000, 0.2, TeX = TRUE)
dev.off()

tikz(file = "a_talk_N5000_d20.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 5000, 0.2, TeX = TRUE)
dev.off()

#------------------------

tikz(file = "a_talk_N500_d30.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 500, 0.3, TeX = TRUE)
dev.off()

tikz(file = "a_talk_N1000_d30.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 1000, 0.3, TeX = TRUE)
dev.off()

tikz(file = "a_talk_N5000_d30.tex", width = 3, height = 6)
a_panel(c(0.1, 0.3), 5000, 0.3, TeX = TRUE)
dev.off()

#------------------------

tikz(file = "b_talk_N500_d10.tex", width = 3, height = 6)
b_panel(c(1, 2), 500, 0.1, TeX = TRUE)
dev.off()

tikz(file = "b_talk_N1000_d10.tex", width = 3, height = 6)
b_panel(c(1, 2), 1000, 0.1, TeX = TRUE)
dev.off()

tikz(file = "b_talk_N5000_d10.tex", width = 3, height = 6)
b_panel(c(1, 2), 5000, 0.1, TeX = TRUE)
dev.off()

#------------------------

tikz(file = "b_talk_N500_d20.tex", width = 3, height = 6)
b_panel(c(1, 2), 500, 0.2, TeX = TRUE)
dev.off()

tikz(file = "b_talk_N1000_d20.tex", width = 3, height = 6)
b_panel(c(1, 2), 1000, 0.2, TeX = TRUE)
dev.off()

tikz(file = "b_talk_N5000_d20.tex", width = 3, height = 6)
b_panel(c(1, 2), 5000, 0.2, TeX = TRUE)
dev.off()

#------------------------

tikz(file = "b_talk_N500_d30.tex", width = 3, height = 6)
b_panel(c(1, 2), 500, 0.3, TeX = TRUE)
dev.off()

tikz(file = "b_talk_N1000_d30.tex", width = 3, height = 6)
b_panel(c(1, 2), 1000, 0.3, TeX = TRUE)
dev.off()

tikz(file = "b_talk_N5000_d30.tex", width = 3, height = 6)
b_panel(c(1, 2), 5000, 0.3, TeX = TRUE)
dev.off()