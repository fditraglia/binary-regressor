library(mbereg)
library(tikzDevice)
setwd('~/binary-regressor/sims/')

load('bonf-CIs-2017-08-07.Rd')
results_weak <- results
rm(results)

load('bonf-CIs-nondiff-2017-08-14.Rd')
results_nondiff <- results
rm(results)

op <- par()

panel_plot <- function(myb, myn, xlims = NULL){
  par(mfrow = c(3, 3), oma = c(0, 0, 0, 0), pty = 's',
      mar = c(2.1, 2.1, 2.1, 0.1))
  indices <- as.numeric(rownames(subset(results_weak$params, n == myn & b == myb &
                                        a0 != 0.3 & a1 != 0.3)))
  for(i in indices){
    CI_compare_plot(results_weak, results_nondiff, i,
                    xlims = xlims, alphaonly = TRUE)
  }
}


setwd('~/binary-regressor/fig')
tikzwidth <- 6.5
tikzheight <- 6.5

tikz(file = 'bonf_zero_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0, 1000, xlim = c(-0.6, 0.6))
dev.off()

tikz(file = 'bonf_zero_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0, 2000, xlim = c(-0.5, 0.5))
dev.off()

tikz(file = 'bonf_quarter_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.25, 1000, xlim = c(-0.2, 1))
dev.off()

tikz(file = 'bonf_quarter_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.25, 2000, xlim = c(-0.1, 0.9))
dev.off()

tikz(file = 'bonf_half_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.5, 1000, xlim = c(0, 1.5))
dev.off()

tikz(file = 'bonf_half_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.5, 2000, xlim = c(0.1, 1.3))
dev.off()

tikz(file = 'bonf_one_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(1, 1000, xlim = c(0.3, 2.5))
dev.off()

tikz(file = 'bonf_one_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(1, 2000, xlim = c(0.4, 2.2))
dev.off()

tikz(file = 'bonf_two_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(2, 1000, xlim = c(0.8, 4))
dev.off()

tikz(file = 'bonf_two_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(2, 2000, xlim = c(1, 3.5))
dev.off()

par(op)
rm(list = ls())

