library(mbereg)
library(tikzDevice)
setwd('~/binary-regressor/sims/')

load('bonf-CIs-weak-2017-08-18.Rd')
results_weak <- results
rm(results)

load('bonf-vs-gmm-CIs-nondiff-2017-08-18.Rd')
results_nondiff <- results
rm(results)

op <- par()

panel_plot <- function(myb, myn, xlims = NULL){
  stopifnot(all.equal(results_nondiff$params, results_weak$params))
  par(mfrow = c(3, 3), oma = c(0, 0, 0, 0), pty = 's',
      mar = c(2.1, 2.1, 2.1, 0.1))
  indices <- as.numeric(rownames(subset(results_nondiff$params,
                                        n == myn & b == myb & a0 != 0.3 & a1 != 0.3)))
  for(i in indices){
    true_params <- results_nondiff$params[i,]

    CIs_nondiff <- do.call(rbind, lapply(results_nondiff$CIs[[i]], function(x) x$bonf))
    CIs_weak <- do.call(rbind, lapply(results_weak$CIs_bonf[[i]], function(x) x$b))

    CI_compare_plot(CIs_main = CIs_nondiff, CIs_compare = CIs_weak,
                    true_params, alphaonly = TRUE, xlims = xlims)
  }
}


setwd('~/binary-regressor/fig')
tikzwidth <- 6.5
tikzheight <- 6.5

tikz(file = 'bonf_0_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0, 1000, xlim = c(-0.6, 0.6))
dev.off()

tikz(file = 'bonf_0_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0, 2000, xlim = c(-0.6, 0.6))
dev.off()

tikz(file = 'bonf_point25_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.25, 1000, xlim = c(-0.25, 1.1))
dev.off()

tikz(file = 'bonf_point25_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.25, 2000, xlim = c(-0.25, 1.1))
dev.off()

tikz(file = 'bonf_point5_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.5, 1000, xlim = c(0, 1.6))
dev.off()

tikz(file = 'bonf_point5_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.5, 2000, xlim = c(0, 1.45))
dev.off()

tikz(file = 'bonf_point75_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.75, 1000, xlim = c(0.1, 2.2))
dev.off()

tikz(file = 'bonf_point75_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(0.75, 2000, xlim = c(0.2, 1.9))
dev.off()

tikz(file = 'bonf_1_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(1, 1000, xlim = c(0.2, 2.6))
dev.off()

tikz(file = 'bonf_1_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(1, 2000, xlim = c(0.3, 2.4))
dev.off()

tikz(file = 'bonf_1point5_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(1.5, 1000, xlim = c(0.5, 3.6))
dev.off()

tikz(file = 'bonf_1point5_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(1.5, 2000, xlim = c(0.6, 3.1))
dev.off()

tikz(file = 'bonf_2_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(2, 1000, xlim = c(0.7, 4.5))
dev.off()

tikz(file = 'bonf_2_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(2, 2000, xlim = c(0.9, 3.8))
dev.off()

tikz(file = 'bonf_3_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(3, 1000, xlim = c(1.2, 6))
dev.off()

tikz(file = 'bonf_3_2000.tex', width = tikzwidth, height = tikzheight)
panel_plot(3, 2000, xlim = c(1.4, 5))
dev.off()

par(op)
rm(list = ls())

