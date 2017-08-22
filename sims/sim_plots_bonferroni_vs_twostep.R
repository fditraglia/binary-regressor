library(mbereg)
setwd('~/binary-regressor/sims/')
load('bonf-vs-gmm-CIs-nondiff-2017-08-18.Rd')


op <- par()

panel_plot <- function(myb, myn, xlims = NULL){
  par(mfrow = c(3, 3), oma = c(0, 0, 0, 0), pty = 's',
      mar = c(2.1, 2.1, 2.1, 0.1))
  indices <- as.numeric(rownames(subset(results$params, n == myn & b == myb &
                                        a0 != 0.3 & a1 != 0.3)))
  for(i in indices){
    true_params <- results$params[i,]

    CIs_bonf <- do.call(rbind, lapply(results$CIs[[i]], function(x) x$bonf))
    CIs_GMM <- do.call(rbind, lapply(results$CIs[[i]], function(x) x$gmm))

    CIs_twostep <- get_twostep_CI(CIs_GMM, CIs_bonf)

    CI_compare_plot(CIs_main = CIs_bonf, CIs_compare = CIs_twostep,
                    true_params, alphaonly = TRUE, xlims = xlims)
  }
}


#setwd('~/binary-regressor/fig')
#tikzwidth <- 6.5
#tikzheight <- 6.5

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0, 1000, xlim = c(-0.6, 0.6))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0, 2000, xlim = c(-0.5, 0.5))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0.25, 1000, xlim = c(-0.2, 1))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0.25, 2000, xlim = c(-0.1, 0.9))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0.5, 1000, xlim = c(0, 1.6))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0.5, 2000, xlim = c(0.1, 1.4))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0.75, 1000, xlim = c(0.15, 2.1))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(0.75, 2000, xlim = c(0.25, 2))
#dev.off()

#tikz(file = 'bonf_one_1000.tex', width = tikzwidth, height = tikzheight)
panel_plot(1, 1000, xlim = c(0.3, 2.7))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(1, 2000, xlim = c(0.4, 2.3))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(1.5, 1000, xlim = c(0.5, 3.6))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(1.5, 2000, xlim = c(0.9, 3.1))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(2, 1000, xlim = c(0.9, 4.5))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(2, 2000, xlim = c(1.2, 3.7))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(3, 1000, xlim = c(1.7, 6.1))
#dev.off()

#tikz(file = '', width = tikzwidth, height = tikzheight)
panel_plot(3, 2000, xlim = c(2, 5))
#dev.off()

par(op)
rm(list = ls())
