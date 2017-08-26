library(mbereg)
setwd('~/binary-regressor/sims/')
load('unconstrained-GMM-CIs-2017-08-16.Rd')

setwd('~/binary-regressor/tab')
cat(TeXtable(build_table(subset(get_GMM_cover_raw(results), n == 1000))),
    file = 'GMM_CIs_cover_1000.tex')
cat(TeXtable(build_table(subset(get_GMM_cover_raw(results), n == 2000))),
    file = 'GMM_CIs_cover_2000.tex')
cat(TeXtable(build_table(subset(get_GMM_width_raw(results), n == 1000))),
    file = 'GMM_CIs_width_1000.tex')
cat(TeXtable(build_table(subset(get_GMM_width_raw(results), n == 2000))),
    file = 'GMM_CIs_width_2000.tex')

rm(list = ls())
