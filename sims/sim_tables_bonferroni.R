library(mbereg)
setwd('~/binary-regressor/sims/')
load('bonf-CIs-nondiff-2017-08-14.Rd')

setwd('~/binary-regressor/tab')
cat(TeXtable(build_table(subset(get_bonf_cover_raw(results), n == 1000))),
    file = 'bonf_CIs_cover_1000.tex')
cat(TeXtable(build_table(subset(get_bonf_cover_raw(results), n == 2000))),
    file = 'bonf_CIs_cover_2000.tex')
cat(TeXtable(build_table(subset(get_bonf_width_raw(results), n == 1000))),
    file = 'bonf_CIs_width_1000.tex')
cat(TeXtable(build_table(subset(get_bonf_width_raw(results), n == 2000))),
    file = 'bonf_CIs_width_2000.tex')

rm(list = ls())
