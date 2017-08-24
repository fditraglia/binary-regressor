library(mbereg)
setwd('~/binary-regressor/sims/')
load('bonf-vs-gmm-CIs-FL-nondiff-2017-08-22.Rd')

setwd('~/binary-regressor/tab')

#------------------------------------
# Proportion of reps for which GMM
# CI fails to exist
#------------------------------------
cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 1000),
                     out_stat = 'GMM_na')),
    file = 'GMM_CIs_FL_na_1000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 2000),
                     out_stat = 'GMM_na')),
    file = 'GMM_CIs_FL_na_2000.tex')

#------------------------------------
# GMM Confidence Intervals
#------------------------------------
cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 1000),
                   out_stat = 'cover_GMM')),
    file = 'GMM_CIs_FL_cover_1000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 1000),
                   out_stat = 'width_GMM')),
    file = 'GMM_CIs_FL_width_1000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 2000),
                   out_stat = 'cover_GMM')),
    file = 'GMM_CIs_FL_cover_2000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 2000),
                   out_stat = 'width_GMM')),
    file = 'GMM_CIs_FL_width_2000.tex')

#------------------------------------
# Bonferroni Confidence Intervals
#------------------------------------
cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 1000),
                   out_stat = 'cover_bonf')),
    file = 'bonf_CIs_FL_cover_1000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 1000),
                   out_stat = 'width_bonf')),
    file = 'bonf_CIs_FL_width_1000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 2000),
                   out_stat = 'cover_bonf')),
    file = 'bonf_CIs_FL_cover_2000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 2000),
                   out_stat = 'width_bonf')),
    file = 'bonf_CIs_FL_width_2000.tex')


#------------------------------------
# Two-step Confidence intervals
#------------------------------------

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 1000),
                   out_stat = 'cover_twostep')),
    file = 'twostep_CIs_FL_cover_1000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 1000),
                   out_stat = 'width_twostep')),
    file = 'twostep_CIs_FL_width_1000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 2000),
                   out_stat = 'cover_twostep')),
    file = 'twostep_CIs_FL_cover_2000.tex')

cat(TeXtable(build_table(subset(summarize_CI_results(results), n == 2000),
                   out_stat = 'width_twostep')),
    file = 'twostep_CIs_FL_width_2000.tex')



rm(list = ls())
