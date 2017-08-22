library(mbereg)
setwd('~/binary-regressor/sims')
load('size-alphas-nondiff-2017-08-22.Rd')

params <- results$params
pvalues <- results$size_alpha
rm(results)

cover90 <- sapply(pvalues, function(x) 100 * round(mean(x > 0.1), 2))
cover95 <- sapply(pvalues, function(x) 100 * round(mean(x > 0.05), 2))
cover97point5 <- sapply(pvalues, function(x) 100 * round(mean(x > 0.025), 3))

alpha_coverage <- cbind(params, cover90, cover95, cover97point5)
rm(cover90, cover95, cover97point5, params, pvalues)


setwd('~/binary-regressor/tab')

cat(TeXtable(build_table(subset(alpha_coverage, n == 1000),
                         out_stat = 'cover90')), file = 'alphas_cover_90_1000.tex')
cat(TeXtable(build_table(subset(alpha_coverage, n == 2000),
                         out_stat = 'cover90')), file = 'alphas_cover_90_2000.tex')

cat(TeXtable(build_table(subset(alpha_coverage, n == 1000),
                         out_stat = 'cover95')), file = 'alphas_cover_95_1000.tex')
cat(TeXtable(build_table(subset(alpha_coverage, n == 2000),
                         out_stat = 'cover95')), file = 'alphas_cover_95_2000.tex')

cat(TeXtable(build_table(subset(alpha_coverage, n == 1000),
                         out_stat = 'cover97point5')),
    file = 'alphas_cover_97point5_1000.tex')
cat(TeXtable(build_table(subset(alpha_coverage, n == 2000),
                         out_stat = 'cover97point5')),
    file = 'alphas_cover_97point5_2000.tex')

rm(list = ls())
