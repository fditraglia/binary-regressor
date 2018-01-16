setwd("~/binary-regressor/examples/Afghanistan/")
library(foreign)
kids <- read.dta("afghanistan_anonymized_data.dta")

labels <- attributes(kids)$var.labels
labels <- cbind(names(kids), labels)
labels

# Note: there are some mistakes in the data dictionary!


# The variable "treatment" is actually our instrument
# --- living in a village that gets a school
z <- kids$treatment

# The variable "f08_formal_school" is our treatment
# --- enrolled in formal school, Spring 2008
Tobs <- kids$s08_formal_school

# The variable "s08_formal_school" is our outcome
# --- total normalized test score, Spring 2008
y <- kids$s08_both_norma_total


datCovariates <- with(kids, data.frame(Tobs, y, z,
                                       "c" = clustercode,
                                       "female" = s08_girls_cnt,
                                       "age" = s08_age_cnt,
                                       "yrsvill" = s08_duration_village_cnt,
                                       "farsi" = s08_farsi_cnt,
                                       "tajik" = s08_tajik_cnt,
                                       "farmers" = s08_farmer_cnt,
                                       "agehead" = s08_age_head_cnt,
                                       "educhead" = s08_yrs_ed_head_cnt,
                                       "nhh" = s08_num_ppl_hh_cnt,
                                       "land" = s08_jeribs_cnt,
                                       "sheep" = s08_num_sheep_cnt,
                                       "distschool" = s08_nearest_scl))

# remove missing observations
datCovariates <- na.omit(datCovariates)

# Partial out the covariates using 2SLS since coeffs for exog regressors are
# not affected by the measurement error in the binary indicator
library(sem)
b_tsls <- tsls(y ~ Tobs + female + age + yrsvill + farsi + tajik + farmers +
                  agehead + educhead + nhh + land + sheep + distschool,
                 ~ z + female + age + yrsvill + farsi + tajik + farmers +
                 agehead + educhead + nhh + land + sheep + distschool,
              data = datCovariates)$coefficients

b_adjust <- b_tsls
b_adjust[2] <- 0
y_adjust <- datCovariates$y - model.matrix(~ Tobs + female + age + yrsvill +
                                             farsi + tajik + farmers +
                                             agehead + educhead + nhh +
                                             land + sheep + distschool,
                                           data = datCovariates) %*% b_adjust

datCovariates$y_adjust <- y_adjust

dat <- with(datCovariates, data.frame("Tobs" = Tobs, "y" = y_adjust, "z" = z))

rm(y, Tobs, z, kids, labels, datCovariates, y_adjust, b_adjust, b_tsls)



with(dat, plot(density(y[Tobs == 0 & z == 0])))
with(dat, plot(density(y[Tobs == 1 & z == 0])))
with(dat, plot(density(y[Tobs == 1 & z == 1])))
with(dat, plot(density(y[Tobs == 0 & z == 1])))

with(dat, hist(y[Tobs == 1 & z == 1]))

# Points estimates and confidence interval for Wald estimator
iv_slope <- with(dat, cov(y, z) / cov(Tobs, z))
iv_intercept <- with(dat, mean(y - iv_slope * Tobs))
epsilon_hat <- with(dat, y - iv_intercept - iv_slope * Tobs)
s_epsilon <- sqrt(sum(epsilon_hat^2) / (length(epsilon_hat) - 2))
se <- with(dat, (s_epsilon / sqrt(length(epsilon_hat)) * sd(z) / cov(Tobs, z)))
delta2 <- 0.05 / 2
UCL <- iv_slope + qnorm(1 - delta2 / 2) * se
LCL <- iv_slope - qnorm(1 - delta2 / 2) * se
theta1_CI <- c(LCL, UCL)

# Point Estiamte and confidence interval for OLS estimator
with(dat, cov(Tobs, y) / var(Tobs))

# Make plot of joint CI for (a0, a1) using the Burde and Linden data
library(mbereg)

set.seed(619283)
nB <- 5000
normal_sims <- matrix(rnorm(nB * 14), nrow = 14, ncol = nB)

alpha_grid <- seq(0, 1, 0.01)
alpha_grid <- expand.grid(a0 = alpha_grid, a1 = alpha_grid)
alpha_grid <- subset(alpha_grid, a0 + a1 < 1)
test_alphas <- GMS_test_alphas_nondiff
pvalues <- sapply(1:nrow(alpha_grid),
                  function(i) test_alphas(alpha_grid$a0[i], alpha_grid$a1[i],
                                          dat, normal_sims))





















