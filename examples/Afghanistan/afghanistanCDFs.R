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
x <- kids$s08_formal_school

# The variable "s08_formal_school" is our outcome
# --- total normalized test score, Spring 2008
y <- kids$s08_both_norma_total

dat <- data.frame(x,y,z)
dat <- na.omit(dat)

# Try partialling out the exogenous covariates
# --- If they are not mis-measured, then IV gives the right coeffs for these!
datCovariates <- with(kids, data.frame(x, y, z,
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
b_tsls <- tsls(y ~ x + female + age + yrsvill + farsi + tajik + farmers +
                  agehead + educhead + nhh + land + sheep + distschool,
                 ~ z + female + age + yrsvill + farsi + tajik + farmers + 
                 agehead + educhead + nhh + land + sheep + distschool, 
              data = datCovariates)$coefficients

b_adjust <- b_tsls
b_adjust[2] <- 0
y_adjust <- datCovariates$y - model.matrix(~ x + female + age + yrsvill + 
                                             farsi + tajik + farmers + 
                                             agehead + educhead + nhh + 
                                             land + sheep + distschool, 
                                           data = datCovariates) %*% b_adjust

datCovariates$y_adjust <- y_adjust

dat2 <- with(datCovariates, data.frame("x" = x, "y" = y_adjust, "z" = z))


# Try out moment conditions with valid IV after partialling out covariates
dat0 <- subset(dat2, z == 0)
dat1 <- subset(dat2, z == 1)
dat10 <- subset(dat0, x == 1)
dat11 <- subset(dat1, x == 1)
dat00 <- subset(dat0, x == 0)
dat01 <- subset(dat1, x == 0)

p0 <- mean(dat0$x)
p1 <- mean(dat1$x)

library(MASS)
par(mfrow = c(2, 2))
truehist(dat0$y, main = "Y|Z=0", xlab = "")
truehist(dat1$y, main = "Y|Z=1", xlab = "")
truehist(dat10$y, main = "Y|T=1, Z=0", xlab = "")
truehist(dat11$y, main = "Y|T=1, Z=1", xlab = "")
par(mfrow = c(1,1))

par(mfrow = c(1, 2))
plot(ecdf(dat0$y), main = "Y | Z=0 vs Z=1", xlab = "", col = "blue")
lines(ecdf(dat1$y), xlab = "", col = "red")
legend("bottomright", c("Y|Z=0", "Y|Z=1"), fill = c("blue", "red"))
plot(ecdf(dat10$y), main = "Y | (T=1, Z=0) vs (T=1, Z=1)", xlab = "", col = "blue")
lines(ecdf(dat11$y), xlab = "", col = "red")
legend("bottomright", c("Y|(T=1, Z=0)", "Y|(T=1, Z=1)"), fill = c("blue", "red"))
par(mfrow = c(1,1))

# Set up the functions that appear in the moment equations
LHS <- function(tau, a1){
  f1 <- ecdf(dat1$y)
  f0 <- ecdf(dat0$y)
  (1 - a1) * (f1(tau) - f0(tau))
}

RHS0 <- function(tau, beta){
  f <- ecdf(dat10$y)
  f(tau + beta) - f(tau)
}

RHS1 <- function(tau, beta){
  f <- ecdf(dat11$y)
  f(tau + beta) - f(tau)
}

RHS <- function(tau, beta){
  p1 * RHS1(tau, beta) - p0 * RHS0(tau, beta)
}

MC <- function(tau, a1, beta){
  LHS(tau, a1) + RHS(tau, beta)
}

ourPlot <- function(a1, tau1 = 0 , tau2 = 1){
  betaRange <- seq(from = -2, to = 2, by = 0.1)
  plot(betaRange, MC(tau1, a1, betaRange), type = 'l', xlab = expression(beta), col = "blue", ylim = c(-1, 1))
  points(betaRange, MC(tau2, a1, betaRange), type = 'l', col = "red")
  abline(h = 0, lty = 2)
}

library(manipulate)
manipulate(ourPlot(a1, tau1, tau2), a1 = slider(0, 1, step = 0.05), 
           tau1 = slider(-2, 2, step = 0.1), tau2 = slider(-2, 2, step = 0.1))


# Bounds on a0 and a1 without valid instrument
F00 <- ecdf(dat00$y)
F01 <- ecdf(dat01$y)
F10 <- ecdf(dat10$y)
F11 <- ecdf(dat11$y)

F00tilde <- function(tau){
  (1 - p0) * F00(tau)
}
F01tilde <- function(tau){
  (1 - p1) * F01(tau)
}
F10tilde <- function(tau){
  p0 * F10(tau)
}
F11tilde <- function(tau){
  p1 * F11(tau)
}

a1_UB1_0 <- function(tau){
  F00tilde(tau) / (F00tilde(tau) + F10tilde(tau))
}
a1_UB2_0 <- function(tau){
  (1 - F00tilde(tau) - p0) / (1 - F00tilde(tau) - F10tilde(tau))
}
a1_UB1_1 <- function(tau){
  F01tilde(tau) / (F01tilde(tau) + F11tilde(tau))
}
a1_UB2_1 <- function(tau){
  (1 - F01tilde(tau) - p1) / (1 - F01tilde(tau) - F11tilde(tau))
}

a0_UB1_0 <- function(tau){
  F10tilde(tau) / (F10tilde(tau) + F00tilde(tau))
}
a0_UB2_0 <- function(tau){
  (1 - F10tilde(tau) - (1 - p0)) / (1 - F10tilde(tau) - F00tilde(tau))
}
a0_UB1_1 <- function(tau){
  F11tilde(tau) / (F11tilde(tau) + F01tilde(tau))
}
a0_UB2_1 <- function(tau){
  (1 - F11tilde(tau) - (1 - p1)) / (1 - F11tilde(tau) - F01tilde(tau))
}


range(dat00$y)
range(dat10$y)
range(dat11$y)
range(dat01$y)

tau_range <- seq(-1.55, 3.2, 0.01)
plot(tau_range, a1_UB1_0(tau_range), type = "s", col = "red", ylim = c(0,1),
     lwd = 2, xlab = expression(tau), ylab = expression(alpha[1]))
points(tau_range, a1_UB2_0(tau_range), type = "s", col = "blue", lwd = 2)
points(tau_range, a1_UB2_1(tau_range), type = "s", col = "green", lwd = 2)
points(tau_range, a1_UB1_1(tau_range), type = "s", col = "orange", lwd = 2)


plot(tau_range, a0_UB1_0(tau_range), type = "s", col = "red", ylim = c(0,1),
     lwd = 2, xlab = expression(tau), ylab = expression(alpha[0]))
points(tau_range, a0_UB2_0(tau_range), type = "s", col = "blue", lwd = 2)
points(tau_range, a0_UB2_1(tau_range), type = "s", col = "green", lwd = 2)
points(tau_range, a0_UB1_1(tau_range), type = "s", col = "orange", lwd = 2)

