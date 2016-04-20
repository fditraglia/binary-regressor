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
dat0 <- subset(dat, z == 0)
dat1 <- subset(dat, z == 1)

table(dat0$x)
table(dat1$x)

par(mfrow = c(1,2))
hist(dat0$y)
hist(dat1$y)
par(mfrow = c(1,1))


est <- function(inData){
  OLS <- with(inData, cov(x,y)/var(x))
  IV <- with(inData, cov(z,y)/cov(z,x))
  dat0 <- subset(inData, z == 0)
  dat1 <- subset(inData, z == 1)
  p1_hat <- mean(dat1$x) 
  p0_hat <- mean(dat0$x) 
  W <- IV
  Dy2 <- mean(dat1$y^2) - mean(dat0$y^2)
  DyT <- mean(dat1$y * dat1$x) - mean(dat0$y * dat0$x)
  R <- (Dy2 - 2 * W * DyT) / (W * (p1_hat - p0_hat))
  a1_a0 <- 1 + R/W 
  Dy3 <- mean(dat1$y^3) - mean(dat0$y^3)
  Dy2T <- mean(dat1$y^2 * dat1$x) - mean(dat0$y^2 * dat0$x)
  S <- (Dy3 - 3 * W * (Dy2T + R * DyT)) / (W * (p1_hat - p0_hat))
  term1 <- -0.5 * R/W
  disc <- sqrt(3 * R^2 - 2 * S)
  term2 <- 0.5 * disc/W
  root1 <- term1 + term2
  root2 <- term1 - term2
  out <- c(OLS, IV, a1_a0, root1, root2)
  names(out) <- c("OLS", "IV", "a1_a0", "root1", "root2")
  return(out)
}

est(dat)

badboot <- function(){
  dat0_boot <- dat0[sample(1:nrow(dat0), replace = TRUE),]
  dat1_boot <- dat1[sample(1:nrow(dat1), replace = TRUE),]
  dat_boot <- rbind(dat0_boot, dat1_boot)
  est(dat_boot)
}

worseboot <- function(){
  dat_boot <- dat[sample(1:nrow(dat), replace = TRUE),]
  est(dat_boot)
}

set.seed(11)
badbootdraws <- as.data.frame(t(replicate(1000, badboot())))
head(badbootdraws)
adiff <- badbootdraws$a1_a0
hist(adiff)
quantile(adiff, c(0.025, 0.975))
mean(adiff)

set.seed(11)
worsebootdraws <- as.data.frame(t(replicate(1000, worseboot())))
head(worsebootdraws)
adiff <- worsebootdraws$a1_a0
hist(adiff)
quantile(adiff, c(0.025, 0.975))
mean(adiff)

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
est(dat2)

library(MASS)
library(tikzDevice)
setwd("~/binary-regressor/fig/")
tikz(file = "AfghanHist.tex", width = 6, height = 6)
par(mfrow = c(2,2))
with(subset(dat2, (z == 0) & (x == 0)), 
     truehist(y, ylab = "$T = 0$", main = "$z = 0$", xlab = "", col = "lightskyblue1", yaxt = "n"))
legend(x = "top", legend = "$N=526$", bty = "n")

with(subset(dat2, (z == 1) & (x == 0)), 
     truehist(y, ylab = "", main = "$z = 1$", xlab = "", col = "lightskyblue1", 
              yaxt = "n"))
legend(x = 0.5, y = 0.6, legend = "$N=223$", bty = "n")

with(subset(dat2, (z == 0) & (x == 1)), 
     truehist(y, ylab = "$T = 1$", main = "", xlab = "", col = "lightskyblue1",
              yaxt = "n"))
legend(x = "topleft", legend = "$N=171$", bty = "n")

with(subset(dat2, (z == 1) & (x == 1)), 
     truehist(y, ylab = "", main = "", xlab = "", col = "lightskyblue1", yaxt = "n"))
legend(x = "topleft", legend = "$N=548$", bty = "n")
par(mfrow = c(1,1))
dev.off()

# Try bootstraps that impose the cluster structure


# form groups for cluster bootstrapping
cfull <- unique(datCovariates$c)
datCovariates0 <- subset(datCovariates, z == 0)
c0 <- unique(datCovariates0$c)
datCovariates1 <- subset(datCovariates, z == 1)
c1 <- unique(datCovariates1$c)

# Form lists of village clusters for boostrapping
clust1 <- lapply(c1, function(x) datCovariates[datCovariates$c == x,])
clust0 <- lapply(c0, function(x) datCovariates[datCovariates$c == x,]) 

first_stage <- formula(~ z + female + age + yrsvill + farsi + tajik + 
                         farmers + agehead + educhead + nhh + land + 
                         sheep + distschool)
second_stage <- formula(~ x + female + age + yrsvill + farsi + tajik + 
                          farmers + agehead + educhead + nhh + land + 
                          sheep + distschool)

# Try a bootstrap with covariates imposing cluster structure
goodboot <- function(){
  dat0_boot <- do.call("rbind", clust0[sample(1:length(clust0), replace = TRUE)])
  dat1_boot <- do.call("rbind", clust1[sample(1:length(clust1), replace = TRUE)])
  dat_boot <- rbind(dat0_boot, dat1_boot)
  y <- matrix(dat_boot$y)
  X <- model.matrix(second_stage, dat_boot)
  Z <- model.matrix(first_stage, dat_boot)
  Qz <- qr.Q(qr(Z))
  Xtilde <- crossprod(Qz, X)
  ytilde <- crossprod(Qz, y)
  qrTilde <- qr(Xtilde)
  Rtilde <- qr.R(qrTilde)
  Qtilde <- qr.Q(qrTilde)
  b_IV <- backsolve(Rtilde, crossprod(Qtilde, ytilde))
  b_IV[2] <- 0
  y_boot_adjust <- y - X %*% b_IV
  est(data.frame("x" = dat_boot$x, "y" = y_boot_adjust, "z" = dat_boot$z))
}


set.seed(11)
goodbootdraws <- as.data.frame(t(replicate(5000, goodboot())))
head(goodbootdraws)
adiff <- goodbootdraws$a1_a0

library(MASS)
library(tikzDevice)
tikz(file = "AfghanBoot.tex", width = 6, height = 4)
setwd("~/binary-regressor/fig")
truehist(adiff, main = "Cluster Bootstrap Distribution of $\\hat{\\alpha}_1 - \\hat{\\alpha}_0$", col = "lightskyblue1", xlab = "", yaxt = "n")
legend(x = 0.3, y = 3.5, bty = "n", "$p = 0.13$")
dev.off()

quantile(adiff, c(0.066, 0.934))
mean(adiff)

with(dat, table(x,z))


# Now try out the new moment conditions
dat0 <- subset(dat, z == 0)
dat1 <- subset(dat, z == 1)
dat10 <- subset(dat0, x == 1)
dat11 <- subset(dat1, x == 1)

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
  betaRange <- seq(from = -5, to = 5, by = 0.1)
  plot(betaRange, MC(tau1, a1, betaRange), type = 'l', xlab = expression(beta), col = "blue", ylim = c(-1, 1))
  points(betaRange, MC(tau2, a1, betaRange), type = 'l', col = "red")
  abline(h = 0, lty = 2)
}

library(manipulate)
manipulate(ourPlot(a1, tau1, tau2), a1 = slider(0, 1, step = 0.05), 
           tau1 = slider(-2, 2, step = 0.1), tau2 = slider(-2, 2, step = 0.1))

