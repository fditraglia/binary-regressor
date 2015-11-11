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
goodbootdraws <- as.data.frame(t(replicate(2000, goodboot())))
head(goodbootdraws)
adiff <- goodbootdraws$a1_a0
hist(adiff)
quantile(adiff, c(0.05, 0.95))
mean(adiff)







