setwd("~/binary-regressor/examples/AngristEtAl")
library(sas7bdat)
notest_raw <- read.sas7bdat("tab7.sas7bdat")
test_raw <- read.sas7bdat("tab7test.sas7bdat")

test <- with(test_raw, data.frame(TOTALPTS, VOUCH0, USESCH))
test <- test[complete.cases(test),]
names(test) <- c("score", "voucher", "scholarship")

# OLS - no controls
with(test, cov(score, scholarship)/var(scholarship))
# IV - no controls
with(test, cov(score, voucher)/cov(voucher, scholarship))


# Calculate various estimators 
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

names(test) <- c("y", "z", "x")
est(test)

test_fake <- test[sample(1:nrow(test), 5000, replace = TRUE),]
est(test_fake)


bogata95 <- subset(notest_raw, BOG95SMP == 1)
bogata95 <- with(bogata95, data.frame(VOUCH0, USESCH, REPT6, REPT7, REPT8))
bogata95 <- bogata95[complete.cases(bogata95),]
bogata95$rep <- with(bogata95, REPT6 + REPT7 + REPT8)
