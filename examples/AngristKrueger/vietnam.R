setwd("~/binary-regressor/examples/AngristKrueger/")
library(foreign)
cps_raw <- read.dta("extract.dta")

labels <- attributes(cps_raw)$var.labels
cbind(names(cps_raw), labels)

# Subset of people born between 1950 and 1953
cps <- subset(cps_raw, (yob50 == 1) | 
                       (yob51 == 1) |
                       (yob52 == 1) |
                       (yob53 == 1))


# Whites only: 1 = white
cps <- subset(cps, race == 1)
rm(cps_raw)

# Construct binary instrument: 1(draft lottery number < 100)
#   lott1 = 1(lottery number <= 25)
#   lott2 = 1(25 < lottery number <= 50)
#   lott3 = 1(50 < lottery number <= 75)
#   lott4 = 1(75 < lottery number <= 100)
cps$lowlott <- with(cps, lott1 + lott2 + lott3 + lott4)


## CPI obtained from inflation calculator 
# http://www.bls.gov/data/inflation_calculator.htm
# $1 in 1979 = $1.00 in 1979
#----NO DATA FOR 1980 in cps!-----
# $1 in 1981 = $0.80 in 1979
# $1 in 1982 = $0.75 in 1979
# $1 in 1983 = $0.73 in 1979
# $1 in 1984 = $0.70 in 1979
# $1 in 1985 = $0.67 in 1979


# Convert nominal annual wage to real
cps$rannwage <- with(cps, annwage * ((year == 79) * 1.00 + 
                                     (year == 81) * 0.80 + 
                                     (year == 82) * 0.75 + 
                                     (year == 83) * 0.73 + 
                                     (year == 84) * 0.70 + 
                                     (year == 85) * 0.67))


# Construct real weekly earnings
# (Set weekly wage to zero for the 266 individuals with zero weeks worked but
#  positive annual earnings.)
cps$rwwage <- with(cps, (weeks > 0) * rannwage / pmax(1, weeks))



cps1 <- with(cps, data.frame("y" = log(rannwage + 1), "x" = veteran, "z" = lowlott))
cps2 <- with(cps, data.frame("y" = log(rwwage + 1), "x" = veteran, "z" = lowlott))
cps3 <- with(cps, data.frame("y" = rannwage, "x" = veteran, "z" = lowlott))
cps4 <- with(cps, data.frame("y" = rwwage, "x" = veteran, "z" = lowlott))


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

est(subset(cps1, y < 6))
est(subset(cps2, y > 6))
est(subset(cps3, y > 5000 & y < 10000))

q0 <- quantile(subset(cps3, z == 0)$y, 1:99/100)
q1 <- quantile(subset(cps3, z == 1)$y, 1:99/100)
plot(q0, q1)
abline(0,1)
