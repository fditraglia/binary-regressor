setwd("~/binary-regressor/examples/AngristEtAl/")
library(haven)
library(AER)
library(ivpack)

tab7 <- read_sas("tab7.sas7bdat")
tab7test <- read_sas("tab7test.sas7bdat")

mymerge <- merge(tab7, tab7test)


names(tab7)
names(tab7test)

summary(ivreg(TOTALPTS ~ USESCH + AGE + SVY +
                STRATA1 + STRATA2 + STRATA3 + STRATA4 +
                DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 +
                DMONTH7 + DMONTH8 + DMONTH9 |
                VOUCH0 + AGE + SVY + 
                STRATA1 + STRATA2 + STRATA3 + STRATA4 +
                DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 +
                DMONTH7 + DMONTH8 + DMONTH9,
                data = mymerge))
                
                
grades <- tab7[,c("FINISH6", "FINISH7", "FINISH8")]

g <- function(x){
  out <- 5
  if(all(x == c(1, 0, 0)))
    out <- 6
  if(all(x == c(1, 1, 0)))
    out <- 7
  if(all(x == c(1, 1, 1)))
    out <- 8
  return(out)
}

highestGrade <- apply(grades, 1, g)
tab7$highestGrade <- highestGrade

rept <- tab7[,c("REPT", "REPT6", "REPT7", "REPT8")]
summary(rept)
apply(rept, 2, table)

f <- function(x){
  x[2] <- x[2] - 1
  x[3] <- x[3] - 1
  return(sum(x, na.rm = TRUE))
}

rept <- rept[,c("REPT6", "REPT7", "REPT8")]
totalRepeats <- apply(rept, 1, f)
tab7$totalRepeats <- totalRepeats

bogsample <- subset(tab7, BOG95SMP == 1)

summary(lm(highestGrade ~ USESCH + AGE + PHONE + SVY + STRATA1 + STRATA2 + 
             STRATA3 + STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + 
             DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + 
             DMONTH11, data = bogsample))


summary(ivreg(highestGrade ~ USESCH + AGE + SVY + STRATA1 + STRATA2 + 
             STRATA3 + STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + 
             DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + 
             DMONTH11 |  VOUCH0 + AGE + SVY + STRATA1 + STRATA2 + STRATA3 + 
             STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + 
             DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11, 
             data = bogsample))



summary(lm(totalRepeats ~ USESCH + AGE  + SVY + STRATA1 + STRATA2 + 
             STRATA3 + STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + 
             DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + 
             DMONTH11, data = bogsample))


summary(ivreg(totalRepeats ~ USESCH + AGE + SVY + STRATA1 + STRATA2 + 
             STRATA3 + STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + 
             DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + 
             DMONTH11 |  VOUCH0 + AGE + SVY + STRATA1 + STRATA2 + STRATA3 + 
             STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + 
             DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11, 
             data = bogsample))

Tobs <- bogsample$USESCH
z <- bogsample$VOUCH0
y1 <- bogsample$totalRepeats
y2 <- bogsample$highestGrade



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

b_tsls <- ivreg(totalRepeats ~ USESCH + AGE + SVY + STRATA1 + STRATA2 + 
             STRATA3 + STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + 
             DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + 
             DMONTH11 |  VOUCH0 + AGE + SVY + STRATA1 + STRATA2 + STRATA3 + 
             STRATA4 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + 
             DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11, 
             data = bogsample)$coefficients


b_adjust <- b_tsls
b_adjust[2] <- 0
design.matrix <- model.matrix( ~ USESCH + AGE + SVY + STRATA1 + STRATA2 +  
                                 STRATA3 + STRATA4 + DMONTH1 + DMONTH2 + 
                                 DMONTH3 + DMONTH4 +  DMONTH5 + DMONTH6 + 
                                 DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + 
                                 DMONTH11, data = bogsample)
y_adjust <- y1 - design.matrix %*% b_adjust


par(mfrow = c(2,2))
hist(y_adjust[Tobs == 0 & z == 0],  main = "Highest Grade: z = 0, T = 0")
hist(y_adjust[Tobs == 1 & z == 0],  main = "Highest Grade: z = 1, T = 0")
hist(y_adjust[Tobs == 0 & z == 1],  main = "Highest Grade: z = 0, T = 1")
hist(y_adjust[Tobs == 1 & z == 1],  main = "Highest Grade: z = 1, T = 1")
par(mfrow = c(1,1))


dat <- data.frame(x = Tobs, y = y_adjust, z = z)
est(dat)




