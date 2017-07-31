library(mbereg)
set.seed(7291)
a0_true <- 0.1
a1_true <- 0.2
b_true <- 2
simdat <- dgp(a0 = a0_true, a1 = a1_true, b = b_true, n = 1000)
B <- 10000
normal_sims <- matrix(rnorm(11 * B), 11, B)

#system.time(pvalue <- GMS_test(a0 = a0_true, a1 = a1_true, beta = b_true,
#                               dat = simdat, normal_sims = normal_sims))

Wald <- with(simdat, cov(y, z) / cov(z, Tobs))
RF <- with(simdat, cov(y, z) / var(z))
p1 <- with(simdat, mean(Tobs[z == 1]))
p0 <- with(simdat, mean(Tobs[z == 0]))
q <- with(simdat, mean(z))
a0_upper <- min(p0, p1)
a1_upper <- min(1 - p0, 1 - p1)


est <- function(inData){
  OLS <- with(inData, cov(Tobs,y)/var(Tobs))
  IV <- with(inData, cov(z,y)/cov(z,Tobs))
  dat0 <- subset(inData, z == 0)
  dat1 <- subset(inData, z == 1)
  p1_hat <- mean(dat1$Tobs)
  p0_hat <- mean(dat0$Tobs)
  W <- IV
  Dy2 <- mean(dat1$y^2) - mean(dat0$y^2)
  DyT <- mean(dat1$y * dat1$Tobs) - mean(dat0$y * dat0$Tobs)
  R <- (Dy2 - 2 * W * DyT) / (W * (p1_hat - p0_hat))
  a1_a0 <- 1 + R/W
  Dy3 <- mean(dat1$y^3) - mean(dat0$y^3)
  Dy2T <- mean(dat1$y^2 * dat1$Tobs) - mean(dat0$y^2 * dat0$Tobs)
  S <- (Dy3 - 3 * W * (Dy2T + R * DyT)) / (W * (p1_hat - p0_hat))
  term1 <- -0.5 * R/W
  disc_sq <- 3 * R^2 - 2 * S
  if(disc_sq > 0){
    disc <- sqrt(3 * R^2 - 2 * S)
    term2 <- 0.5 * disc/W
    root1 <- term1 + term2
    root2 <- term1 - term2
    a1 <- 1 - max(root1, root2)
    a0 <- min(root1, root2)
    b <-
    out <- c(OLS, IV, a1_a0, a0, a1)
    names(out) <- c("OLS", "IV", "a1_a0", "a0", "a1")
    return(out)
  } else {
    return(NA)
  }
}

myest <- function(inData){
 eta1 <- with(inData, cov(y, z))
 eta2 <- with(inData, cov(y^2, z))
 eta3 <- with(inData, cov(y^3, z))
 tau1 <- with(inData, cov(Tobs * y, z))
 tau2 <- with(inData, cov(Tobs * y^2, z))
 PI <- with(inData, cov(Tobs, z))
 theta1 <- eta1 / PI
 theta2 <- (2 * tau1 * theta1 - eta2) / PI
 theta3 <- (eta3 - 3 * tau2 * theta1 + 3 * tau1 * theta2) / PI
 A <- theta2 / theta1^2
 B <- theta3 / theta1^3
 beta_squared <- 3 * theta2^2 / theta1^2 - 2 * theta3 / theta1
 if(beta_squared >= 0) return(sqrt(beta_squared))
 else return(NA)
}

est(simdat)
myest(simdat)


# Constraint function: inside CI if p-value >= 0.05
g_lower <- function(x) {
  pvalue <- GMS_test(a0 = x[1], a1 = x[2], beta = x[3],
                     dat = simdat, normal_sims = normal_sims)
  return(c(x[3], 0.05 - pvalue)) # constraint is g(x) <= 0
}

g_upper <- function(x) {
  pvalue <- GMS_test(a0 = x[1], a1 = x[2], beta = x[3],
                     dat = simdat, normal_sims = normal_sims)
  return(c(-x[3], 0.05 - pvalue)) # constraint is g(x) <= 0
}

lower <- crs::snomadr(eval.f = g_lower,
                      n = 3,
                      x0 = c(0.12, 0.23, 1.9),
                      bbin = c(0, 0, 0), # Continuous inputs
                      bbout = c(0, 1), # 1st output of eval.f is OBJ, 2nd is EB
                      lb = c(0, 0, -10),
                      ub = c(0.9, 0.9, 10))

upper <- crs::snomadr(eval.f = g_upper,
                      n = 3,
                      x0 = c(0.12, 0.23, 1.9),
                      bbin = c(0, 0, 0), # Continuous inputs
                      bbout = c(0, 1), # 1st output of eval.f is OBJ, 2nd is EB
                      lb = c(0, 0, -10),
                      ub = c(0.9, 0.9, 10))

c(lower$solution[3], upper$solution[3])
Wald
RF

