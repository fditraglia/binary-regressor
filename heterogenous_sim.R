# In this design, u1 determins compliance type:
#     Complier:       -pi <= 2 * u1 - 1 <= pi
#     Always-taker:              u1 <= 0.5 * (1 - pi)
#     Never-taker:               u1 > 0.5 * (1 + pi)
dgp1 <- function(n, pi, b, a0, a1){
  z <- rbinom(n, 1, 0.5)
  u1 <- runif(n)
  always <- u1 <= 0.5 * (1 - pi)
  never <- u1 > 0.5 * (1 + pi)
  complier <- (-pi <= 2 * u1 - 1) & (2 * u1 - 1 <= pi)
  u2 <- runif(n)
  # If z = 0, then Tstar = 1 * (u1 <= 0.5 * (1 - pi))
  # If z = 1, then Tstar = 1 * (u1 <= 0.5 * (1 + pi))
  Tstar <- 1 * (u1 <= 0.5 * (1 + (-1)^z * pi))
  het <- (0.5 * qnorm(u1) + qnorm(u2)) / (1 + 0.25)
  y <- pnorm(b * Tstar + het)
  Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1) 
  out <- data.frame(z = z, Tobs = Tobs, y = y, Tstar = Tstar, always = always,
                    never = never, complier = complier)
  return(out)
}

LATE <- function(dat){
  with(dat, cov(z,y) / cov(Tstar, z))
}

ITT <- function(dat){
 with(dat, cov(z,y) / var(z)) 
}

Wald <- function(dat){
  with(dat, cov(z,y) / cov(Tobs, z))
}

parameters <- function(dat){
  data.frame(ITT = ITT(dat), LATE = LATE(dat), Wald = Wald(dat))
}

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
  disc <- sqrt(3 * R^2 - 2 * S)
  term2 <- 0.5 * disc/W
  root1 <- term1 + term2
  root2 <- term1 - term2
  BBS <- max(root1, root2) - min(root1, root2)
  b <- W * BBS
  out <- c("OLS" = OLS, "IV" = IV, "a1_a0" = a1_a0, "root1" = root1, 
           "root2" = root2, "BBS" = BBS, "bhat" = b)
  return(out)
}

set.seed(927)

sim1 <- dgp1(n = 1000000, pi = 0.5, b = 3, a0 = 0.2, a1 = 0.2)
ITT(sim1)
LATE(sim1)
Wald(sim1)
est(sim1)

# Potential outcome distributions etc.
Y1_always <- subset(sim1, (Tobs == 1) & (z == 0))$y
Y0_never <- subset(sim1, (Tobs == 0) & (z == 1))$y
mean(Y1_always)
mean(Y0_never)
mean(subset(sim1, always == TRUE)$y)
mean(subset(sim1, never == TRUE)$y)
mean(subset(sim1, (complier == TRUE) & (Tstar == 0))$y)
mean(subset(sim1, (complier == TRUE) & (Tstar == 1))$y)

var(subset(sim1, (complier == TRUE) & (Tstar == 0))$y)
var(subset(sim1, (complier == TRUE) & (Tstar == 1))$y)
