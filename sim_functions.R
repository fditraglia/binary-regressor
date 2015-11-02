# A selection model with randomly assigned offer of treatment
# and non-differential measurement error
dgp <- function(a0, a1, b = 2, c = 0.5, g0 = -1, g1 = 5, s_noise = 1,
                s_ability = 1, q = 0.5, n = 500){
  
  z <- rbinom(n, 1, q) #offer of treatment
  ability <- rnorm(n, 0, s_ability)
  xstar <- as.numeric(g0 + g1 * z + ability > 0) #select into treatment
  e <- rnorm(n, 0, s_noise) + ability
  y <- c + b * xstar + e
  #mis-classification
  x <- (1 - xstar) * rbinom(n, 1, a0) + xstar * rbinom(n, 1, 1 - a1) 
  return(data.frame(x, y, z))
}

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

set.seed(8372)
sim <- t(replicate(5000, est(dgp(0.1, 0.3))))
sim <- as.data.frame(sim)
mean(sim$a1_a0)
hist(sim$a1_a0)
qqnorm(sim$a1_a0)
qqline(sim$a1_a0)
mean(sim$root1, na.rm = TRUE)
mean(sim$root2, na.rm = TRUE)
hist(sim$root1)
hist(sim$root2)
