n <- 1e6
a0 <- 0.1
a1 <- 0.2
q <- 0.5
b0 <- -0.2
b1 <- 0.4
g0 <- -0.5
g1 <- 1
z <- rbinom(n, 1, q) 
ability <- rnorm(n)
Tstar <- rbinom(n, 1, pnorm(g0 + g1 * z + ability))
y <- rbinom(n, 1, pnorm(b0 + b1 * Tstar + ability))
Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1)

dat <- data.frame(y = y, Tobs = Tobs, z = z, Tstar = Tstar)

# It appears that this model features heterogeneous treatment effects!
mean(y[Tstar == 0 & z == 0])
mean(y[Tstar == 0 & z == 1])
mean(y[Tstar == 1 & z == 0])
mean(y[Tstar == 1 & z == 1])

p0 <- mean(Tobs[z == 0])
p1 <- mean(Tobs[z == 1])

p_y_z0 <- mean(y[z == 0])
p_y_z1 <- mean(y[z == 1])
Dy <- p_y_z1 - p_y_z0

p_yTobs0_z0 <- mean(with(subset(dat, z == 0), (y == 1) & (Tobs == 0)))
p_yTobs0_z1 <- mean(with(subset(dat, z == 1), (y == 1) & (Tobs == 0)))
DyTobs0 <- p_yTobs0_z1 - p_yTobs0_z0
p_yTobs1_z0 <- mean(with(subset(dat, z == 0), (y == 1) & (Tobs == 1)))
p_yTobs1_z1 <- mean(with(subset(dat, z == 1), (y == 1) & (Tobs == 1)))
DyTobs1 <- p_yTobs1_z1 - p_yTobs1_z0

# These bounds are, unfortunately, completely uniformative
(DyTobs1 - (p1 - p0)) / Dy
(DyTobs1) / Dy

DyTobs0 / Dy
(DyTobs0 + (p1 - p0)) / Dy

# Check that the formulas are correct
(1 - a0 - a1) * cov(y, z) / cov(Tobs, z)
cov(y, z) / cov(Tstar, z)
((DyTobs1 - a0 * Dy) - (a1 * Dy - DyTobs0)) / (p1 - p0)

# What about looking at y == 0?
