# Simple Generalized Roy Model
# based on Heckman, Urzua & Vytlacil (2006)
set.seed(84827)
n <- 1e6
alpha <- 0.67
beta_bar <- 0.2 
cost <- 1.5 
gamma <- 0.2
r <- -0.9
u <- MASS::mvrnorm(n, c(0, 0), matrix(c(1, r, r, 1), 2, 2))
u0 <- u[, 1]
u1 <- u[, 2]
rm(u)
y0 <- alpha + u0
y1 <- alpha + beta_bar + u1
z <- rbinom(n, 1, 0.5)
dstar <- y1 - y0 - (cost - gamma * z)
d <- 1 * (dstar > 0)

never <- ((y1 - y0) - (cost - gamma)) <= 0
always <- (y1 - y0) - cost > 0
complier <- (!never) & (!always)

# The assumption to get conditional independence doesn't appear to be satisfied
par(mfrow = c(3, 2))
MASS::truehist(y0[always])
MASS::truehist(y1[always])
MASS::truehist(y0[never])
MASS::truehist(y1[never])
MASS::truehist(y0[complier])
MASS::truehist(y1[complier])
par(mfrow = c(1, 1))

# But the assumption to get same variance for compliers is satistfied.
# Does this come from normality? Symmetry? What about the other moments?
var(y0[complier])
var(y1[complier])
quantile(y1[complier], 1:9/10) - quantile(y0[complier], 1:9/10)
qqnorm(y0[complier])
qqnorm(y1[complier])
# It looks like this is just a shift family...
