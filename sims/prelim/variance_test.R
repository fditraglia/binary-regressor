# Since this is the DGP these are the "star" probabilities but
# to reduce typing I don't indicate this, e.g. p denotes p* in the notation
# of the paper. Further, since T is TRUE I call the treatment x. Since I do
# need to distinguish between the true and observed treatment I preserve the
# difference between T and T* from the paper as x and xstar in the code


binaryDGP <- function(a0, a1, p0 = 0.3, p1 = 0.6, q = 0.3, c = 0.3, 
                      b = -1, d0 = -1.5, d1 = 1.5, sigma = 2, N = 10000000){
  p10 <- p0 * (1 - q)
  p11 <- p1 * q
  p01 <- q - p11
  p00 <- 1 - p01 - p10 - p11
  p <- p10 + p11
  m01 <- (p1 / (p1 - p0)) * ((1 - p0) * d0 + p0 * d1)
  m00 <- m01 - d0
  m11 <- (p1 - 1)/p1 * m01
  m10 <- m11 - d1
  z <- rbinom(N, 1, q)
  xstar <- (1 - z) * rbinom(N, 1, p0) + z * rbinom(N, 1, p1)
  sigma0 <- sqrt(sigma^2 - (m10^2 * p0 + m00^2 * (1 - p0)))
  sigma1 <- sqrt(sigma^2 - (m11^2 * p1 + m01^2 * (1 - p1)))
  e <- (1 - z) * rnorm(N, 0, sigma0) + z * rnorm(N, 0, sigma1)
  m <- (1 - xstar) * (1 - z) * m00 + xstar * (1 - z) * m10 +
    (1 - xstar) * z * m01 + xstar * z * m11
  u <- e + m
  y <- c + b * xstar + u
  x <- (1 - xstar) * rbinom(N, 1, a0) + xstar * rbinom(N, 1, 1 - a1)
  dat <- data.frame(x, y, z, xstar, u)
  param <- data.frame(a0, a1, b, c, sigma, d0, d1, p0, p1, q,
                      m00, m01, m10, m11, p00, p01, p10, p11)
  return(list(dat = dat, param = param))
}

set.seed(208)

foo <- binaryDGP(0.2, 0.05)
dat <- foo$dat
dat0 <- subset(dat, z == 0)
dat1 <- subset(dat, z == 1)
ybar <- mean(dat$y)
p_hat <- mean(dat$x)
p1_hat <- mean(dat1$x)
p0_hat <- mean(dat0$x)
Var_y1 <- var(dat1$y)
Var_x1 <- var(dat1$x)
Var_y0 <- var(dat0$y)
Var_x0 <- var(dat0$x)
W <- (mean(dat1$y) - mean(dat0$y)) / (p1_hat - p0_hat)
Wtilde <- (mean(dat1$y * dat1$x) - mean(dat0$y * dat0$x)) / (p1_hat - p0_hat)

# This should equal a0 - a1
(2 * p_hat - 1 - p1_hat - p0_hat) + (2 / W) * (Wtilde - ybar) -
  (Var_y1 - Var_y0) / ((p1_hat - p0_hat) * W^2)
with(foo$param, a0 - a1)
# and it does!

# Check the other equations!!!
# Wald
W
foo$param$b /(1 - with(foo$param, a0 + a1))

# Modified Wald
mu <- foo$param$m11 * (p1_hat - foo$param$a0) - foo$param$m10 * (p0_hat - foo$param$a0)
Wtilde
ybar + W * ((1 - p_hat) + with(foo$param, a0 - a1)) + mu / (p1_hat - p0_hat)

# Original Variance Equation (before solving with modified Wald)
W^2 * ( (Var_x1 - Var_x0) + with(foo$param, a0 - a1) * (p1_hat - p0_hat)) +
  2 * W * mu
Var_y1 - Var_y0



#---------------- Test out equation for (a0 - a1) from October 28th
R1 <- mean(dat1$y^2) - mean(dat0$y^2)
R2 <- mean(dat1$y * dat1$x) - mean(dat0$y * dat0$x)
#R2 should be Wtilde up to scale:
R2 / (p1_hat - p0_hat)
Wtilde

mu_tilde <- with(foo$param, (p1_hat - a0) * (m11 + c) - (p0_hat - a0) * (m10 + c))
mu_tilde - mu 
# Should equal the following:
foo$param$c * (p1_hat - p0_hat) #and it does!

#Check the the Delta(yT) equation
(1 - foo$param$a1) * W * (p1_hat - p0_hat) + mu_tilde
#Should equal R2
R2 #and it does!

#Check the Delta(y^2) equation
foo$param$b * W * (p1_hat - p0_hat) + 2 * W * mu_tilde
#Should equal R1
R1 #And it does!

#Check the expression for the identified quantity R
R <- (R1 - 2 * W * R2) / (W * (p1_hat - p0_hat))
R #should equal the following
foo$param$b - 2 * (1 - foo$param$a1) * W
# and it does!

#Should be (a0 - a1)
-1 - R/W
with(foo$param, a0 - a1)
# and it is!


#Try checking the solution for b using third moments
# Seems to contain a small mistake...
dy2 <- R1
dyT <- R2
dy3 <- mean(dat1$y^3) - mean(dat0$y^3)
dy2T <- mean(dat1$y^2 * dat1$x) - mean(dat0$y^2 * dat0$x)

A <- W * (p1_hat - p0_hat)
B <- 3 * W * (dyT + 2 * R * (p1_hat - p0_hat))
C <- 3 * W * (dy2T + 2 * R * dyT + 2 * R^2 * (p1_hat - p0_hat)) - dy3

polyroot(c(C, B, A))

#Check the equations upon which the quadratic is based
v00 <- mean(subset(dat0, xstar == 0)$u^2)
v01 <- mean(subset(dat1, xstar == 0)$u^2)
v10 <- mean(subset(dat0, xstar == 1)$u^2)
v11 <- mean(subset(dat1, xstar == 1)$u^2)

with(foo$param, v00 * (1 - p0) + v01 * p0)
with(foo$param, v10 * (1 - p1) + v11 * p1)
with(foo$param, sigma)^2

v <- with(foo$param, p1 * v11 - p0 * v10)
v_tilde <- with(foo$param, p1 * (v11 + c^2) - p0 * (v10 + c^2))

# Difference of expectation of y^3 equation
foo$param$b^2 * W * (p1_hat - p0_hat) + 3 * foo$param$b * W * mu_tilde + 3 * W * v_tilde
# Should equal dy3...
dy3 # Close enough!

# Difference of E[y^2 T] equation
foo$param$b * (1 - foo$param$a1) * W * (p1_hat - p0_hat) + 2 * (1 - foo$param$a1) * W * mu_tilde + v_tilde
# Should equal dy2T...
dy2T #Yes!
