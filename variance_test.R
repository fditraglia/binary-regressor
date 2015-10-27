# Since this is the DGP these are the "star" probabilities but
# to reduce typing I don't indicate this, e.g. p denotes p* in the notation
# of the paper. Further, since T is TRUE I call the treatment x. Since I do
# need to distinguish between the true and observed treatment I preserve the
# difference between T and T* from the paper as x and xstar in the code


binaryDGP <- function(a0, a1, p0 = 0.2, p1 = 0.7, q = 0.5, c = 0, b = 1,
                      d0 = -1, d1 = 1, sigma = 1, N = 1000000){
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

foo <- binaryDGP(0.05, 0.4)
dat <- foo$dat
dat0 <- subset(dat, z == 0)
dat1 <- subset(dat, z == 1)
ybar <- mean(dat$y)
p <- mean(dat$x)
p1 <- mean(dat1$x)
p0 <- mean(dat0$x)
Var_y1 <- var(dat1$y)
Var_x1 <- var(dat1$x)
Var_y0 <- var(dat0$y)
Var_x0 <- var(dat0$x)
W <- (mean(dat1$y) - mean(dat0$y)) / (p1 - p0)
Wtilde <- (mean(dat1$y * dat1$x) - mean(dat0$y * dat0$x)) / (p1 - p0)

# This should equal a0 - a1
(2 * p - 1 - p1 - p0) + (2 / W) * (Wtilde - ybar) -
  (Var_y1 - Var_y0) / ((p1 - p0) * W^2)
with(foo$param, a0 - a1)
# and it does!

# Check the other equations!!!
# Wald
W
1/(1 - with(foo$param, a0 + a1))

# Modified Wald
mu <- foo$param$m11 * (p1 - foo$param$a0) - foo$param$m10 * (p0 - foo$param$a0)
Wtilde
ybar + W * ((1 - p) + with(foo$param, a0 - a1)) + mu / (p1 - p0)

# Original Variance Equation (before solving with modified Wald)
W^2 * ( (Var_x1 - Var_x0) + with(foo$param, a0 - a1) * (p1 - p0)) +
  2 * W * mu
Var_y1 - Var_y0

# Check the equations upon which it is based
0.7 * (1 - 0.7) + 1 + 2 * cov(dat1$xstar, dat1$u)
Var_y1
