library(mvtnorm)

solve_quadratic <- function(a, b, c) {
  stopifnot(all(a != 0))
  stopifnot(is.atomic(a) && is.atomic(b) && is.atomic(c))
  stopifnot(is.numeric(a) && is.numeric(b) && is.numeric(c))
  n <- length(a)
  stopifnot(identical(n, length(b)))
  stopifnot(identical(n, length(c)))
  d <- b^2 - 4 * a * c
  q <- rep(NA_complex_, n)
  real <- d >= 0
  # sign(0) = 0, but here we want it to be 1
  b_sign <- -1 * (b < 0) + 1 * (b >= 0)
  q[real] <- -0.5 * (b[real] + b_sign[real] * sqrt(d[real]))
  q[!real] <- -0.5 * (b[!real] + b_sign[!real] * 1i * sqrt(abs(d[!real])))
  x1 <- q / a # Function has already terminated if any(a == 0)
  ok <- q != 0 # q equals zero when b = c = 0
  x2 <- rep(NA_complex_, n)
  x2[ok] <- c[ok] / q[ok]
  x2[!ok] <- 0
  data.frame(root1 = x1, root2 = x2, real = real)
}


dgp <- function(a0, a1, b = 1, n = 1000, d = 0.15, rho = 0.5){
  n_treat <- ceiling(n/2)
  n_control <- n - n_treat
  z <- c(rep(0, n_control), rep(1, n_treat)) # offer of treatment
  errors <- rmvnorm(n, sigma = matrix(c(1, rho, rho, 1), 2, 2, byrow = TRUE))
  g0 <- qnorm(d) 
  g1 <- qnorm(1 - d) - qnorm(d) 
  Tstar <- as.numeric(g0 + g1 * z + errors[,2] > 0) #select into treatment
  y <- b * Tstar + errors[,1]
  #mis-classification
  Tobs <- (1 - Tstar) * rbinom(n, 1, a0) + Tstar * rbinom(n, 1, 1 - a1) 
  return(data.frame(Tobs, Tstar, y, z))
}

set.seed(5176392)

a0_true <- 0.1
a1_true <- 0.2

dat <- dgp(a0_true, a1_true, n = 1000000)

F0 <- ecdf(with(dat, y[z == 0]))
F1 <- ecdf(with(dat, y[z == 1]))
F10 <- ecdf(with(dat, y[Tobs == 1 & z == 0]))
F11 <- ecdf(with(dat, y[Tobs == 1 & z == 1]))
F00 <- ecdf(with(dat, y[Tobs == 0 & z == 0]))
F01 <- ecdf(with(dat, y[Tobs == 0 & z == 1]))
p0 <- with(dat, mean(Tobs[z == 0]))
p1 <- with(dat, mean(Tobs[z == 1]))

# Same for a0 and a1
c2 <- function(x){
  F1(x) - F0(x)  
} 

c1_a0 <- function(x){
  term1 <- p1 * F0(x) - p0 * F1(x)
  term2 <- p0 * F10(x) - p1 * F11(x)
  return(term1 + term2)
}

c1_a1 <- function(x){
 term1 <- (1 - p1) * F0(x) - (1 - p0) * F1(x)
 term2 <- (1 - p0) * F00(x) - (1 - p1) * F00(x)
 return(term1 + term2)
}

c0_a0 <- function(x){
  (p1 * p0) * (F11(x) - F10(x))
}

c0_a1 <- function(x){
  (1 - p0) * (1 - p1) * (F01(x) - F00(x))
}

# Discriminants
d_a0 <- function(x){
  c1_a0(x)^2 - 4 * c2(x) * c0_a0(x)
}

d_a1 <- function(x){
  c1_a1(x)^2 - 4 * c2(x) * c0_a1(x)
}

lower <- min(range(dat$y))
upper <- max(range(dat$y))

curve(c2, lower, upper)
curve(c1_a0, lower, upper)
curve(c1_a1, lower, upper)
curve(c0_a0, lower, upper)
curve(c0_a1, lower, upper)
curve(d_a0, lower, upper)
curve(d_a1, lower, upper)

get_a0 <- function(x){
  solve_quadratic(a = c2(x), b = c1_a0(x), c = c0_a0(x))[,1:2]
}

get_a1 <- function(x){
  solve_quadratic(a = c2(x), b = c1_a1(x), c = c0_a1(x))[,1:2]
}

tau <- seq(-1, 1, 0.01)
a0 <- apply(apply(get_a0(tau), 2, Re), 1, min)
a1 <- apply(apply(get_a1(tau), 2, Re), 1, min)

plot(tau, a0, type = 'l')
mean(a0)
plot(tau, a1, type = 'l')
mean(a1)

# Doesn't seem right... Make sure the simulation satisfies the assumptions
Fstar00 <- with(dat, ecdf(y[Tstar == 0 & z == 0]))
Fstar01 <- with(dat, ecdf(y[Tstar == 0 & z == 1]))
Fstar10 <- with(dat, ecdf(y[Tstar == 1 & z == 0]))
Fstar11 <- with(dat, ecdf(y[Tstar == 1 & z == 1]))

# The assumption is that Fstar00 = Fstar01 and Fstar10 = Fstar11
test0 <- function(x){
  Fstar01(x) - Fstar00(x)
}
test1 <- function(x){
  Fstar11(x) - Fstar10(x)
}
curve(test0, -2, 2)
curve(test1, -2, 2)

# Weird: the DGP doesn't seem to satisfy independence! Why not?
ystar00 <- subset(dat, Tstar == 0 & z == 0)$y
ystar01 <- subset(dat, Tstar == 0 & z == 1)$y

summary(ystar00)
summary(ystar01)


dat1 <- subset(dat, Tstar == 1)

# Check the other equations: these look ok
plot(tau, (Fstar00(tau) - F00(tau)) / (F00(tau) - F10(tau)) - 
       (a1_true * p0) / (1 - p0 - a1_true), type = 'l')

plot(tau, (Fstar01(tau) - F01(tau)) / (F01(tau) - F11(tau)) - 
       (a1_true * p1) / (1 - p1 - a1_true), type = 'l')

plot(tau, (Fstar10(tau) - F10(tau)) / (F10(tau) - F00(tau)) - 
       (a0_true * (1 - p0)) / (p0 - a0_true), type = 'l')


# What about the bounds based on the preceding?
# They seem to attain a minimum at the TRUTH which is hard to explain
a0_0 <- function(x){
  p0 * pmin(F10(x) / F0(x), (1 - F10(x)) / (1 - F0(x)))
}

a0_1 <- function(x){
  p1 * pmin(F11(x) / F1(x), (1 - F11(x)) / (1 - F1(x)))
}

a1_0 <- function(x){
  (1 - p0) * pmin(F00(x) / F0(x), (1 - F00(x)) / (1 - F0(x)))
}

a1_1 <- function(x){
  (1 - p1) * pmin(F01(x) / F1(x), (1 - F01(x)) / (1 - F1(x)))
}

curve(a1_1, -2, 2)
curve(a1_0, -2, 2)
curve(a0_1, -2, 2)
curve(a0_0, -2, 2)
