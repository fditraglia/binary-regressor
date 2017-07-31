library(mbereg)
m01 <- function(delta = 0.15, rho = 0.5){
  a <- -qnorm(1 - delta)
  f <- function(x) x * dnorm(x) * pnorm((a - rho * x) / sqrt(1 - rho^2))
  integrate(f, -Inf, Inf)$value / pnorm(a)
}

m11 <- function(delta = 0.15, rho = 0.5){
  a <- -qnorm(1 - delta)
  f <- function(x) x * dnorm(x) * (1 - pnorm((a - rho * x) / sqrt(1 - rho^2)))
  integrate(f, -Inf, Inf)$value / (1 - pnorm(a))
}

library(mbereg)
set.seed(82716)
a1 <- 0.2
beta <- 0.8
q <- 0.5
delta <- 0.15
p1_star <- 1 - delta

dat <- dgp(a0 = 0, a1 = a1, b = beta, n = 100000000, d = delta)
with(dat, mean(e[Tstar == 0 & z == 1]))
m01()
with(dat, mean(e[Tstar == 1 & z == 1]))
m11() # This checks out!

x1 <- with(dat, Tobs * z)
x2 <- with(dat, y * z)

# mu_1
mean(x1)
(1 - a1) * p1_star * q

# mu_2
mean(x2)
beta * (1 - delta) * q

# s_11
var(x1)
((1 - a1) * p1_star * q) * (1 - (1 - a1) * p1_star * q)

# s_22
beta^2 * p1_star * q * (1 - p1_star * q) + 
  2 * beta * p1_star * q * m11(delta = delta, rho = 0.5) + q * 1
var(x2)

# s_12
cov(x1, x2)
(1 - a1) * p1_star * q * (beta + m11()) - ((1 - a1) * p1_star * q) * (beta * p1_star * q)

# Double-check!
pi_bar <- p1_star * q
c_bar <- m11()

X <- cbind(x1, x2)
colMeans(X)
pi_bar * c(1 - a1, beta)

var(X)
(1 - a1) * pi_bar * (1 - (1 - a1) * pi_bar)
pi_bar * (1 - a1) * (c_bar + beta * (1 - pi_bar))
pi_bar * beta * (beta * (1 - pi_bar) + 2 * c_bar) + q

