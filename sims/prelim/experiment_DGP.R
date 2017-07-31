# A selection model with random assignment
set.seed(208)
n <- 10000000
# Randomly assign offer of treatment
q <- 0.5
z <- rbinom(n, 1, q)
# Independently draw subject "ability"
s_ability <- 1
ability <- rnorm(n, 0, s_ability)
# Subjects select into treatment, xstar, based on ability and treatment offer
g0 <- -1  
g1 <- 5
xstar <- as.numeric(g0 + g1 * z + ability > 0)
# Draw outcome (log wages)
c <- 0.5
b <- 2
s_noise <- 1
e <- rnorm(n, 0, s_noise) + ability
y <- c + b * xstar + e
# Measurement Error
a0 <- 0.84
a1 <- 0.62
x <- (1 - xstar) * rbinom(n, 1, a0) + xstar * rbinom(n, 1, 1 - a1)

# Sanity Check
W <- cov(y,z)/cov(x,z)
W
b / (1 - a0 - a1)


dat <- data.frame(x, xstar, y, z, e, ability)
dat0 <- subset(dat, z == 0)
dat1 <- subset(dat, z == 1)
ybar <- mean(dat$y)
p_hat <- mean(dat$x)
p1_hat <- mean(dat1$x) #observed
p0_hat <- mean(dat0$x) #observed
p1 <- mean(dat1$xstar) #truth
p0 <- mean(dat0$xstar) #truth
Var_y1 <- var(dat1$y)
Var_x1 <- var(dat1$x)
Var_y0 <- var(dat0$y)
Var_x0 <- var(dat0$x)
W <- (mean(dat1$y) - mean(dat0$y)) / (p1_hat - p0_hat)
Wtilde <- (mean(dat1$y * dat1$x) - mean(dat0$y * dat0$x)) / (p1_hat - p0_hat)


# Calculate the m_tk
m00 <- mean(subset(dat0, xstar == 0)$e)
m10 <- mean(subset(dat0, xstar == 1)$e)
m01 <- mean(subset(dat1, xstar == 0)$e)
m11 <- mean(subset(dat1, xstar == 1)$e)

mu <- m11 * (p1_hat - a0) - m10 * (p0_hat - a0)
mu_tilde <- (p1_hat - a0) * (m11 + c) - (p0_hat - a0) * (m10 + c)
mu_tilde - mu 
# Should equal the following:
c * (p1_hat - p0_hat) #and it does!

# Calculate the v_tk
v00 <- mean(subset(dat0, xstar == 0)$e^2)
v10 <- mean(subset(dat0, xstar == 1)$e^2)
v01 <- mean(subset(dat1, xstar == 0)$e^2)
v11 <- mean(subset(dat1, xstar == 1)$e^2)

v <- p1 * v11 - p0 * v10
v_tilde <- p1 * (v11 + c^2) - p0 * (v10 + c^2)


# This should equal a0 - a1
(2 * p_hat - 1 - p1_hat - p0_hat) + (2 / W) * (Wtilde - ybar) -
  (Var_y1 - Var_y0) / ((p1_hat - p0_hat) * W^2)
(a0 - a1)
# and it does!

# Check the other equations!!!
# Wald
W
b /(1 - a0 - a1)

# Modified Wald
Wtilde
ybar + W * ((1 - p_hat) + (a0 - a1)) + mu / (p1_hat - p0_hat)

# Original Variance Equation (before solving with modified Wald)
W^2 * ( (Var_x1 - Var_x0) + (a0 - a1) * (p1_hat - p0_hat)) +
  2 * W * mu
Var_y1 - Var_y0



#---------------- Test out equation for (a0 - a1) from October 28th
R1 <- mean(dat1$y^2) - mean(dat0$y^2)
R2 <- mean(dat1$y * dat1$x) - mean(dat0$y * dat0$x)
#R2 should be Wtilde up to scale:
R2 / (p1_hat - p0_hat)
Wtilde


#Check the the Delta(yT) equation
(1 - a1) * W * (p1_hat - p0_hat) + mu_tilde
#Should equal R2
R2 #and it does!

#Check the Delta(y^2) equation
b * W * (p1_hat - p0_hat) + 2 * W * mu_tilde
#Should equal R1
R1 #And it does!

#Check the expression for the identified quantity R
R <- (R1 - 2 * W * R2) / (W * (p1_hat - p0_hat))
R #should equal the following
b - 2 * (1 - a1) * W
# and it does!

#Should be (a0 - a1)
-1 - R/W
(a0 - a1)
# and it is!


#Try checking the solution for b using third moments
dy2 <- R1
dyT <- R2
dy3 <- mean(dat1$y^3) - mean(dat0$y^3)
dy2T <- mean(dat1$y^2 * dat1$x) - mean(dat0$y^2 * dat0$x)

A <- W * (p1_hat - p0_hat)
B <- 3 * W * (dyT + 2 * R * (p1_hat - p0_hat))
C <- 3 * W * (dy2T + 2 * R * dyT + 2 * R^2 * (p1_hat - p0_hat)) - dy3

polyroot(c(C, B, A))

#Check the equations upon which the quadratic is based
# v00 <- mean(subset(dat0, xstar == 0)$u^2)
# v01 <- mean(subset(dat1, xstar == 0)$u^2)
# v10 <- mean(subset(dat0, xstar == 1)$u^2)
# v11 <- mean(subset(dat1, xstar == 1)$u^2)
# 
# with(foo$param, v00 * (1 - p0) + v01 * p0)
# with(foo$param, v10 * (1 - p1) + v11 * p1)
# with(foo$param, sigma)^2
# 
# v <- with(foo$param, p1 * v11 - p0 * v10)
# v_tilde <- with(foo$param, p1 * (v11 + c^2) - p0 * (v10 + c^2))
# 
# # Difference of expectation of y^3 equation
# foo$param$b^2 * W * (p1_hat - p0_hat) + 3 * foo$param$b * W * mu_tilde + 3 * W * v_tilde
# # Should equal dy3...
# dy3 # Close enough!
# 
# # Difference of E[y^2 T] equation
# foo$param$b * (1 - foo$param$a1) * W * (p1_hat - p0_hat) + 2 * (1 - foo$param$a1) * W * mu_tilde + v_tilde
# # Should equal dy2T...
# dy2T #Yes!

#------------Check Camilo's other quadratic for (1 - a1) - Halloween 2015
S <- (dy3 - 3 * W * (dy2T + R * dyT)) / (W * (p1_hat - p0_hat))
S
# Should be equal to the following
b^2 - 3 * W * (1 - a1) * (b + R)
# and it is!

A2 <- 2 * W^2
B2 <- 2 * R * W
C2 <- S - R^2

polyroot(c(C2, B2, A2))
#should equal (1 - a1)
1 - a1 #and it does!

#------------Check The other thing...
A3 <- 2 * W^2
B3 <- -6 * W * R
C3 <- S + 3 * R^2


polyroot(c(C3, B3, A3))


#-------------Check final stuff!
R^2 - 2 * (1 - a1) * W * (R + (1 - a1) * W) #should equal S...
S #Yes!

3 * R^2 - 2 * S # Should equal the following...
R^2 + 2 * (1 - a1) * W^2 * -a0
