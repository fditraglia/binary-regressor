# Same as molinari_special_case_calculation2.R except I have re-parameterized
# to eliminate the ratio beta / (1 - a1)

# Moment selection step plot
GMS_plot <- function(a1, n, p1_star = 0.85, q = 1/2){
  pi_bar <- q * p1_star
  a1_seq <- seq(0, 1, 0.001)
  Delta <- sqrt(n) * (pi_bar * (1 - a1) - q * (1 - a1_seq)) /  
    sqrt(pi_bar * (1 - a1) * (1 - (1 - a1) * pi_bar))
  plot(a1_seq, pnorm(-(sqrt(log(n)) + Delta)), type = 'l',
     xlab = 'Hypothesized a1', ylab = 'P(Drop Inequality)') 
  abline(v = a1, lty = 2, col = 'blue') 
  text(a1 - 0.05, 0.5, labels = 'Truth', col = 'blue') 
  abline(v = 1 - p1_star * (1 - a1), lty = 2, col = 'red') 
  text(1 - p1_star * (1 - a1) + 0.15, 0.5, labels = 'Upper Bound', col = 'red')
}

GMS_plot(0.2, 1000)
GMS_plot(0.1, 1000)
GMS_plot(0.0, 1000)


m11 <- function(delta = 0.15, rho = 0.5){
  a <- -qnorm(1 - delta)
  f <- function(x) x * dnorm(x) * (1 - pnorm((a - rho * x) / sqrt(1 - rho^2)))
  integrate(f, -Inf, Inf)$value / (1 - pnorm(a))
}

# Derivative D when we exclude the inequality
D <- function(a1_eval, beta_eval, a1_true, beta_true, 
              q = 0.5, delta = 0.15, rho = 0.5, sigma_epsilon = 1){
  c_bar <- m11(delta, rho)
  p1_star <- 1 - delta
  pi_bar <- q * p1_star
  
  mu <- pi_bar * c(1 - a1_true, beta_true)
  s11 <- (1 - a1_true) * pi_bar * (1 - (1 - a1_true) * pi_bar)
  s12 <- (1 - a1_true) * pi_bar * (c_bar + beta_true * (1 - pi_bar))
  s22 <- pi_bar * beta_true * ((1 - pi_bar) + 2 * c_bar) + q * sigma_epsilon^2
  M <- matrix(c(s11, s12,
                s12, s22), 2, 2, byrow = TRUE)
  
  nu <- c(-beta_eval, 1 - a1_eval)
  
  K1 <- drop(1 / sqrt(t(nu) %*% M %*% nu))
  K2 <- drop(t(nu) %*% mu / (t(nu) %*% M %*% nu))
  
  out <- -K1 * (mu[1] - K2 * drop(t(nu) %*% M[, 1, drop = FALSE])) 
  return(out)
}

a1_true <- 0.1
beta_true <- 0.1
Wald <- beta_true / (1 - a1_true)
D1 <- function(a1_eval, beta_eval) {
  D(a1_eval, beta_eval, a1_true = a1_true, beta_true = beta_true)
}

r <- 2.8
1.96 - 2.8 * abs(D1(0, Wald))
1.96 - 2.8 * abs(D1(0, Wald + 0.1))
1.96 - 2.8 * abs(D1(0, Wald + 0.01))
1.96 - 2.8 * abs(D1(0, Wald + 0.001))

