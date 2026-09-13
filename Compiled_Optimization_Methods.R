################################################################################
##
##  COMPILED NOTES: NUMERICAL OPTIMIZATION & SIMULATION METHODS IN R
##
##  This file compiles six separate scripts into one organized reference,
##  covering (in order):
##    1. Golden Section Search          - derivative-free 1D minimization
##    2. Fibonacci Search               - derivative-free 1D minimization
##    3. Newton-Raphson Method          - Gamma MLE & location-Cauchy MLE
##    4. Gradient Ascent                - location-Cauchy MLE & logistic regression
##    5. Simulated Annealing            - global optimization via random search
##    6. Stochastic / Mini-batch Gradient Ascent - large-scale logistic regression
##
##  Each section below retains the original code logic, with added headings
##  and explanatory comments describing what the code does and why.
##
################################################################################


################################################################################
## SECTION 1: GOLDEN SECTION SEARCH
################################################################################
## Golden Section Search is a derivative-free method for finding the minimum
## of a unimodal function on an interval [a, b]. It works by repeatedly
## shrinking the bracketing interval using the golden ratio (tau ~ 0.618),
## which guarantees one of the two interior points can be reused at each
## iteration -- so only one new function evaluation is needed per step.
################################################################################

# Objective function: a quartic with minima at x = 2 and x = 8
f <- function(x) {
  (x - 2)^2 * (x - 8)^2
}

# Golden Section Search
golden_search <- function(f, a, b, tol = 1e-4) {

  tau <- (sqrt(5) - 1) / 2  # golden ratio constant, ~0.618

  # Keep a record of the shrinking interval at each iteration (for plotting)
  history <- data.frame(
    iter = 0,
    a = a,
    b = b
  )

  # Two interior points that split [a,b] according to the golden ratio
  x1 <- b - tau * (b - a)
  x2 <- a + tau * (b - a)

  f1 <- f(x1)
  f2 <- f(x2)

  iter <- 0
  while ((b - a) > tol) {
    iter <- iter + 1

    if (f1 > f2) {
      # Minimum is not in [a, x1] -> discard left portion
      a <- x1
      x1 <- x2
      f1 <- f2

      x2 <- a + tau * (b - a)
      f2 <- f(x2)

    } else {
      # Minimum is not in [x2, b] -> discard right portion
      b <- x2
      x2 <- x1
      f2 <- f1

      x1 <- b - tau * (b - a)
      f1 <- f(x1)
    }
    history <- rbind(
      history,
      data.frame(iter = iter, a = a, b = b)
    )
  }

  list(
    xmin = (a + b) / 2,        # midpoint of final (tiny) interval
    interval = c(a, b),
    history = history
  )
}

# Run Golden Section Search on f, searching within [1, 5]
result <- golden_search(f, a = 1, b = 5, tol = 1e-4)

print(result)

h <- result$history
h

# Visualize how the bracketing interval [a, b] shrinks toward the minimum
u <- seq(0, 6, length = 500)
plot(u, f(u), type = 'l', main = "Golden Section Search",
     xlab = "x", ylab = "f(x)")

for (i in 1:nrow(h)) {
  abline(v = h[i, 2], col = 2)  # left endpoint at each iteration
  abline(v = h[i, 3], col = 3)  # right endpoint at each iteration
  # Sys.sleep(0.5)  # uncomment to animate the shrinking interval
}


################################################################################
## SECTION 2: FIBONACCI SEARCH
################################################################################
## Fibonacci Search is closely related to Golden Section Search but uses a
## precomputed sequence of Fibonacci numbers to determine the interior points
## at each step, guaranteeing the interval shrinks by a known ratio in a
## FIXED number of iterations (determined up front from the desired tolerance).
################################################################################

# Objective function: same quartic shape, written slightly differently
f <- function(x) {
  ((x - 2) * (x - 8))^2
}

# Fibonacci search function
fib_search <- function(f, a, b, tol = 1e-4) {

  # Generate enough Fibonacci numbers so that the final interval width
  # will be smaller than the desired tolerance
  fib <- c(1, 1)
  while (fib[length(fib)] < (b - a) / tol) {
    fib <- c(fib, fib[length(fib)] + fib[length(fib) - 1])
  }

  history <- data.frame(
    a = a,
    b = b
  )

  N <- length(fib)

  # Initial interior points, placed using ratios of Fibonacci numbers
  x1 <- a + fib[N - 2] / fib[N] * (b - a)
  x2 <- a + fib[N - 1] / fib[N] * (b - a)

  f1 <- f(x1)
  f2 <- f(x2)

  # The number of iterations is fixed in advance (N - 2), unlike Golden Section
  for (k in 1:(N - 2)) {

    if (f1 > f2) {
      # Minimum is not in [a, x1] -> discard left portion
      a <- x1
      x1 <- x2
      f1 <- f2

      x2 <- a + fib[N - k - 1] / fib[N - k] * (b - a)
      f2 <- f(x2)

    } else {
      # Minimum is not in [x2, b] -> discard right portion
      b <- x2
      x2 <- x1
      f2 <- f1

      x1 <- a + fib[N - k - 2] / fib[N - k] * (b - a)
      f1 <- f(x1)
    }

    history <- rbind(
      history,
      data.frame(a = a, b = b)
    )
  }

  list(
    xmin = (a + b) / 2,
    fmin = f((a + b) / 2),
    interval = c(a, b),
    history = history
  )
}

# Run Fibonacci search on the same interval as before
result <- fib_search(f, a = 1, b = 5, tol = 1e-4)

print(result)

h <- result$history

# Visualize the shrinking bracket, same style as Golden Section Search
u <- seq(0, 6, length = 500)
plot(u, f(u), type = 'l', main = "Fibonacci Search",
     xlab = "x", ylab = "f(x)")
for (i in 1:nrow(h)) {
  abline(v = h[i, 1], col = 2)
  abline(v = h[i, 2], col = 3)
}


################################################################################
## SECTION 3: NEWTON-RAPHSON METHOD
################################################################################
## Newton-Raphson finds a root of the score equation (derivative of the
## log-likelihood set to zero) by repeatedly using a local quadratic
## approximation of the objective function. It typically converges very
## fast (quadratically) near the optimum, but can behave poorly or even
## diverge for bad starting values or non-concave likelihoods.
##
## Two examples are covered here:
##   3a. MLE of the Gamma(alpha, 1) shape parameter
##   3b. MLE of the location parameter of a Cauchy distribution
################################################################################

## ----------------------------------------------------------------------
## 3a. MLE for Gamma(alpha, 1) via Newton-Raphson
## ----------------------------------------------------------------------
set.seed(100)
library(pracma)  # provides the psi() (digamma/trigamma) function needed below

## --- Small sample size first ---
## With a small sample, the Newton-Raphson estimate (blue line) may not be
## very close to the true alpha (red line), because the MLE itself is not
## yet close to the truth (small-sample variability, not an algorithm flaw).

alpha <- 5  # true value of alpha
n <- 10     # small sample size
dat <- rgamma(n, shape = alpha, rate = 1)

alpha_newton <- numeric()
epsilon <- 1e-8      # tolerance for convergence
alpha_newton[1] <- 2 # starting value alpha_0
count <- 1
tol <- 100 # placeholder large number so the while loop starts

while (tol > epsilon) {
  count <- count + 1

  # First derivative of the log-likelihood w.r.t. alpha
  f.prime <- -n * psi(k = 0, alpha_newton[count - 1]) + sum(log(dat))

  # Second derivative (for the Newton-Raphson step)
  f.dprime <- -n * psi(k = 1, alpha_newton[count - 1])

  alpha_newton[count] <- alpha_newton[count - 1] - f.prime / f.dprime
  tol <- abs(alpha_newton[count] - alpha_newton[count - 1])
}
alpha_newton  # sequence of Newton-Raphson iterates

# Plot the log-likelihood surface and overlay the Newton-Raphson path
alpha.grid <- seq(0, 10, length = 100)
log.like <- numeric(length = 100)
for (i in 1:100) {
  log.like[i] <- sum(dgamma(dat, shape = alpha.grid[i], log = TRUE))
}
plot(alpha.grid, log.like, type = 'l', xlab = expression(alpha),
     ylab = "Log Likelihood", main = "Gamma MLE (small n)")
abline(v = alpha, col = "red", lty = 2)  # true alpha
for (t in 1:count) {
  points(alpha_newton[t], sum(dgamma(dat, shape = alpha_newton[t], log = TRUE)), pch = 16)
}
abline(v = tail(alpha_newton[count]), col = "blue", lty = 2)  # NR estimate
legend("bottomright", legend = c("Log Likelihood", "Truth", "MLE"),
       lty = c(1, 2, 2), col = c("black", "red", "blue"))


## --- Larger sample size ---
## Now the MLE is much closer to the truth, and the red/blue lines
## essentially coincide -- illustrating consistency of the MLE.

alpha <- 5   # true value of alpha
n <- 1000    # larger sample size
dat <- rgamma(n, shape = alpha, rate = 1)

alpha_newton <- numeric()
epsilon <- 1e-8
alpha_newton[1] <- 2
count <- 1
tol <- 100
while (tol > epsilon) {
  count <- count + 1
  f.prime <- -n * psi(k = 0, alpha_newton[count - 1]) + sum(log(dat))
  f.dprime <- -n * psi(k = 1, alpha_newton[count - 1])
  alpha_newton[count] <- alpha_newton[count - 1] - f.prime / f.dprime
  tol <- abs(alpha_newton[count] - alpha_newton[count - 1])
}
alpha_newton

alpha.grid <- seq(0, 10, length = 100)
log.like <- numeric(length = 100)
for (i in 1:100) {
  log.like[i] <- sum(dgamma(dat, shape = alpha.grid[i], log = TRUE))
}
plot(alpha.grid, log.like, type = 'l', xlab = expression(alpha),
     ylab = "Log Likelihood", main = "Gamma MLE (large n)")
abline(v = alpha, col = "red", lty = 2)
for (t in 1:count) {
  points(alpha_newton[t], sum(dgamma(dat, shape = alpha_newton[t], log = TRUE)), pch = 16)
}
abline(v = tail(alpha_newton[count]), col = "blue", lty = 2)
legend("bottomright", legend = c("Log Likelihood", "Truth", "MLE"),
       lty = c(1, 2, 2), col = c("black", "red", "blue"))


## ----------------------------------------------------------------------
## 3b. MLE for the location-Cauchy distribution via Newton-Raphson
## ----------------------------------------------------------------------
## The Cauchy log-likelihood is famously NOT concave, so Newton-Raphson can
## converge to the true MLE, oscillate, or diverge entirely depending on
## the starting value. The examples below illustrate "good", "bad", and
## "horrible" starting points, as well as the effect of sample size.

set.seed(1)
mu.star <- 5  # true location parameter
n <- 4        # small sample size
X <- rt(n, df = 1) + mu.star  # Cauchy = t-distribution with df = 1

# Log-likelihood function for the location-Cauchy model
log.like <- function(mu, X) {
  n <- length(X)
  rtn <- -n * log(pi) - sum(log(1 + (X - mu)^2))
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)
ll.est <- sapply(mu.x, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu),
     main = "Cauchy log-likelihood (non-concave)")

## Newton-Raphson setup
tol <- 1e-5  # convergence tolerance

# First derivative (score function) of the log-likelihood
f.prime <- function(X, mu) {
  rtn <- 2 * sum((X - mu) / (1 + (X - mu)^2))
  return(rtn)
}

# Second derivative of the log-likelihood
f.double.prime <- function(X, mu) {
  rtn <- 2 * sum(2 * (X - mu)^2 / (1 + (X - mu)^2)^2 - (1 + (X - mu)^2)^(-1))
  return(rtn)
}

## --- Good starting value: the sample median ---
current <- median(X)
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to the MLE
iter
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(0, 0, 1, alpha = .5))

## --- Bad starting value ---
current <- 7
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(1, 0, 0, alpha = .5))

## --- Worst starting value ---
current <- 19
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(.2, .7, .1, alpha = .8))

legend("topright", legend = c("Good starting", "Bad starting", "Horrible starting"),
       pch = 16, col = c("blue", "red", rgb(.2, .7, .1)))


## --- Repeating with a different seed (same small n = 4) ---
## Illustrates that with a very small sample, even the "good" starting
## value (median) can lead to a poor MLE estimate, since the MLE itself
## is unreliable at small sample sizes.
set.seed(10)

mu.star <- 5
n <- 4
X <- rt(n, df = 1) + mu.star

mu.x <- seq(-10, 40, length = 1e3)
ll.est <- sapply(mu.x, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))

tol <- 1e-5

current <- median(X)  # good starting value
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(0, 0, 1, alpha = .5))

current <- 2  # bad starting value
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(1, 0, 0, alpha = .5))

current <- 19  # bad starting value
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(.2, .7, .1, alpha = .8))

## The issue here: with n = 4, the sample median and the MLE are both
## unreliable estimators of mu.star.


## --- Repeating with the same seed but a much larger sample size ---
## Demonstrates that with n = 1e5, Newton-Raphson converges to the true
## value reliably from a good starting point, but can behave badly
## (large jumps) with a poorly chosen starting value, motivating the
## use of Modified Newton-Raphson (e.g., step-halving) in such cases.
set.seed(10)

mu.star <- 5
n <- 1e5
X <- rt(n, df = 1) + mu.star

mu.x <- seq(-10, 40, length = 1e3)
ll.est <- sapply(mu.x, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))

tol <- 1e-5

current <- mean(X)  # good starting value
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(mu.k, evals, pch = 16, col = rgb(0, 0, 1, alpha = .5))

current <- 3.5  # leads to a very large jump in the update!
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 100) {
  iter <- iter + 1
  update <- current - f.prime(X, current) / f.double.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
points(mu.k, evals, pch = 16, col = rgb(1, 0, 0, alpha = .5))

## Takeaway: large jumps like this motivate Modified Newton-Raphson methods.


################################################################################
## SECTION 4: GRADIENT ASCENT
################################################################################
## Gradient Ascent moves in the direction of the gradient (steepest ascent)
## scaled by a fixed step size t, rather than using second-derivative
## (curvature) information as Newton-Raphson does. It is more robust to bad
## starting values but generally converges more slowly, and the step size t
## strongly affects behavior (too large -> divergence/oscillation).
##
## Two examples are covered:
##   4a. MLE of the location-Cauchy parameter (with quadratic-approximation
##       visualization at each step)
##   4b. MLE for logistic regression via gradient ascent
################################################################################

## ----------------------------------------------------------------------
## 4a. Gradient Ascent for location-Cauchy MLE
## ----------------------------------------------------------------------
set.seed(1)
mu.star <- 5
n <- 4
X <- rt(n, df = 1) + mu.star

log.like <- function(mu, X) {
  n <- length(X)
  rtn <- -n * log(pi) - sum(log(1 + (X - mu)^2))
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)
ll.est <- sapply(mu.x, log.like, X)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu),
     main = "Gradient Ascent: Cauchy log-likelihood")

tol <- 1e-5  # convergence tolerance

f.prime <- function(X, mu) {
  rtn <- sum(2 * (X - mu) / (1 + (X - mu)^2))
  return(rtn)
}

f.double.prime <- function(X, mu) {
  rtn <- sum(2 * (2 * (X - mu)^2 / (1 + (X - mu)^2)^2 - (1 + (X - mu)^2)^(-1)))
  return(rtn)
}

# Step size t: try changing this to 1 to see the "bad" starting value diverge
t <- .3

plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))

## --- Bad starting value ---
## At each of the first few steps, overlay the local quadratic approximation
## implied by the gradient-ascent step size t, to visualize why the method
## moves the way it does.
current <- 7
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 1000) {
  iter <- iter + 1
  update <- current + t * f.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)

for (l in 1:min(5, iter)) {
  points(mu.k[l], evals[l], pch = 16, col = rgb(1, 0, 0, alpha = .5))
  foo <- mu.k[l]
  approx <- log.like(foo, X) + f.prime(X, foo) * (mu.x - foo) - (mu.x - foo)^2 / (2 * t)
  lines(mu.x, approx, col = rgb(1, 0, 0, alpha = .5))
  Sys.sleep(2)  # brief pause so the animation is visible
}
points(mu.k, evals, pch = 16, col = rgb(1, 0, 0, alpha = .5))


## --- Good starting value: sample median ---
current <- median(X)
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 1000) {
  iter <- iter + 1
  update <- current + t * f.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current

evals <- sapply(mu.k, log.like, X)
for (l in 1:min(5, iter)) {
  points(mu.k[l], evals[l], pch = 16, col = rgb(0, 0, 1, alpha = .5))
  foo <- mu.k[l]
  approx <- log.like(foo, X) + f.prime(X, foo) * (mu.x - foo) - (mu.x - foo)^2 / (2 * t)
  lines(mu.x, approx, col = rgb(0, 0, 1, alpha = .5))
  Sys.sleep(2)
}
points(mu.k, evals, pch = 16, col = rgb(0, 0, 1, alpha = .5))


## --- Worst starting value ---
current <- 19
diff <- 100
iter <- 0
mu.k <- current
while ((diff > tol) && iter < 1000) {
  iter <- iter + 1
  update <- current + t * f.prime(X, current)
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current
evals <- sapply(mu.k, log.like, X)
for (l in 1:iter) {
  points(mu.k[l], evals[l], pch = 16, col = rgb(.2, .7, .1, alpha = .5))
  foo <- mu.k[l]
  approx <- log.like(foo, X) + f.prime(X, foo) * (mu.x - foo) - (mu.x - foo)^2 / (2 * t)
  lines(mu.x, approx, col = rgb(.2, .7, .1, alpha = .5))
  Sys.sleep(2)
}
points(mu.k, evals, pch = 16, col = rgb(.2, .7, .1, alpha = .5))
legend("topright", legend = c("Good starting", "Bad starting", "Horrible starting"),
       pch = 16, col = c("blue", "red", rgb(.2, .7, .1)))


## ----------------------------------------------------------------------
## 4b. Gradient Ascent for logistic regression MLE
## ----------------------------------------------------------------------
## Here gradient ascent is used to maximize the logistic regression
## log-likelihood over the coefficient vector beta, using the full
## dataset's gradient at every step (i.e., "batch" gradient ascent).
library(mcmc)  # provides the example "logit" dataset
data(logit)
head(logit)  # y is the binary response; there are 4 covariates

y <- logit$y
X <- as.matrix(logit[, 2:5])
p <- dim(X)[2]

# Gradient of the logistic log-likelihood with respect to beta
f.gradient <- function(y, X, beta) {
  beta <- matrix(beta, ncol = 1)
  pi <- exp(X %*% beta) / (1 + exp(X %*% beta))
  rtn <- colSums(X * as.numeric(y - pi))  # equivalent to t(X) %*% (y - pi)
  return(rtn)
}

store.beta <- matrix(0, nrow = 1, ncol = p)
store.grads <- NULL
beta_k <- rep(0, p)  # start all coefficients at 0
grads <- 100         # placeholder large value for the gradient norm
t <- .1              # step size
tol <- 1e-8
iter <- 0

while ((grads > tol) && iter < 1e4) {  # cap iterations for safety
  iter <- iter + 1
  foo <- f.gradient(y = y, X = X, beta = beta_k)
  grads <- sqrt(sum(foo^2))       # Euclidean norm of the gradient
  store.grads <- c(store.grads, grads)  # track convergence
  beta_k <- beta_k + t * foo
  store.beta <- rbind(store.beta, beta_k)
}
iter    # number of iterations to convergence
beta_k  # final coefficient estimates

plot(store.grads, type = "b", pch = 16, ylab = "Norm of gradient",
     main = "Gradient Ascent convergence (logistic regression)")
abline(h = 0, col = "red")


################################################################################
## SECTION 5: SIMULATED ANNEALING
################################################################################
## Simulated Annealing is a stochastic global-optimization method inspired by
## the physical process of annealing metals. It explores the search space via
## a random-walk-like proposal, accepting "uphill" moves probabilistically
## based on a "temperature" T. As iterations proceed, T is decreased
## (cooling schedule), making the algorithm progressively more greedy so it
## settles near a global maximum -- helping it avoid getting stuck in local
## optima the way gradient-based methods can.
##
## Two examples are covered:
##   5a. A toy multimodal function
##   5b. MLE of the location-Cauchy parameter
################################################################################

## ----------------------------------------------------------------------
## 5a. Simulated Annealing on a toy multimodal function
## ----------------------------------------------------------------------
set.seed(1)

# fn represents exp(h(x)/T), i.e., a "Boltzmann-like" transform of the
# objective h(x) at temperature T. Lower T makes peaks much sharper/taller.
fn <- function(x, T = 1) {
  h <- (cos(50 * x) + sin(20 * x))^2
  exp(h / T) * (0 < x & x < 1)
}

x <- seq(0, 1, length = 5e2)
plot(x, fn(x), type = 'l', ylim = c(0, 150), ylab = "exp(f/T)",
     main = "Effect of temperature T on the target shape")

tseq <- c(.83, .75, .71)
for (t in 1:3) {
  lines(x, fn(x, T = tseq[t]), col = t + 1)
}
legend("topright", col = 1:4, lty = 1,
       legend = c("T = 1", "T = .83", "T = .75", "T = .71"))

# Simulated annealing sampler: at each step, propose a new point near the
# current one, and accept it with probability = ratio of fn values (which
# plays the role of a Metropolis-Hastings acceptance ratio), with a
# temperature T that cools as k (the iteration number) increases.
simAn <- function(N = 10, r = .3) {
  x <- numeric(length = N)
  x[1] <- runif(1)

  for (k in 2:N) {
    a <- runif(1, x[k - 1] - r, x[k - 1] + r)  # local random proposal
    T <- 1 / (log(k))  # cooling schedule: T -> 0 as k increases

    ratio <- fn(a, T) / fn(x[k - 1], T)  # fn is exp(h/T)
    if (runif(1) < ratio) {
      x[k] <- a          # accept the proposal
    } else {
      x[k] <- x[k - 1]   # reject; stay at current point
    }
  }
  return(x)
}

N <- 500
sim <- simAn(N = N)
sim[which.max(fn(sim))]  # estimate of theta^* (the global maximizer)

plot(x, fn(x), type = 'l', ylab = "exp(f/T)", main = "Simulated Annealing path")
points(sim, fn(sim), pch = 16, col = 1)


## ----------------------------------------------------------------------
## 5b. Simulated Annealing for location-Cauchy MLE
## ----------------------------------------------------------------------
## Because the Cauchy log-likelihood is non-concave (as seen in Sections 3
## and 4), Simulated Annealing offers an alternative way to locate the
## global maximum without relying on derivatives or a good starting value.
set.seed(1)
mu.star <- 5
n <- 4
X <- rt(n, df = 1) + mu.star

# Returns exp(log-likelihood / T)
log.like <- function(mu, X, T = 1) {
  n <- length(X)
  rtn <- -n * log(pi) - sum(log(1 + (X - mu)^2))
  return(exp(rtn / T))
}

mu.x <- seq(-10, 40, length = 1e3)
ll.est <- log(sapply(mu.x, log.like, X))  # back to log-scale for plotting
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu),
     main = "Cauchy log-likelihood (non-concave)")

# Simulated annealing algorithm for maximizing the Cauchy log-likelihood
simAn <- function(N = 10, r = .5) {
  x <- numeric(length = N)
  x[1] <- runif(1, min = -10, max = 40)
  fn.value <- numeric(length = N)

  fn.value[1] <- log.like(mu = x[1], X, T = 1)
  for (k in 2:N) {
    a <- runif(1, x[k - 1] - r, x[k - 1] + r)
    T <- 1 / (1 + log(log(k)))  # cooling schedule
    ratio <- log.like(mu = a, X, T) / log.like(mu = x[k - 1], X, T)
    if (runif(1) < ratio) {
      x[k] <- a
    } else {
      x[k] <- x[k - 1]
    }
    fn.value[k] <- log.like(mu = x[k], X, T = 1)
  }
  return(list("x" = x, "fn.value" = fn.value))
}

# A single run: extract the MLE as the visited point with highest likelihood
sim <- simAn(N = 1e2, r = 5)
sim$x[which.max(sim$fn.value)]  # MLE of mu

## Four independent runs, all converging near the same region
par(mfrow = c(2, 2))

sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("blue", alpha = .4))

sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("darkred", alpha = .4))

sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("darkgreen", alpha = .4))

sim <- simAn(N = 1e2, r = 5)
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("purple", alpha = .4))

## --- Effect of the proposal-width parameter r ---
## Very large r -> most proposals are far away and get rejected (inefficient
## exploration). Very small r -> proposals move only a little each time, so
## the chain accepts often but explores slowly (many small acceptances).
sim <- simAn(N = 1e3, r = 500)
sim$x[which.max(sim$fn.value)]  # MLE of mu

par(mfrow = c(1, 2))
plot(mu.x, ll.est, type = 'l', main = "r = 500. Many rejections",
     ylab = "log-likelihood", xlab = expression(mu))
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("blue", alpha = .2))

plot(mu.x, ll.est, type = 'l', main = "r = .1. Many small acceptances",
     ylab = "log-likelihood", xlab = expression(mu))
sim <- simAn(N = 1e3, r = .1)
points(sim$x, log(sim$fn.value), pch = 16, col = adjustcolor("blue", alpha = .2))


################################################################################
## SECTION 6: STOCHASTIC & MINI-BATCH GRADIENT ASCENT
################################################################################
## When the dataset is large, computing the exact gradient over ALL
## observations at every step (full-batch Gradient Ascent, as in Section 4b)
## can be expensive. Stochastic Gradient Ascent (SGA) approximates the
## gradient using only a small random subset ("mini-batch") of the data at
## each step. This trades a noisier gradient estimate for much cheaper
## iterations, which is often a good trade-off for large n.
##
## The function below is a single, general implementation that covers:
##   - Full-batch Gradient Ascent   (batch.size = n)
##   - Pure Stochastic Gradient Ascent (batch.size = 1)
##   - Mini-batch Stochastic Gradient Ascent (1 < batch.size < n)
################################################################################

## Gradient of the logistic log-likelihood (same form as Section 4b)
f.gradient <- function(y, X, beta) {
  n <- dim(X)[1]
  beta <- matrix(beta, ncol = 1)
  pi <- exp(X %*% beta) / (1 + exp(X %*% beta))
  rtn <- colSums(X * as.numeric(y - pi))
  return(n * rtn)
}

#################################################
## General function implementing regular Gradient Ascent, Stochastic
## Gradient Ascent, and Mini-batch Stochastic Gradient Ascent, depending
## on the chosen batch.size.
#################################################
SGA <- function(y, X, batch.size = dim(X)[1], t = .1, max.iter = dim(X)[1], adapt = FALSE) {
  p <- dim(X)[2]
  n <- dim(X)[1]

  # Randomly partition the data into K mini-batches of size batch.size
  permutation <- sample(1:n, replace = FALSE)
  K <- floor(n / batch.size)
  batch.index <- split(permutation, rep(1:K, each = n / K))

  # Index used to cycle through the mini-batches
  count <- 1
  beta_k <- rep(0, p)  # start at all zeros
  track.gradient <- matrix(0, nrow = max.iter, ncol = p)
  track.gradient[1, ] <- f.gradient(y = y, X = X, beta = beta_k)

  # Running mean of the beta estimates (useful for SGA/mini-batch, since
  # individual iterates are noisy -- the average is a better estimate of
  # the true beta^*)
  mean_beta <- rep(0, p)

  # tk allows for a decaying step size t_k, if adapt = TRUE
  tk <- t

  # Note: for demonstration purposes this always runs exactly max.iter
  # steps, rather than using a convergence-based stopping rule.
  for (iter in 1:max.iter) {
    count <- count + 1
    if (adapt) tk <- t / (sqrt(iter))  # decaying step size, if requested
    if (count %% K == 0) count <- count %% K + 1  # restart batch cycle when exhausted
    if (iter %% (max.iter / 10) == 0) cat(iter, " ")  # progress feedback

    # Select the current mini-batch of data
    y.batch <- y[batch.index[[count]]]
    X.batch <- matrix(X[batch.index[[count]], ], nrow = batch.size)

    # SGA update step, using only the current mini-batch's gradient
    beta_k <- beta_k + tk * f.gradient(y = y.batch, X = X.batch, beta = beta_k) / batch.size

    # Update the running mean of beta and track the FULL-DATA gradient
    # (computed only for diagnostic/plotting purposes, not used in the
    # update itself) to monitor overall convergence
    mean_beta <- (beta_k + mean_beta * (iter - 1)) / (iter)
    if (batch.size == n) {
      est <- beta_k     # full-batch GA: use beta_k directly
    } else {
      est <- mean_beta  # SGA / mini-batch: use the running average
    }
    track.gradient[iter, ] <- f.gradient(y = y, X = X, beta = est) / n
  }
  rtn <- list("iter" = iter, "est" = est, "grad" = track.gradient[1:iter, ])
  return(rtn)
}


## --- Demonstration: comparing GA, SGA, and mini-batch SGA ---

# Generate synthetic logistic regression data
set.seed(10)
p <- 5
n <- 1e4
X <- matrix(rnorm(n * (p - 1)), nrow = n, ncol = p - 1)
X <- cbind(1, X)
beta <- matrix(rnorm(p, 0, sd = 1), ncol = 1)
p_prob <- exp(X %*% beta) / (1 + exp(X %*% beta))
y <- rbinom(n, size = 1, prob = p_prob)

# Run: full-batch GA, then SGA and mini-batch SGA with matching iteration count
ga <- SGA(y, X, batch.size = 1e4, t = .0015, max.iter = 1e3)
b1 <- SGA(y, X, batch.size = 1, t = .1, max.iter = ga$iter)
b10 <- SGA(y, X, batch.size = 10, t = .1, max.iter = ga$iter)
b100 <- SGA(y, X, batch.size = 100, t = .1, max.iter = ga$iter)

# Compare convergence: sum of |full-data gradient| over iterations
index <- 1:500
plot(apply(ga$grad[index, ], 1, function(t) sum(abs(t))), type = 'l',
     ylim = c(0, max(apply(b1$grad[, ], 1, function(t) sum(abs(t))))),
     ylab = "Complete gradient", main = "GA vs SGA vs Mini-batch SGA (good step sizes)")
lines(apply(b1$grad[index, ], 1, function(t) sum(abs(t))), col = "red")
lines(apply(b10$grad[index, ], 1, function(t) sum(abs(t))), col = "blue")
lines(apply(b100$grad[index, ], 1, function(t) sum(abs(t))), col = "orange")
legend("topright", col = c("black", "red", "blue", "orange"), lty = 1,
       legend = c("GA", "SGA", "MB-SGA-10", "MB-SGA-100"))


## --- Effect of a poorly-chosen step size ---
## Repeating the comparison with step sizes t that are too large: all
## variants now oscillate locally instead of converging smoothly.
ga <- SGA(y, X, batch.size = n, t = .005, max.iter = 1e3)

b1 <- SGA(y, X, batch.size = 1, t = 1, max.iter = ga$iter)
b10 <- SGA(y, X, batch.size = 10, t = 1, max.iter = ga$iter)
b100 <- SGA(y, X, batch.size = 100, t = 1, max.iter = ga$iter)

index <- 1:1000
plot(apply(ga$grad[index, ], 1, function(t) sum(abs(t))), type = 'l',
     ylim = c(0, max(apply(b1$grad[, ], 1, function(t) sum(abs(t))))),
     ylab = "Complete gradient", main = "GA vs SGA vs Mini-batch SGA (step size too large)")
lines(apply(b1$grad[index, ], 1, function(t) sum(abs(t))), col = "red")
lines(apply(b10$grad[index, ], 1, function(t) sum(abs(t))), col = "blue")
lines(apply(b100$grad[index, ], 1, function(t) sum(abs(t))), col = "orange")
legend("topright", col = c("black", "red", "blue", "orange"), lty = 1,
       legend = c("GA", "SGA", "MB-SGA-10", "MB-SGA-100"))

################################################################################
## END OF FILE
################################################################################
