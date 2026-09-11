
# =========================================================
# PROBLEM SET 2 — BIN SMOOTHER — FIXED ALL-IN-ONE SCRIPT
# =========================================================

# =========================================================
# Q1: Simulate the data
# =========================================================

set.seed(1234)

X <- runif(100, 5, 15)
Y <- 5 * sin(X) + 23 * cos(X)^2 + rnorm(100, 0, 5)

Sim.1 <- data.frame(X = X, Y = Y)

# True regression function
reg <- function(x) {
  5 * sin(x) + 23 * cos(x)^2
}


# =========================================================
# Q2 & Q3: Train/Test split
# =========================================================
# The first 80 observations are training data and the
# remaining 20 observations are test data.

X.train <- X[1:80]
Y.train <- Y[1:80]

X.test <- X[81:100]
Y.test <- Y[81:100]


# =========================================================
# Q4 & Q5: Plot data + true regression function
# =========================================================

plot(
  X.train, Y.train,
  pch = 1,
  xlab = "X",
  ylab = "Y",
  main = "Simulated data with true regression function"
)

points(
  X.test, Y.test,
  pch = 2,
  col = "red"
)

curve(
  reg(x),
  from = 5,
  to = 15,
  add = TRUE,
  col = "blue",
  lwd = 2
)

legend(
  "topright",
  legend = c("Train", "Test", "True function"),
  pch = c(1, 2, NA),
  lty = c(NA, NA, 1),
  col = c("black", "red", "blue")
)


# =========================================================
# Helper function: Bin smoother
# =========================================================
#
# x_tr  = training X values
# y_tr  = training Y values
# x_new = values at which predictions are required
# k     = number of bins
#
# The function:
# 1. Creates k equal-width bins.
# 2. Calculates the mean Y in each bin.
# 3. Handles empty bins.
# 4. Clips new X values to the training range.
# 5. Returns the corresponding bin means.
# =========================================================

binsmoother <- function(x_tr, y_tr, x_new, k) {
  
  # Basic checks
  if (length(x_tr) != length(y_tr)) {
    stop("x_tr and y_tr must have the same length.")
  }
  
  if (k < 1 || k != as.integer(k)) {
    stop("k must be a positive integer.")
  }
  
  # Construct k equal-width bins
  edges <- seq(
    min(x_tr),
    max(x_tr),
    length.out = k + 1
  )
  
  # Assign training observations to bins
  bins <- cut(
    x_tr,
    breaks = edges,
    labels = FALSE,
    include.lowest = TRUE
  )
  
  # Mean Y in each bin
  means <- tapply(
    y_tr,
    bins,
    mean,
    na.rm = TRUE
  )
  
  # Make sure all k bins exist in the result
  means_full <- rep(NA_real_, k)
  means_full[as.integer(names(means))] <- as.numeric(means)
  
  # Empty-bin fix:
  # use the overall training mean
  means_full[is.na(means_full)] <- mean(y_tr)
  
  # Clip new X values to the range used to construct the bins
  x_new <- pmin(
    pmax(x_new, edges[1]),
    edges[k + 1]
  )
  
  # Assign new observations to bins
  bins_new <- cut(
    x_new,
    breaks = edges,
    labels = FALSE,
    include.lowest = TRUE
  )
  
  # Return predictions
  as.numeric(means_full[bins_new])
}


# =========================================================
# Q6 & Q7:
# Fit bin smoother for k = 2, 5, 10, 20
# =========================================================

K_list <- c(2, 5, 10, 20)

for (k in K_list) {
  
  # Plot training and test data
  plot(
    X.train, Y.train,
    pch = 1,
    xlab = "X",
    ylab = "Y",
    main = paste("Bin smoother, k =", k)
  )
  
  points(
    X.test, Y.test,
    pch = 2,
    col = "red"
  )
  
  # True regression function
  curve(
    reg(x),
    from = 5,
    to = 15,
    add = TRUE,
    col = "blue",
    lwd = 2
  )
  
  # Smooth fitted curve
  xg <- seq(5, 15, length.out = 300)
  
  lines(
    xg,
    binsmoother(
      X.train,
      Y.train,
      xg,
      k
    ),
    col = "darkorange",
    lwd = 3
  )
  
  legend(
    "topright",
    legend = c(
      "Train",
      "Test",
      "True",
      "Bin smoother"
    ),
    pch = c(1, 2, NA, NA),
    lty = c(NA, NA, 1, 1),
    col = c(
      "black",
      "red",
      "blue",
      "darkorange"
    )
  )
  
  # Training MSE
  train_pred <- binsmoother(
    X.train,
    Y.train,
    X.train,
    k
  )
  
  tr_err <- mean(
    (Y.train - train_pred)^2
  )
  
  # Test MSE
  test_pred <- binsmoother(
    X.train,
    Y.train,
    X.test,
    k
  )
  
  te_err <- mean(
    (Y.test - test_pred)^2
  )
  
  cat(
    sprintf(
      "k = %2d | Train MSE = %8.3f | Test MSE = %8.3f\n",
      k,
      tr_err,
      te_err
    )
  )
}


# =========================================================
# Q8: Repeat 50 times with random splits
# =========================================================

K_list <- c(2, 5, 10, 20)

n_rep <- 50

avg_tr <- matrix(
  NA_real_,
  nrow = n_rep,
  ncol = length(K_list)
)

avg_te <- matrix(
  NA_real_,
  nrow = n_rep,
  ncol = length(K_list)
)

for (r in 1:n_rep) {
  
  # Reproducible seed for each repetition
  set.seed(r)
  
  # Generate a new sample
  Xr <- runif(100, 5, 15)
  
  Yr <- reg(Xr) +
    rnorm(100, 0, 5)
  
  # Random 80/20 split
  idx <- sample(
    1:100,
    size = 100,
    replace = FALSE
  )
  
  tr <- idx[1:80]
  te <- idx[81:100]
  
  for (j in seq_along(K_list)) {
    
    k <- K_list[j]
    
    # Training predictions
    train_pred <- binsmoother(
      Xr[tr],
      Yr[tr],
      Xr[tr],
      k
    )
    
    avg_tr[r, j] <- mean(
      (Yr[tr] - train_pred)^2
    )
    
    # Test predictions
    test_pred <- binsmoother(
      Xr[tr],
      Yr[tr],
      Xr[te],
      k
    )
    
    avg_te[r, j] <- mean(
      (Yr[te] - test_pred)^2
    )
  }
}


# Average over the 50 repetitions

Q8 <- data.frame(
  k = K_list,
  Avg_Train_MSE = colMeans(
    avg_tr,
    na.rm = TRUE
  ),
  Avg_Test_MSE = colMeans(
    avg_te,
    na.rm = TRUE
  )
)

cat(
  "\n----- Q8: Average over 50 random splits -----\n"
)

print(Q8)


# Plot average training/test error

plot(
  K_list,
  Q8$Avg_Train_MSE,
  type = "b",
  pch = 1,
  ylim = range(
    Q8$Avg_Train_MSE,
    Q8$Avg_Test_MSE
  ),
  xlab = "k",
  ylab = "Average MSE",
  main = "Bin smoother: average train vs test error (50 repeats)"
)

lines(
  K_list,
  Q8$Avg_Test_MSE,
  type = "b",
  pch = 2,
  col = "red"
)

legend(
  "topright",
  legend = c(
    "Avg Train",
    "Avg Test"
  ),
  pch = c(1, 2),
  col = c("black", "red")
)


# =========================================================
# Q9: Boston data — 23-fold cross-validation
# =========================================================

library(MASS)

data(Boston)

xb <- Boston$lstat
yb <- Boston$medv


# ---------------------------------------------------------
# Create 23 randomized folds
# ---------------------------------------------------------
#
# Boston contains 506 observations.
# 506 / 23 = 22 exactly.
#
# Therefore every fold contains exactly 22 observations.
# ---------------------------------------------------------

set.seed(1234)

fold_id <- sample(
  rep(
    1:23,
    each = 22
  )
)

# Verify that every observation was assigned exactly once
stopifnot(
  length(fold_id) == length(xb)
)

stopifnot(
  all(
    table(fold_id) == 22
  )
)


# ---------------------------------------------------------
# Candidate numbers of bins
# ---------------------------------------------------------

k_grid <- c(
  2, 3, 5, 7, 10,
  15, 20, 25, 30,
  40, 50
)

cv_err <- numeric(
  length(k_grid)
)


# ---------------------------------------------------------
# 23-fold cross-validation
# ---------------------------------------------------------

for (ki in seq_along(k_grid)) {
  
  k <- k_grid[ki]
  
  fold_mse <- numeric(23)
  
  for (i in 1:23) {
    
    # Training observations
    tr <- which(
      fold_id != i
    )
    
    # Validation observations
    te <- which(
      fold_id == i
    )
    
    # Fit bin smoother using training fold
    pred <- binsmoother(
      xb[tr],
      yb[tr],
      xb[te],
      k
    )
    
    # Validation MSE
    fold_mse[i] <- mean(
      (yb[te] - pred)^2
    )
  }
  
  # Average MSE over all 23 folds
  cv_err[ki] <- mean(
    fold_mse
  )
}


# ---------------------------------------------------------
# Display CV results
# ---------------------------------------------------------

Q9 <- data.frame(
  k = k_grid,
  CV_MSE = cv_err
)

best_k <- k_grid[
  which.min(cv_err)
]

cat(
  "\n----- Q9: Boston 23-fold CV -----\n"
)

print(Q9)

cat(
  "Best k =",
  best_k,
  "\n"
)


# ---------------------------------------------------------
# Plot CV error
# ---------------------------------------------------------

plot(
  k_grid,
  cv_err,
  type = "b",
  pch = 19,
  xlab = "Number of bins (k)",
  ylab = "23-fold CV MSE",
  main = "Boston: Bin smoother CV error"
)

abline(
  v = best_k,
  col = "red",
  lty = 2
)

legend(
  "topright",
  legend = paste(
    "Best k =",
    best_k
  ),
  lty = 2,
  col = "red"
)


# =========================================================
# End of Problem Set 2
# =========================================================





# ============================================================
# PROBLEM SET 3 : KNN SMOOTHER USING knnreg()
# ============================================================


# ------------------------------------------------------------
# 1. Generate the simulated data
# ------------------------------------------------------------

set.seed(1234)

X <- runif(100, 5, 15)

Y <- 5*sin(X) + 23*cos(X)^2 + rnorm(100, 0, 5)

Sim.1 <- data.frame(X = X, Y = Y)

reg <- function(x) {
  5*sin(x) + 23*cos(x)^2
}


# ------------------------------------------------------------
# Load caret package
# ------------------------------------------------------------

library(caret)


# ------------------------------------------------------------
# 2 & 3. Training and Test Data
# ------------------------------------------------------------

X.train <- X[1:80]
X.test  <- X[81:100]

Y.train <- Y[1:80]
Y.test  <- Y[81:100]


# Create training and test data frames

train.data <- data.frame(
  X = X.train,
  Y = Y.train
)

test.data <- data.frame(
  X = X.test,
  Y = Y.test
)


# ------------------------------------------------------------
# 4 & 5. Plot Data and Regression Function
# ------------------------------------------------------------

x.grid <- seq(5, 15, length.out = 200)

plot(X.train, Y.train,
     pch = 16,
     col = "blue",
     xlab = "X",
     ylab = "Y",
     main = "Training, Test Data and Regression Function")

points(X.test, Y.test,
       pch = 17,
       col = "red")

lines(x.grid, reg(x.grid),
      col = "black",
      lwd = 2)

legend("topright",
       legend = c("Training", "Test", "Regression"),
       pch = c(16, 17, NA),
       lty = c(NA, NA, 1),
       col = c("blue", "red", "black"))


# ------------------------------------------------------------
# 6. KNN Smoother using knnreg()
# ------------------------------------------------------------

k.values <- c(1, 2, 5, 10, 20, 40, 80)


# Plot KNN estimates for different k

par(mfrow = c(2, 4))

for (k in k.values) {
  
  model <- knnreg(
    Y ~ X,
    data = train.data,
    k = k
  )
  
  y.hat <- predict(
    model,
    data.frame(X = x.grid)
  )
  
  plot(X.train, Y.train,
       pch = 16,
       col = "grey",
       main = paste("KNN, k =", k),
       xlab = "X",
       ylab = "Y")
  
  lines(x.grid,
        y.hat,
        lwd = 2)
  
  lines(x.grid,
        reg(x.grid),
        lty = 2,
        lwd = 2)
}

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# 7. Training and Test Errors
# ------------------------------------------------------------

train.error <- numeric(length(k.values))

test.error <- numeric(length(k.values))


for (i in 1:length(k.values)) {
  
  k <- k.values[i]
  
  # Fit KNN model
  
  model <- knnreg(
    Y ~ X,
    data = train.data,
    k = k
  )
  
  
  # Training predictions
  
  train.pred <- predict(
    model,
    train.data
  )
  
  
  # Test predictions
  
  test.pred <- predict(
    model,
    test.data
  )
  
  
  # Training error
  
  train.error[i] <- mean(
    (Y.train - train.pred)^2
  )
  
  
  # Test error
  
  test.error[i] <- mean(
    (Y.test - test.pred)^2
  )
}


# Create results table

results <- data.frame(
  k = k.values,
  Training_Error = train.error,
  Test_Error = test.error
)

print(results)


# Plot training and test errors

plot(k.values,
     train.error,
     type = "b",
     pch = 16,
     xlab = "k",
     ylab = "MSE",
     main = "Training and Test Errors")

lines(k.values,
      test.error,
      type = "b",
      pch = 17)

legend("topright",
       legend = c("Training Error", "Test Error"),
       pch = c(16, 17),
       lty = 1)


# ------------------------------------------------------------
# 8. Repeat 50 times
# ------------------------------------------------------------

set.seed(1234)

train.error.50 <- matrix(
  0,
  nrow = 50,
  ncol = length(k.values)
)

test.error.50 <- matrix(
  0,
  nrow = 50,
  ncol = length(k.values)
)


for (r in 1:50) {
  
  # Randomly select 80 observations
  
  train.id <- sample(
    1:100,
    80
  )
  
  # Remaining 20 observations
  
  test.id <- setdiff(
    1:100,
    train.id
  )
  
  
  # Training data
  
  X.train <- X[train.id]
  Y.train <- Y[train.id]
  
  
  # Test data
  
  X.test <- X[test.id]
  Y.test <- Y[test.id]
  
  
  # Create data frames
  
  train.data <- data.frame(
    X = X.train,
    Y = Y.train
  )
  
  test.data <- data.frame(
    X = X.test,
    Y = Y.test
  )
  
  
  # Try each k
  
  for (i in 1:length(k.values)) {
    
    k <- k.values[i]
    
    
    # Fit KNN model
    
    model <- knnreg(
      Y ~ X,
      data = train.data,
      k = k
    )
    
    
    # Training predictions
    
    train.pred <- predict(
      model,
      train.data
    )
    
    
    # Test predictions
    
    test.pred <- predict(
      model,
      test.data
    )
    
    
    # Training error
    
    train.error.50[r, i] <-
      mean((Y.train - train.pred)^2)
    
    
    # Test error
    
    test.error.50[r, i] <-
      mean((Y.test - test.pred)^2)
  }
}


# ------------------------------------------------------------
# Average errors over 50 repetitions
# ------------------------------------------------------------

average.train.error <- colMeans(
  train.error.50
)

average.test.error <- colMeans(
  test.error.50
)


average.results <- data.frame(
  k = k.values,
  Average_Training_Error = average.train.error,
  Average_Test_Error = average.test.error
)

print(average.results)


# Plot average errors

plot(k.values,
     average.train.error,
     type = "b",
     pch = 16,
     xlab = "k",
     ylab = "Average MSE",
     main = "Average Error - 50 Repetitions")

lines(k.values,
      average.test.error,
      type = "b",
      pch = 17)

legend("topright",
       legend = c("Training Error", "Test Error"),
       pch = c(16, 17),
       lty = 1)


# ------------------------------------------------------------
# 9. Boston Dataset
# ------------------------------------------------------------

library(MASS)

data(Boston)

X <- Boston$lstat

Y <- Boston$medv


# ------------------------------------------------------------
# 22-Fold Cross Validation
# ------------------------------------------------------------

set.seed(1234)

folds <- sample(
  rep(1:22, length.out = length(Y))
)


# K values

k.values <- c(1, 2, 5, 10, 20, 40, 80)


# Store CV errors

cv.error <- matrix(
  0,
  nrow = 22,
  ncol = length(k.values)
)


# Cross-validation

for (fold in 1:22) {
  
  # Training observations
  
  train.id <- which(
    folds != fold
  )
  
  
  # Test observations
  
  test.id <- which(
    folds == fold
  )
  
  
  # Training data
  
  X.train <- X[train.id]
  Y.train <- Y[train.id]
  
  
  # Test data
  
  X.test <- X[test.id]
  Y.test <- Y[test.id]
  
  
  # Create data frames
  
  train.data <- data.frame(
    X = X.train,
    Y = Y.train
  )
  
  test.data <- data.frame(
    X = X.test,
    Y = Y.test
  )
  
  
  # Try each k
  
  for (i in 1:length(k.values)) {
    
    k <- k.values[i]
    
    
    # Fit KNN model
    
    model <- knnreg(
      Y ~ X,
      data = train.data,
      k = k
    )
    
    
    # Prediction
    
    prediction <- predict(
      model,
      test.data
    )
    
    
    # CV error
    
    cv.error[fold, i] <-
      mean((Y.test - prediction)^2)
  }
}


# ------------------------------------------------------------
# Average CV Error
# ------------------------------------------------------------

average.cv.error <- colMeans(
  cv.error
)


Boston.results <- data.frame(
  k = k.values,
  CV_Error = average.cv.error
)

print(Boston.results)


# ------------------------------------------------------------
# Best value of k
# ------------------------------------------------------------

best.k <- k.values[
  which.min(average.cv.error)
]

cat(
  "Best k =",
  best.k,
  "\n"
)


# ------------------------------------------------------------
# Plot CV errors
# ------------------------------------------------------------

plot(k.values,
     average.cv.error,
     type = "b",
     pch = 16,
     xlab = "k",
     ylab = "CV Error",
     main = "22-Fold Cross-Validation")







# ============================================================
# PROBLEM SET 4 : KERNEL SMOOTHER
# ============================================================


# ------------------------------------------------------------
# 1. Generate the simulated data
# ------------------------------------------------------------

set.seed(1234)

X <- runif(100, 5, 15)

Y <- 5*sin(X) + 23*cos(X)^2 + rnorm(100, 0, 5)

Sim.1 <- data.frame(X = X, Y = Y)

reg <- function(x) {
  5*sin(x) + 23*cos(x)^2
}


# ------------------------------------------------------------
# 2 & 3. Training and Test Data
# ------------------------------------------------------------

X.train <- X[1:80]
X.test  <- X[81:100]

Y.train <- Y[1:80]
Y.test  <- Y[81:100]


# ------------------------------------------------------------
# 4 & 5. Plot Data and Regression Function
# ------------------------------------------------------------

x.grid <- seq(5, 15, length.out = 200)

plot(X.train, Y.train,
     pch = 16,
     col = "blue",
     xlab = "X",
     ylab = "Y",
     main = "Training, Test Data and Regression Function")

points(X.test, Y.test,
       pch = 17,
       col = "red")

lines(x.grid, reg(x.grid),
      col = "black",
      lwd = 2)

legend("topright",
       legend = c("Training", "Test", "Regression"),
       pch = c(16, 17, NA),
       lty = c(NA, NA, 1),
       col = c("blue", "red", "black"))



# ============================================================
# PROBLEM SET 4 : KERNEL SMOOTHER
# Using built-in ksmooth()
# ============================================================


# ------------------------------------------------------------
# 1. Generate the simulated data
# ------------------------------------------------------------

set.seed(1234)

X <- runif(100, 5, 15)

Y <- 5*sin(X) + 23*cos(X)^2 + rnorm(100, 0, 5)

Sim.1 <- data.frame(
  X = X,
  Y = Y
)


# True regression function

reg <- function(x) {
  5*sin(x) + 23*cos(x)^2
}


# ------------------------------------------------------------
# 2 & 3. Training and Test Data
# ------------------------------------------------------------

X.train <- X[1:80]
X.test <- X[81:100]

Y.train <- Y[1:80]
Y.test <- Y[81:100]


# ------------------------------------------------------------
# 4 & 5. Plot Data and Regression Function
# ------------------------------------------------------------

x.grid <- seq(5, 15, length.out = 200)

plot(X.train, Y.train,
     pch = 16,
     col = "blue",
     xlab = "X",
     ylab = "Y",
     main = "Training, Test Data and Regression Function")

points(X.test, Y.test,
       pch = 17,
       col = "red")

lines(x.grid,
      reg(x.grid),
      lwd = 2)

legend("topright",
       legend = c("Training", "Test", "Regression"),
       pch = c(16, 17, NA),
       lty = c(NA, NA, 1),
       col = c("blue", "red", "black"))


# ------------------------------------------------------------
# 6. Gaussian Kernel Smoother using ksmooth()
# ------------------------------------------------------------

h.values <- c(0.1, 0.25, 0.5, 1, 2)


par(mfrow = c(2, 3))

for (h in h.values) {
  
  model <- ksmooth(
    X.train,
    Y.train,
    kernel = "normal",
    bandwidth = h,
    x.points = x.grid
  )
  
  
  plot(X.train, Y.train,
       pch = 16,
       col = "grey",
       main = paste("Gaussian Kernel, h =", h),
       xlab = "X",
       ylab = "Y")
  
  
  lines(model$x,
        model$y,
        lwd = 2)
  
  
  lines(x.grid,
        reg(x.grid),
        lty = 2,
        lwd = 2)
}

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# Interpretation of bandwidth
# ------------------------------------------------------------

# Small h:
# The curve follows the observations closely
# and may become wiggly.
#
# Large h:
# The curve becomes smoother.
#
# Very small h can overfit.
# Very large h can underfit.
#
# Therefore, bandwidth controls the amount of smoothing.


# ============================================================
# 7. TRAINING AND TEST ERRORS
# ============================================================


training.error <- numeric(
  length(h.values)
)

test.error <- numeric(
  length(h.values)
)


for (i in 1:length(h.values)) {
  
  h <- h.values[i]
  
  
  # Training prediction
  
  train.model <- ksmooth(
    X.train,
    Y.train,
    kernel = "normal",
    bandwidth = h,
    x.points = X.train
  )
  
  
  # Test prediction
  
  test.model <- ksmooth(
    X.train,
    Y.train,
    kernel = "normal",
    bandwidth = h,
    x.points = X.test
  )
  
  
  # Training MSE
  
  training.error[i] <- mean(
    (Y.train - train.model$y)^2
  )
  
  
  # Test MSE
  
  test.error[i] <- mean(
    (Y.test - test.model$y)^2
  )
}


# Results

results <- data.frame(
  h = h.values,
  Training_Error = training.error,
  Test_Error = test.error
)

print(results)


# Plot training and test errors

plot(h.values,
     training.error,
     type = "b",
     pch = 16,
     xlab = "Bandwidth h",
     ylab = "MSE",
     main = "Training and Test Errors")

lines(h.values,
      test.error,
      type = "b",
      pch = 17)

legend("topright",
       legend = c("Training Error", "Test Error"),
       pch = c(16, 17),
       lty = 1)


# ============================================================
# 8. Repeat 50 Times
# ============================================================

set.seed(1234)


train.error.50 <- matrix(
  0,
  nrow = 50,
  ncol = length(h.values)
)


test.error.50 <- matrix(
  0,
  nrow = 50,
  ncol = length(h.values)
)


for (r in 1:50) {
  
  
  # Randomly select 80 observations
  
  train.id <- sample(
    1:100,
    80
  )
  
  
  # Remaining 20 observations
  
  test.id <- setdiff(
    1:100,
    train.id
  )
  
  
  # Training data
  
  X.train <- X[train.id]
  Y.train <- Y[train.id]
  
  
  # Test data
  
  X.test <- X[test.id]
  Y.test <- Y[test.id]
  
  
  # Try each bandwidth
  
  for (i in 1:length(h.values)) {
    
    h <- h.values[i]
    
    
    # Training prediction
    
    train.model <- ksmooth(
      X.train,
      Y.train,
      kernel = "normal",
      bandwidth = h,
      x.points = X.train
    )
    
    
    # Test prediction
    
    test.model <- ksmooth(
      X.train,
      Y.train,
      kernel = "normal",
      bandwidth = h,
      x.points = X.test
    )
    
    
    # Training error
    
    train.error.50[r, i] <-
      mean((Y.train - train.model$y)^2)
    
    
    # Test error
    
    test.error.50[r, i] <-
      mean((Y.test - test.model$y)^2)
  }
}


# ------------------------------------------------------------
# Average errors over 50 repetitions
# ------------------------------------------------------------

average.train.error <- colMeans(
  train.error.50
)


average.test.error <- colMeans(
  test.error.50
)


average.results <- data.frame(
  h = h.values,
  Average_Training_Error = average.train.error,
  Average_Test_Error = average.test.error
)


print(average.results)


# Plot average errors

plot(h.values,
     average.train.error,
     type = "b",
     pch = 16,
     xlab = "Bandwidth h",
     ylab = "Average MSE",
     main = "Average Error - 50 Repetitions")

lines(h.values,
      average.test.error,
      type = "b",
      pch = 17)

legend("topright",
       legend = c("Training Error", "Test Error"),
       pch = c(16, 17),
       lty = 1)


# ============================================================
# 9. BOSTON DATASET
# ============================================================

library(MASS)

data(Boston)


# Predictor and response

X <- Boston$lstat

Y <- Boston$medv


# ------------------------------------------------------------
# 22-Fold Cross Validation
# ------------------------------------------------------------

set.seed(1234)


folds <- sample(
  rep(1:22, length.out = length(Y))
)


# Bandwidth values

h.values <- c(
  0.1,
  0.25,
  0.5,
  1,
  2
)


# Store CV errors

cv.error <- matrix(
  0,
  nrow = 22,
  ncol = length(h.values)
)


# ------------------------------------------------------------
# Cross-validation
# ------------------------------------------------------------

for (fold in 1:22) {
  
  
  # Training observations
  
  train.id <- which(
    folds != fold
  )
  
  
  # Test observations
  
  test.id <- which(
    folds == fold
  )
  
  
  # Training data
  
  X.train <- X[train.id]
  Y.train <- Y[train.id]
  
  
  # Test data
  
  X.test <- X[test.id]
  Y.test <- Y[test.id]
  
  
  # Try each bandwidth
  
  for (i in 1:length(h.values)) {
    
    h <- h.values[i]
    
    
    # Fit Gaussian kernel smoother
    
    model <- ksmooth(
      X.train,
      Y.train,
      kernel = "normal",
      bandwidth = h,
      x.points = X.test
    )
    
    
    # Calculate CV error
    
    cv.error[fold, i] <-
      mean((Y.test - model$y)^2)
  }
}


# ------------------------------------------------------------
# Average CV Error
# ------------------------------------------------------------

average.cv.error <- colMeans(
  cv.error
)


Boston.results <- data.frame(
  h = h.values,
  CV_Error = average.cv.error
)


print(Boston.results)


# ------------------------------------------------------------
# Best bandwidth
# ------------------------------------------------------------

best.h <- h.values[
  which.min(average.cv.error)
]


cat(
  "Best bandwidth h =",
  best.h,
  "\n"
)


# ------------------------------------------------------------
# Plot CV errors
# ------------------------------------------------------------

plot(h.values,
     average.cv.error,
     type = "b",
     pch = 16,
     xlab = "Bandwidth h",
     ylab = "CV Error",
     main = "22-Fold Cross-Validation - Gaussian Kernel")










# ============================================================
# PROBLEM SET 7 - SMOOTHING SPLINE
# ============================================================

# Load package
library(MASS)

# ------------------------------------------------------------
# 1. SIMULATE DATA
# ------------------------------------------------------------

# True regression function
reg <- function(x) {
  5 * sin(x) + 23 * (cos(x))^2
}

set.seed(1234)

X <- runif(500, 5, 15)
Y <- reg(X) + rnorm(500, 0, 5)


# ------------------------------------------------------------
# 2. TRAIN-TEST SPLIT
# ------------------------------------------------------------

set.seed(1234)

train.index <- sample(1:500, 400)

X.train <- X[train.index]
Y.train <- Y[train.index]

X.test <- X[-train.index]
Y.test <- Y[-train.index]


# ------------------------------------------------------------
# 3. SCATTER PLOT + TRUE REGRESSION FUNCTION
# ------------------------------------------------------------

plot(X, Y,
     main = "Data and True Regression Function",
     xlab = "X",
     ylab = "Y",
     pch = 16)

x.grid <- seq(5, 15, length.out = 500)

lines(x.grid, reg(x.grid), lwd = 2)


# ------------------------------------------------------------
# 4. B-SPLINE FOR DIFFERENT DEGREES OF FREEDOM
# ------------------------------------------------------------

df.values <- c(4, 5, 10, 20, 30, 40, 50)

# Store models
spline.models <- list()

# Fit spline for each df
for (df in df.values) {
  
  model <- smooth.spline(X.train, Y.train, df = df)
  
  spline.models[[as.character(df)]] <- model
  
  # Plot
  plot(X, Y,
       main = paste("Smoothing Spline, df =", df),
       xlab = "X",
       ylab = "Y",
       pch = 16)
  
  lines(x.grid, reg(x.grid), lwd = 2)
  
  lines(x.grid,
        predict(model, x.grid)$y,
        lwd = 2,
        lty = 2)
}


# ------------------------------------------------------------
# 5. TRAINING AND TESTING ERRORS
# ------------------------------------------------------------

results <- data.frame(
  df = df.values,
  Training_Error = NA,
  Test_Error = NA
)

for (i in 1:length(df.values)) {
  
  df <- df.values[i]
  
  model <- spline.models[[as.character(df)]]
  
  train.pred <- predict(model, X.train)$y
  test.pred <- predict(model, X.test)$y
  
  results$Training_Error[i] <-
    mean((Y.train - train.pred)^2)
  
  results$Test_Error[i] <-
    mean((Y.test - test.pred)^2)
}

print(results)


# Plot training error
plot(results$df,
     results$Training_Error,
     type = "b",
     pch = 16,
     main = "Training Error vs Degrees of Freedom",
     xlab = "Degrees of Freedom",
     ylab = "Training Error")


# Plot test error
plot(results$df,
     results$Test_Error,
     type = "b",
     pch = 16,
     main = "Test Error vs Degrees of Freedom",
     xlab = "Degrees of Freedom",
     ylab = "Test Error")


# ------------------------------------------------------------
# 6. REPEAT 50 TIMES
# ------------------------------------------------------------

set.seed(1234)

all.results <- data.frame()

for (r in 1:50) {
  
  # Generate fresh data
  X <- runif(500, 5, 15)
  Y <- reg(X) + rnorm(500, 0, 5)
  
  # Train-test split
  train.index <- sample(1:500, 400)
  
  X.train <- X[train.index]
  Y.train <- Y[train.index]
  
  X.test <- X[-train.index]
  Y.test <- Y[-train.index]
  
  # Calculate errors for each df
  for (df in df.values) {
    
    model <- smooth.spline(X.train, Y.train, df = df)
    
    train.pred <- predict(model, X.train)$y
    test.pred <- predict(model, X.test)$y
    
    train.error <- mean((Y.train - train.pred)^2)
    test.error <- mean((Y.test - test.pred)^2)
    
    all.results <- rbind(
      all.results,
      data.frame(
        Repeat = r,
        df = df,
        Training_Error = train.error,
        Test_Error = test.error
      )
    )
  }
}


# Average errors
average.results <- aggregate(
  cbind(Training_Error, Test_Error) ~ df,
  data = all.results,
  FUN = mean
)

print(average.results)


# Plot average training error
plot(average.results$df,
     average.results$Training_Error,
     type = "b",
     pch = 16,
     main = "Average Training Error",
     xlab = "Degrees of Freedom",
     ylab = "Average Training Error")


# Plot average test error
plot(average.results$df,
     average.results$Test_Error,
     type = "b",
     pch = 16,
     main = "Average Test Error",
     xlab = "Degrees of Freedom",
     ylab = "Average Test Error")


# Best df based on average test error
best.df <- average.results$df[
  which.min(average.results$Test_Error)
]

cat("Best df from 50 repetitions =", best.df, "\n")


# ------------------------------------------------------------
# 7. BOSTON DATASET - 22-FOLD CROSS VALIDATION
# ------------------------------------------------------------

data(Boston)

X.boston <- Boston$lstat
Y.boston <- Boston$medv

df.values <- c(4, 5, 10, 20, 30, 40, 50)

set.seed(1234)

# Create 22 folds
folds <- sample(
  rep(1:22, length.out = length(Y.boston))
)

cv.results <- data.frame(
  df = df.values,
  CV_Error = NA
)

for (d in 1:length(df.values)) {
  
  df <- df.values[d]
  
  errors <- c()
  
  for (k in 1:22) {
    
    test.index <- which(folds == k)
    
    X.train <- X.boston[-test.index]
    Y.train <- Y.boston[-test.index]
    
    X.test <- X.boston[test.index]
    Y.test <- Y.boston[test.index]
    
    model <- smooth.spline(
      X.train,
      Y.train,
      df = df
    )
    
    prediction <- predict(model, X.test)$y
    
    error <- mean((Y.test - prediction)^2)
    
    errors <- c(errors, error)
  }
  
  cv.results$CV_Error[d] <- mean(errors)
}

print(cv.results)


# Plot CV error
plot(cv.results$df,
     cv.results$CV_Error,
     type = "b",
     pch = 16,
     main = "Boston Data: CV Error vs df",
     xlab = "Degrees of Freedom",
     ylab = "22-Fold CV Error")


# Best df
best.df.boston <- cv.results$df[
  which.min(cv.results$CV_Error)
]

cat("Best df for Boston dataset =", best.df.boston, "\n")


# ============================================================
# END OF PROBLEM SET 7
# ============================================================


download.file(url="https://tinyurl.com/advcastuff",destfile="/Users/Kairi/Documents/SUJAAAAA/Adv Reg/uwu.R")
