# Function to calculate the W-statistic for the Shapiro-Wilk test
swTest = function(data, histogram = FALSE, qq_plot = FALSE) {
  # Validate input data
  if (any(is.na(data)) || any(is.infinite(data))) {
    stop("The data contains NA or inf values")
  }
  if (!is.numeric(data)) {
    stop("The data format must be numeric")
  }
  if (!is.vector(data)) {
    stop("The data must be a vector")
  }
  
  n = length(data)
  if (!(n >= 3 && n <= 5000)) {
    stop("The sample size must be between 3 and 5000")
  }
  
  # Sort the data
  data_sorted = sort(data)
  
  # Calculate expected quantiles of a standard normal distribution
  m = qnorm((1:n - 3/8) / (n + 1/4))
  
  # Calculate the a_i coefficients
  a = m / sqrt(sum(m^2))
  
  # Calculate the sample mean
  x_bar = mean(data)
  
  # Calculate the W statistic
  num = (sum(a * data_sorted))^2
  den = sum((data - x_bar)^2)
  W = num / den
  
  # Optional: display histogram and QQ plot
  if (histogram) {
    hist(data, main = "Histogram of Data", xlab = "Data Values", ylab = "Frequency")
  }
  if (qq_plot) {
    qqnorm(data)
    qqline(data, col = "red")
  }
  
  return(W)
}


library(testthat)

# Test with normal data
test_that("Test with normal data", {
  data = rnorm(100)
  expect_type(swTest(data), "double")
})

# Test handling of NA values
test_that("Handling NA values", {
  data = c(rnorm(99), NA)
  expect_error(swTest(data))
})

# Test handling of infinite values
test_that("Handling infinite values", {
  data = c(rnorm(99), Inf)
  expect_error(swTest(data))
})

# Test handling of non-numeric data
test_that("Handling non-numeric data", {
  data = c("a", "b", "c")
  expect_error(swTest(data))
})

# Test handling of non-vector data
test_that("Handling non-vector data", {
  data = matrix(rnorm(100), nrow = 10)
  expect_error(swTest(data))
})

# Test sample size constraints
test_that("Handling small sample size", {
  data = rnorm(2)
  expect_error(swTest(data))
})

test_that("Handling large sample size", {
  data = rnorm(5001)
  expect_error(swTest(data))
})

