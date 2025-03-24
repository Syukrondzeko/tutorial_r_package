#' Linear Regression from Scratch
#'
#' This function fits a linear regression model using the Ordinary Least Squares (OLS) method.
#'
#' @param x A numeric vector representing the independent variable.
#' @param y A numeric vector representing the dependent variable.
#' @return A list containing the intercept, slope, and predictions.
#' @export
linear_regression <- function(x, y) {
  # Step 1: Calculate means of x and y
  x_bar <- mean(x)
  y_bar <- mean(y)

  # Step 2: Calculate the slope (beta_1)
  beta_1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)

  # Step 3: Calculate the intercept (beta_0)
  beta_0 <- y_bar - beta_1 * x_bar

  # Step 4: Predictions
  y_pred <- beta_0 + beta_1 * x

  # Step 5: Return results
  return(list(intercept = beta_0, slope = beta_1, predictions = y_pred))
}
