#' Logistic Regression from Scratch
#'
#' This function fits a logistic regression model using gradient descent.
#'
#' @param x A numeric vector representing the independent variable.
#' @param y A binary numeric vector representing the dependent variable.
#' @param max_iter The maximum number of iterations for gradient descent. Default is 1000.
#' @param learning_rate The learning rate for gradient descent. Default is 0.01.
#' @return A list containing the intercept and slope of the fitted model.
#' @export
logistic_regression <- function(x, y, max_iter = 1000, learning_rate = 0.01) {

  # Initialize coefficients (intercept and slope)
  beta_0 <- 0
  beta_1 <- 0

  # Sigmoid function
  sigmoid <- function(z) {
    return(1 / (1 + exp(-z)))
  }

  # Gradient Descent for Logistic Regression
  for (i in 1:max_iter) {
    # Predictions (probabilities)
    y_pred <- sigmoid(beta_0 + beta_1 * x)

    # Calculate the gradients
    gradient_beta_0 <- sum(y_pred - y) / length(y)
    gradient_beta_1 <- sum((y_pred - y) * x) / length(y)

    # Update the coefficients
    beta_0 <- beta_0 - learning_rate * gradient_beta_0
    beta_1 <- beta_1 - learning_rate * gradient_beta_1
  }

  # Return results
  return(list(intercept = beta_0, slope = beta_1))
}
