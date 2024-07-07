#' Calculate Mean, Median, Variance, and Standard Deviation
#' 
#' @param df Data frame to use the function on
#' @param col Column name to perform calculations on
#' @return A list with the computed mean, median, variance, and standard deviation
#' @examples
#' stats(mtcars, "mpg")
statsparam <- function(df, col) {
  mean_value <- mean(df[[col]])
  median_value <- median(df[[col]])
  variance_value <- var(df[[col]])
  sd_value <- sd(df[[col]])
  list(mean = mean_value, median = median_value, variance = variance_value, sd = sd_value)
}

#' Calculate Quantiles
#' 
#' @param df Data frame to use the function on
#' @param col Column name to perform calculations on
#' @param prob Numeric vector of probabilities to specify which quantiles to calculate
#' @return A numeric vector of quantiles
#' @examples
#' quantiles(mtcars, "mpg", probs = c(0.25, 0.5, 0.75))
quantiles <- function(df, col, prob) {
  quantile(df[[col]], prob)
}

#' Visualize Box Plot
#' 
#' @param df Data frame to use the function on
#' @param col Column name to visualize in the plot
#' @param title Title for the box plot
#' @param ylab Label for the y-axis
#' @return A boxplot of the specified column
#' @examples
#' box_plot(mtcars, "mpg", title = "Box Plot", ylab = "Miles Per Gallon")
box_plot <- function(df, col, title = "", ylab = "Values") {
  boxplot(df[[col]], main = title, ylab = ylab)
}



