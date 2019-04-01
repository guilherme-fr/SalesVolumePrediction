errorMetrics <- function(predictedValues, actualValues) {
  absoluteError <- predictedValues - actualValues
  square_error <- absoluteError * absoluteError
  relative_error <- abs( absoluteError / actualValues )
  data.frame(absoluteError, relative_error, square_error)
}