errorMetrics <- function(predictedValues, actualValues) {
  errors <- data.frame()
  errors$absoluteError <- abs( predictedValues - actualValues )
  errors$MAE <- mean(errors$absoluteError)
  
  errors$relative_error <- abs( absoluteError / actualValues )
  errors$MRE <- mean(errors$relative_error)
  
  errors$square_error <- absoluteError * absoluteError
  errors$RMSE <- sqrt( mean(errors$square_error) )
  
  errors
}