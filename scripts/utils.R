#' Calculates the following error metrics: absolute error, Mean Absolute Error, relative error, 
#' Mean Relative Error, squared error, root mean squared error
errorMetrics <- function(predictedValues, actualValues) {
  absolute_error <- abs( predictedValues - actualValues )
  MAE <- mean(absolute_error, na.rm = TRUE)
  
  relative_error <- abs( absolute_error / actualValues )
  #Removing the possible 'infinite' values due to actualValue = 0
  relative_error[is.infinite(relative_error)] <- NA
  MRE <- mean(relative_error, na.rm = TRUE)
  
  square_error <- absolute_error * absolute_error
  RMSE <- sqrt( mean(square_error, na.rm = TRUE) )
  
  errors <- data.frame(absolute_error, MAE, relative_error, MRE, square_error, RMSE)
  errors
}

rankFeatures <- function(dataset, dependentVarName, percent = 0.75, seed = 123, ...) {
  
  partition <- createTrainAndTestSets(dataset, dataset[, dependentVarName], percent, seed)
  #Formula with all independent variables
  formula <- formula(paste(dependentVarName, "~ .", sep = ""))
  
  #training
  modelFit <- train(formula, data = partition$training, importance = TRUE, ...)
  
  rank <- varImp(modelFit)
  featuresRank <- rank[[1]]
  dfFeaturesRank <- data.frame(variable = row.names(featuresRank), overall = featuresRank[,1])
  dfFeaturesRank <- dfFeaturesRank[order(-dfFeaturesRank[,2]), ]
  
  dfFeaturesRank
}

runModels <- function(dataset, dependentVarName, featuresName, percent = 0.75, seed = 123, ...) {
  
  partitions <- createTrainAndTestSets(dataset, dataset[, dependentVarName], percent, seed)
  
  formulas <- buildFormulas(dependentVarName, featuresName)
  result <- vector("list", length(formulas))
  i <- 1
  for (formulaStr in formulas) {
    formula <- formula(formulaStr)
    modelFit <- train(formula, data = partitions$training, ...)
    
    testPrediction <- predict(modelFit, partitions$testing)
    testMetrics <- postResample(testPrediction, partitions$testing[ , dependentVarName])
    testErrors <- errorMetrics(testPrediction, partitions$testing[ , dependentVarName])
    result[[i]] <- list(method = method, formula = formulaStr, trainMetrics = modelFit[["results"]], 
         testMetrics = testMetrics, testErrors = testErrors)
    i <- i + 1
  }
  
  result
}

buildFormulas <- function(dependentVarName, featuresName) {
  formulas <- c()
  
  for( i in 1:length(featuresName) ) {
    fCombinations <- combn(featuresName, i)
    for (col in 1:ncol(fCombinations)) {
      featureSet <- fCombinations[ , col]
      
      #The first feature is the most important one. It must be in all final formulas
      if (featuresName[1] %in% featureSet) {
        
        #Beginning of the formula
        formulaString <- paste(dependentVarName, "~", sep = "")
        for (feature in featureSet) {
          formulaString <- paste(formulaString, feature, "+", sep = " ")
        }
        #Removing the last '+'
        formulaString <- substr(formulaString, 1, nchar(formulaString) - 2)
        formulas <- append(formulas, formulaString)
      }
    }
  }
  
  formulas
}