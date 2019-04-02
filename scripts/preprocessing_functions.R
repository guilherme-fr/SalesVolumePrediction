library(caret)

preProcessDataSet <- function(productsData) {
  preparedData <- productsData
  #Removing the BestSellersRank column (missing values)
  preparedData$BestSellersRank <- NULL 
  
  #Dummify factors columns
  # dummy <- dummyVars(" ~ .", data = preparedData)
  # preparedData <- data.frame(predict(dummy, newdata = preparedData))
  
  #Removing the factors attributes
  preparedData$ProductType <- NULL
  
  #Removing attributes with colinearity and perfect correlation with the dependent variable
  removeCol <- checkColinearity(preparedData, 0.9, 0.999, "Volume")
  preparedData <- preparedData[ , !(names(preparedData) %in% removeCol)]
  
  #Removing the product ID
  preparedData$ProductNum <- NULL
  
  #Remove the outliers
  preparedData <- preparedData[ preparedData$Volume < 7000, ]
  
  
  preparedData
}

pairWiseCorrMatrix <- function(correlationMatrix) {
  dfCorrMatrix <- data.frame(correlationMatrix)
  i <- 1
  j <- 1
  value <- vector()
  attribute1 <- vector()
  attribute2 <- vector()
  
  #The following loops runs through the lower trigle of the correlation matrix not including the diagonal
  while(i <= nrow(dfCorrMatrix)) {
    while(j <= ncol(dfCorrMatrix)) {
      if (i == j) {
        break
      } else {
        value <- append(value, dfCorrMatrix[i, j])
        colName <- attributes(dfCorrMatrix)$names[j]
        rowName <- attributes(dfCorrMatrix)$row.names[i]
        attribute1 <- append(attribute1, colName)
        attribute2 <- append(attribute2, rowName)
      }
      j <- j + 1
    }
    j <- 1
    i <- i + 1
  }
  
  pairWiseCorr <- data.frame(attribute1, attribute2, value)
  pairWiseCorr <- pairWiseCorr[order(-pairWiseCorr$value), ]
  pairWiseCorr
}

checkColinearity <- function(dataset, colinCutValue, dependCutValue, dependentVarName) {
  result <- c()
  correlationMatrix <- cor(dataset)
  pwCorrMatrix <- pairWiseCorrMatrix(correlationMatrix)
  
  #Runs through the pairwise correlation matrix
  for (i in (1:nrow(pwCorrMatrix)) ) {
    if ( is.na(pwCorrMatrix[i, 3]) ) {
      #Ignoring NA values
      next
    }
    
    correlationValue <- abs( pwCorrMatrix[i, 3] )
    if (pwCorrMatrix[i, 1] != dependentVarName && pwCorrMatrix[i, 2] != dependentVarName) {
      #This are two independent variables. Then, we need to check for colinearity
      
      if (correlationValue >= colinCutValue) {
        #We should check the value of each variable with the dependent variable. 
        #The variable with the greater correlaction with the dependent variable should stay.
        charAttr1 <- as.character(pwCorrMatrix[i, 1])
        charAttr2 <- as.character(pwCorrMatrix[i, 2])
        corrDependentAttr1 <- abs( correlationMatrix[charAttr1, dependentVarName] )
        corrDependentAttr2 <- abs( correlationMatrix[charAttr2, dependentVarName] )
        if (corrDependentAttr1 > corrDependentAttr2) {
          #We should remove the attribute 2
          result <- append(result, charAttr2)
        } else {
          #We should remove the attribute 1
          result <- append(result, charAttr1)
        }
      }
    } else {
      #One of the two variables is the dependent variable
      #We need to verify if the correlation value is greater than the dependent variable cut value
      if (correlationValue >= dependCutValue) {
        charAttr1 <- as.character(pwCorrMatrix[i, 1])
        charAttr2 <- as.character(pwCorrMatrix[i, 2])
        if (pwCorrMatrix[i, 1] == dependentVarName) {
          #The attribute1 is the dependent variable. We should remove the attribute 2
          result <- append(result, charAttr2)
        } else {
          #The attribute2 is the dependent variable. We should remove the attribute 1
          result <- append(result, charAttr1)
        }
      }
    }
  }
  
  result
}