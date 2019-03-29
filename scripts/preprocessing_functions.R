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
  
  #TODO - Check the correlation matrix and remove the attributes with colinearity
  #TODO - Check the correlation matrix and remove perfect correlation with the Volume
  preparedData$x5StarReviews <- NULL
  preparedData$x1StarReviews <- NULL
  preparedData$x3StarReviews <- NULL
  
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