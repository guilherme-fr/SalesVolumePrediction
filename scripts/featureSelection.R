
productsLocation <- "datasets/existingproductattributes2017.csv"
# algorithm = "rf"
algorithm = "knn"

####Loading data into memory####
products <- read.csv(productsLocation)

####Pre-processing####
products <- preProcessDataSet(products)

#Creating the train and test set from the sample
featureSelset <- createTrainAndTestSets(products, products$Volume, 0.7, 123)

print("#########FEATURE SELECTION PROCESS#########")
cat("\n")
if (algorithm == "rf") {
  
  rfGrid <- expand.grid(mtry=c(1,2,3,4,5))
  
  print("Training Random Forest...")
  featureSelModel <- trainModel(featureSelset$training, Volume~ ., "rf", tuneGrid = rfGrid)
  print("Training finished!")
  
} else if (algorithm == "knn") {
  
  tuneLength <- 10
  print("Training K-NN...")
  featureSelModel <- trainModel(featureSelset$training, Volume~ ., "knn", tuneLength = tuneLength)
  print("Training finished!")
  
} else if (algorithm == "svm") {
  
}

cat("\n")
print("#########Training Metrics#########")
print(featureSelModel)

importantVariables <- varImp(featureSelModel)
cat("\n")
print("#########Most Important Variables#########")
print(importantVariables)

predictedTest <- predict(featureSelModel, featureSelset$testing)
assessModel <- postResample(predictedTest, featureSelset$testing$Volume)

cat("\n")
print("#########Testing Metrics#########")
print(assessModel)
