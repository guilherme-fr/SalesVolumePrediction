

productsLocation <- "datasets/existingproductattributes2017.csv"
algorithm = "rf"
# algorithm = "knn"
# algorithm = "svm"

####Loading data into memory####
products <- read.csv(productsLocation)

####Pre-processing####
products <- preProcessDataSet(products)

#Creating the train and test set from the sample
featureSelset <- createTrainAndTestSets(products, products$Volume, 0.7, 998)

print("#########FEATURE SELECTION PROCESS#########")
cat("\n")
if (algorithm == "rf") {
  
  rfGrid <- expand.grid(mtry=c(1:6))
  
  print("Training Random Forest...")
  featureSelModel <- trainModel(featureSelset$training, 
                                Volume~ PositiveServiceReview + x4StarReviews 
                                + Recommendproduct, 
                                "rf", 
                                tuneGrid = rfGrid)
  print("Training finished!")
  
} else if (algorithm == "knn") {
  
  tuneLength <- 10
  preProc = c("center", "scale")
  print("Training K-NN...")
  featureSelModel <- trainModel(featureSelset$training, 
                                Volume~ PositiveServiceReview 
                                + x4StarReviews
                                + x2StarReviews
                                + NegativeServiceReview, 
                                "knn", 
                                tuneLength = tuneLength, 
                                preProc = preProc)
  print("Training finished!")
  
} else if (algorithm == "svm") {
  tuneLength <- 10
  preProc = c("center", "scale")
  print("Training SVM...")
  featureSelModel <- trainModel(featureSelset$training, 
                                Volume~ PositiveServiceReview
                                + x4StarReviews
                                + x2StarReviews
                                + NegativeServiceReview, 
                                "svmRadial", 
                                tuneLength = tuneLength,
                                preProc = preProc)
  print("Training finished!")
  
} 

cat("\n")
print("#########Training Metrics#########")
print(featureSelModel)

predictedTest <- predict(featureSelModel, featureSelset$testing)
assessModel <- postResample(predictedTest, featureSelset$testing$Volume)

cat("\n")
print("#########Testing Metrics#########")
print(assessModel)

importantVariables <- varImp(featureSelModel)
cat("\n")
print("#########Most Important Variables#########")
print(importantVariables)

errors <- errorMetrics(predictedTest, featureSelset$testing$Volume)



rankFeatures <- function(dataset, dependentVarName, method, 
                         percent = 0.75, seed = 123, trainControl, 
                         tuneLength = NULL, tuneGrid = NULL) {
  
  list <- createTrainAndTestSets(dataset, dataset[, dependentVarName], percent, seed)
  #Formula with all independent variables
  formula <- formula(paste(dependentVarName, "~ .", sep = ""))
  
  #training
  modelFit <- train(formula, data = list$training, method = method, 
                    trControl = trainControl, tuneLength = tuneLength, 
                    tuneGrid = tuneGrid, preProc = preProc, importance = TRUE)
  
  rank <- varImp(modelFit)
  rank
}