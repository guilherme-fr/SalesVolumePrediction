
productsLocation <- "datasets/existingproductattributes2017.csv"
# classifierName = "rf"
# classifierName = "knn"
classifierName = "svm"

####Loading data into memory####
products <- read.csv(productsLocation)

#Pre-processing
products <- preProcessDataSet(products)

#Splitting in training and testing sets
list <- createTrainAndTestSets(products, products$Volume, 0.7, 998)

if (classifierName == "knn") {
  print("Training K-NN model...")
  
  #Training K-NN model
  model <- trainModel(list$training, Volume~ PositiveServiceReview, "knn", 10)
  
  print("Training finished!")
} else if (classifierName == "rf") {
  print("Training Random Forest model...")
  
  #Training Random Forest model
  rfGrid <- expand.grid(mtry=c(1:6))
  model <- trainModel(list$training, 
                      Volume~ PositiveServiceReview + x4StarReviews + Recommendproduct, 
                      "rf", 
                      tuneGrid = rfGrid)
  
  print("Training finished!")
} else if (classifierName == "svm") {
  print("Training SVM model...")
  
  #Training SVM model
  model <- trainModel(list$training, Volume~ PositiveServiceReview, "svmRadial", 10)
  
  print("Training finished!")
}

#saving model to file
save(model, file = paste(classifierName, ".rda", sep = "") )
print( paste("Model saved to file '", classifierName, ".rda'", sep = ""))

predictedTest <- predict(model, list$testing)
assessModel <- postResample(predictedTest, list$testing$Volume)
print("Model test metrics:")
print(assessModel)