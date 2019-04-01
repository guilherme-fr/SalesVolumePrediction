modelFile = "rf.rda"
# modelFile = "knn.rda"
# modelFile = "svm.rda"
newProductsFile = "datasets/newproductattributes2017.csv"
predictionsFile = "datasets/productPredictions.csv"
existingProductsFile <- "datasets/existingproductattributes2017.csv"

evaluateModel = TRUE

if (file.exists(modelFile)) {
  load(modelFile)
  print(paste("Model '", modelFile,"' loaded", sep = ""))
  
  if (evaluateModel) {
    #Code for just evaluate the model saved for all dataset
    
    products <- read.csv(existingProductsFile)
    #Pre-processing
    products <- preProcessDataSet(products)
    
    predictedValues <- predict(model, products)
    assessModel <- postResample(predictedValues, products$Volume)
    errors <- errorMetrics(predictedValues, products$Volume)
    print("Model test metrics:")
    print(assessModel)
  } else {
    #Code to make the predictiions on the new products
    
    newProducts <- read.csv(newProductsFile)
    newProductsPrepared <- preProcessDataSet(newProducts)
    predictedValues <- predict(model, newProductsPrepared)
    print("Prediction finished")
    
    productsPredicted <- newProducts
    productsPredicted$Volume <- predictedValues
    write.csv(productsPredicted, predictionsFile)
    print( paste("Prediction saved to file '", predictionsFile, "'", sep = "") )
  }
  
} else {
  print( paste("ERROR: It's not possible to load model. The file '", modelFile, "' does not exist", sep = "") )
}