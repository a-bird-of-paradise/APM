library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
data("ChemicalManufacturingProcess")
ChemicalManufacturingProcess

set.seed(666)

Predictors <- ChemicalManufacturingProcess %>% select(-Yield)
Response <- ChemicalManufacturingProcess$Yield

x <- caret::train(Predictors,
                  Response,
                  method = "pls",
                  tuneLength = 10,
                  preProcess = c("knnImpute","pca"),
                  trControl = caret::trainControl(method = "repeatedcv",
                                                  repeats = 5))
x$results


