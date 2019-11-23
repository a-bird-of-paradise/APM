library(tidyverse)
library(caret)
data(oil)

output_directory <- file.path(getwd(),"12.2")

set.seed(1827777)

TrainingIndices <- caret::createDataPartition(oilType,
                                              times = 1,
                                              p = 0.7)$Resample1

TrainingPreds <- fattyAcids[TrainingIndices,]
TrainingObs <- oilType[TrainingIndices]
TestingPreds <- fattyAcids[-TrainingIndices,]
TestingObs <- oilType[-TrainingIndices]

ctrl <- caret::trainControl(method = "cv",
                            classProbs = T,
                            summaryFunction = caret::multiClassSummary,
                            sampling = "up")

LinReg <- caret::train(TrainingPreds,
                       TrainingObs,
                       method = "multinom",
                       metric = "Accuracy",
                       trControl = ctrl,
                       tuneGrid = expand.grid(.decay = c(0, 0.0001, 0.001, 0.01, 0.1, 1)),
                       preProc = c("center","scale","nzv","corr"))

LDA <- caret::train(TrainingPreds,
                    TrainingObs,
                    method = "lda",
                    metric = "Accuracy",
                    trControl = ctrl,
                    preProc = c("center","scale"))

GLM <- caret::train(TrainingPreds,
                    TrainingObs,
                    method = "glmnet",
                    tuneGrid = expand.grid(.alpha = c(0,.1,.2,.4,.6,.8,1.0),
                                           .lambda = seq(.01,2,length = 40)),
                    preProc = c("center","scale"),
                    trControl = ctrl,
                    metric = "Accuracy")
