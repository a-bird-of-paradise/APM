library(tidyverse)
library(caret)
library(mlbench)

output_directory <- file.path(getwd(),"7.2")

raw_data <- mlbench.friedman1(n = 200, sd = 1)

predictors <- raw_data$x %>% as.tibble
featurePlot(predictors, raw_data$y)

testing_data <- mlbench.friedman1(n = 5000, sd = 1)

testing_predictors <- testing_data$x %>% as.tibble
testing_observations<-testing_data$y

# K nearest neighbours 

knnModel <- caret::train(x = predictors,
                         y = raw_data$y,
                         method = "knn",
                         preProc = c("center","scale"),
                         tuneLength = 10)

knnPred <- predict(knnModel, newdata = testing_predictors)

caret::postResample(pred = knnPred, obs = testing_observations)

tibble(obs = testing_observations,
       pred = knnPred) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

# MARS

MARSModel <- caret::train(x = predictors %>% as.data.frame,
                          y = raw_data$y,
                          method = "earth",
                          trControl = trainControl(method = "cv"))

MARSPred <- predict(MARSModel, newdata = testing_predictors) %>% as.vector

caret::postResample(pred = MARSPred, obs = testing_observations)

tibble(obs = testing_observations,
       pred = MARSPred) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

MARSModel %>% summary

plotmo(MARSModel$finalModel)

# SVM
# NN
