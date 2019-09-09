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

postResampTbl <- caret::postResample(pred = knnPred, obs = testing_observations) %>%
  enframe %>%
  spread(key=name,value=value) %>%
  mutate(what = "knn")

knn_plot <- tibble(obs = testing_observations,
       pred = knnPred) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

ggsave(knn_plot,
       filename = file.path(output_directory,
                            "knn-pred-plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

knnTrainSubset <- predictors[1:5]
knnTestSubset <- testing_predictors[1:5]

knnModelSubset <- caret::train(x = knnTrainSubset,
                         y = raw_data$y,
                         method = "knn",
                         preProc = c("center","scale"),
                         tuneLength = 10)
knnPredSubset <- predict(knnModelSubset,knnTestSubset)

postResampTbl <- postResampTbl %>%
  bind_rows(caret::postResample(pred = knnPredSubset, obs = testing_observations) %>%
  enframe %>%
  spread(key=name,value=value) %>%
  mutate(what = "knn_sub"))

knn_subset_plot <- tibble(obs = testing_observations,
                   pred = knnPredSubset) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

ggsave(knn_subset_plot,
       filename = file.path(output_directory,
                            "knn-subset-pred-plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

# MARS

MARSModel <- caret::train(x = predictors %>% as.data.frame,
                          y = raw_data$y,
                          method = "earth",
                          trControl = trainControl(method = "cv"))

MARSPred <- predict(MARSModel, newdata = testing_predictors) %>% as.vector

postResampTbl <- postResampTbl %>%
  bind_rows(caret::postResample(pred = MARSPred, obs = testing_observations)  %>%
              enframe %>%
              spread(key=name,value=value) %>%
              mutate(what = "MARS"))

mars_plot <- tibble(obs = testing_observations,
       pred = MARSPred) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

ggsave(mars_plot,
       filename = file.path(output_directory,
                            "mars-pred-plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

# what does MARS do for V3? 

mars_plot_func <- function(i)
{
  newdata <-predictors %>%
    mutate_at(vars(-i),mean)
  
  newpred <- predict(MARSModel,
                     newdata = newdata) %>%
    as.tibble
  
  the_plot <- newdata %>% 
    select(i) %>% 
    bind_cols(newpred) %>% 
    `names<-`(.,c("x","y")) %>% 
    arrange(x) %>%
    ggplot(aes(x=x,y=y)) + geom_line() + ggtitle(paste0("predictor ",i))
  
  ggsave(the_plot,
         filename = file.path(output_directory,
                              paste0("mars-var-",i,".png")),
         width = 8,
         height = 6,
         dpi = 100)
  
  return(the_plot)
}

1:10 %>% purrr::map(mars_plot_func)

# SVM

SVMModel <- caret::train(x = predictors,
                         y = raw_data$y,
                         method = "svmRadial",
                         preProc = c("center","scale"),
                         tuneLength = 14,
                         trControl = caret::trainControl(method = "cv"),
                         tuneGrid = expand.grid(sigma = seq(0.01,0.10,by=0.01),
                                                C = 2 ** (-2:11)))

SVMPred <- predict(SVMModel, newdata = testing_predictors)

postResampTbl <- postResampTbl %>%
  bind_rows(
    caret::postResample(pred = SVMPred, obs = testing_observations) %>%
      enframe %>%
      spread(key=name,value=value) %>%
      mutate(what = "SVM"))

svm_plot <- tibble(obs = testing_observations,
       pred = SVMPred) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

ggsave(svm_plot,
       filename = file.path(output_directory,
                            "svn-pred-plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

# NN

NNetModel <- caret::train(x = predictors,
                          y = raw_data$y,
                          method = "avNNet",
                          tuneGrid = expand.grid(.decay = c(0.001,0.01,0.1),
                                                 .size = 1:10,
                                                 .bag = F),
                          preProc = c("center","scale"),
                          trControl = caret::trainControl(method = "cv"),
                          linout = T,
                          trace = F,
                          MaxNWts = 10 * (ncol(predictors) + 1) + 10 + 1,
                          maxit = 50,
                          repeats = 200)

NNPred <- predict(NNetModel, newdata = testing_predictors)

postResampTbl <- postResampTbl %>%
  bind_rows(
    caret::postResample(pred = NNPred, obs = testing_observations) %>%
      enframe %>%
      spread(key=name,value=value) %>%
      mutate(what = "avNNet"))


av_nnet_plot <- tibble(obs = testing_observations,
       pred = NNPred) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

ggsave(av_nnet_plot,
       filename = file.path(output_directory,
                            "av-nnet-pred-plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

knitr::kable(postResampTbl)
