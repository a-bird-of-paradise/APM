library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
data(permeability)

set.seed(776644331)

output_folder <- file.path(getwd(),"7.4")

pointless_vars <- caret::nearZeroVar(fingerprints)

meaningful_predictors <- fingerprints[,-pointless_vars] %>% as.tibble

TrainingIndices <- caret::createDataPartition(permeability,p=0.7)[[1]]

TrainingPredictors <- meaningful_predictors[TrainingIndices,]
TestingPredictors <- meaningful_predictors[-TrainingIndices,]

TrainingObs <- permeability[TrainingIndices]
TestingObs <- permeability[-TrainingIndices]

# SVM

SVMModel <- caret::train(TrainingPredictors,
                         TrainingObs,
                         method = "svmRadial",
                         tuneLength = 14,
                         trControl = caret::trainControl(method = "cv"),
                         preProc = c("center","scale"))

# KNN 

KNNModel <- caret::train(TrainingPredictors,
                         TrainingObs,
                         method = "knn",
                         tuneGrid = data.frame(.k = 1:20),
                         trControl = caret::trainControl(method = "cv"),
                         preProc = c("center","scale"))

# NNets

NNetModel <- caret::train(TrainingPredictors,
                          TrainingObs,
                          method = "avNNet",
                          tuneGrid = expand.grid(.decay = c(0.001, 0.01, 0.1),
                                                 .size = 1:10,
                                                 .bag = F),
                          trControl = caret::trainControl(method = "cv"),
                          linout = T,
                          trace = T,
                          MaxNWts = 10 * (ncol(TrainingPredictors) + 1) * 10+1,
                          maxIt = 50,
                          repeats = 100,
                          preProc = c("center","scale"))

# MARS

MarsModel <- caret::train(TrainingPredictors %>% as.data.frame,
                          TrainingObs,
                          method = "earth",
                          trControl = caret::trainControl(method = "cv"),
                          tuneLength = 10)


models <- list(
  KNN = KNNModel,
  SVM = SVMModel,
  NNet = NNetModel,
  Mars = MarsModel)

comp_plot <- models %>%
  purrr::map_df(~ tibble(pred = predict(.x, TestingPredictors) %>%
                           as.vector,
                         obs = TestingObs),
                .id = "id")  %>%
  ggplot(aes(x=pred,y=obs)) +
  geom_point() +
  facet_wrap(~ id) + 
  geom_abline()

comp_plot_log <- models %>%
  purrr::map_df(~ tibble(pred = predict(.x, TestingPredictors) %>%
                           as.vector,
                         obs = TestingObs),
                .id = "id")  %>%
  ggplot(aes(x=pred,y=obs)) +
  geom_point() +
  facet_wrap(~ id) + 
  geom_abline() + 
  scale_x_log10() + 
  scale_y_log10()

ggsave(comp_plot,
       filename = file.path(output_folder,"comparison.png"),
       width = 8,
       height = 6,
       dpi = 100)


ggsave(comp_plot_log,
       filename = file.path(output_folder,"comparison-log.png"),
       width = 8,
       height = 6,
       dpi = 100)

caret::postResample(pred = predict(KNNModel,TestingPredictors),
                    obs = TestingObs) %>%
  enframe %>%
  spread(key=name,value=value) %>%
  mutate(what="KNN") %>%
  bind_rows(
    caret::postResample(pred = predict(SVMModel,TestingPredictors),
                        obs = TestingObs) %>%
      enframe %>%
      spread(key=name,value=value) %>%
      mutate(what="SVM"))%>%
  bind_rows(
    caret::postResample(pred = predict(MarsModel,TestingPredictors),
                        obs = TestingObs) %>%
      enframe %>%
      spread(key=name,value=value) %>%
      mutate(what="MARS"))%>%
  bind_rows(
    caret::postResample(pred = predict(NNetModel,TestingPredictors),
                        obs = TestingObs) %>%
      enframe %>%
      spread(key=name,value=value) %>%
      mutate(what="AvNNet")) %>%
  knitr::kable(.)

