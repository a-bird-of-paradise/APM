library(tidyverse)
library(caret)
data(tecator)

set.seed(18888889)

output_folder <- file.path(getwd(),"7.3")

absorp_preds <- absorp %>%
  as.tibble

fats <- endpoints[,2] %>% as.vector

training_indices <- caret::createDataPartition(fats,p = 0.7)[[1]]

training_preds <- absorp_preds[training_indices,]
training_obs <- fats[training_indices]

testing_preds <- absorp_preds[-training_indices,]
testing_obs <- fats[-training_indices]

PreProcessorPCA <- caret::preProcess(training_preds,
                                     method = c("pca"),
                                     thresh = 0.95)

training_preds_pca <- predict(PreProcessor, training_preds)
testing_preds_pca <- predict(PreProcessor, testing_preds)

# SVM 

SVMModelPCA <- caret::train(training_preds_pca,
                            training_obs,
                            method = "svmRadial",
                            tuneLength = 14,
                            trControl = caret::trainControl(method = "cv"))

SVMModel <- caret::train(training_preds,
                         training_obs,
                         method = "svmRadial",
                         tuneLength = 14,
                         trControl = caret::trainControl(method = "cv"),
                         preProc = c("center","scale"))

# KNN 

KNNModelPCA <- caret::train(training_preds_pca,
                            training_obs,
                            method = "knn",
                            tuneGrid = data.frame(.k = 1:20),
                            trControl = caret::trainControl(method = "cv"))
KNNModel <- caret::train(training_preds,
                         training_obs,
                         method = "knn",
                         tuneGrid = data.frame(.k = 1:20),
                         trControl = caret::trainControl(method = "cv"),
                         preProc = c("center","scale"))

# NNets

NNetModelPCA <- caret::train(training_preds_pca,
                             training_obs,
                             method = "avNNet",
                             tuneGrid = expand.grid(.decay = c(0.001, 0.01, 0.1),
                                                    .size = 1:10,
                                                    .bag = F),
                             trControl = caret::trainControl(method = "cv"),
                             linout = T,
                             trace = F,
                             MaxNWts = 10 * (ncol(training_preds_pca) + 1) * 10+1,
                             maxIt = 50,
                             repeats = 200)

NNetModel <- caret::train(training_preds,
                          training_obs,
                          method = "avNNet",
                          tuneGrid = expand.grid(.decay = c(0.001, 0.01, 0.1),
                                                 .size = 1:10,
                                                 .bag = F),
                          trControl = caret::trainControl(method = "cv"),
                          linout = T,
                          trace = F,
                          MaxNWts = 10 * (ncol(training_preds_pca) + 1) * 10+1,
                          maxIt = 50,
                          repeats = 200,
                          preProc = c("center","scale"))

# MARS

MarsModel <- caret::train(training_preds %>% as.data.frame,
                          training_obs,
                          method = "earth",
                          trControl = caret::trainControl(method = "cv"),
                          tuneLength = 10)

MarsModelPCA <- caret::train(training_preds_pca %>% as.data.frame,
                          training_obs,
                          method = "earth",
                          trControl = caret::trainControl(method = "cv"),
                          tuneLength = 10)

models <- list(
  KNN = KNNModel,
  SVM = SVMModel,
  NNet = NNetModel,
  Mars = MarsModel)

models.pca <- list(
  KNN = KNNModelPCA,
  SVM = SVMModelPCA,
  NNet = NNetModelPCA,
  Mars = MarsModelPCA
)

comp_plot <- models %>%
  purrr::map_df(~ tibble(pred = predict(.x, testing_preds) %>%
                           as.vector,
                         obs = testing_obs),
                .id = "id") %>%
  mutate(pca = F) %>%
  bind_rows(models.pca %>%
              purrr::map_df(~ tibble(pred = predict(.x, testing_preds_pca) %>%
                                       as.vector,
                                     obs = testing_obs),
                            .id = "id") %>% 
              mutate(pca = T)) %>%
  ggplot(aes(x=pred,y=obs)) +
  geom_point() +
  facet_grid(id ~ pca) + 
  geom_abline()

ggsave(comp_plot,
       filename = file.path(output_folder,"comparison.png"),
       width = 8,
       height = 6,
       dpi = 100)
