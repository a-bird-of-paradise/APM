library(tidyverse)
library(caret)
data(oil)

output_directory <- file.path(getwd(),"13.2")

set.seed(1827567)  # cherry picked so G is in both sets at least once

TrainingIndices <- caret::createDataPartition(oilType,
                                              times = 1,
                                              p = 0.5)$Resample1

TrainingPreds <- fattyAcids[TrainingIndices,]
TrainingObs <- oilType[TrainingIndices]
TestingPreds <- fattyAcids[-TrainingIndices,]
TestingObs <- oilType[-TrainingIndices]

ctrl <- caret::trainControl(method = "cv",
                            classProbs = TRUE,
                            summaryFunction = caret::multiClassSummary,
                            sampling = 'up')

MDA <- caret::train(TrainingPreds,
                    TrainingObs,
                    method = 'mda',
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(.subclasses = 1:10),
                    trControl = ctrl,
                    preProcess = c('nzv','corr'))

NN <- caret::train(TrainingPreds,
                       TrainingObs,
                       method = 'avNNet',
                       metric = 'Accuracy',
                       trControl = ctrl,
                       preProcess = c('center','scale','nzv','corr'),
                       maxit = 30,
                       tuneGrid = expand.grid(.size = seq(1,9,by=2),
                                              .decay = c(0, 0.1, 1, 2),
                                              .bag = T))
library(earth)
FDA <- caret::train(TrainingPreds,
                        TrainingObs,
                        method = 'fda',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        tuneGrid = expand.grid(.degree = 1,
                                               .nprune = 2:25))


SVM <- caret::train(TrainingPreds,
                        TrainingObs,
                        method = 'svmRadial',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        tuneLength = 10,
                        preProcess = c('center','scale')
)

KNN <- caret::train(TrainingPreds,
                        TrainingObs,
                        method = 'knn',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        tuneGrid = expand.grid(.k = 1:20),
                        preProcess = c('center','scale','nzv')
)

NB <- caret::train(TrainingPreds,
                       TrainingObs,
                       method = 'nb',
                       metric = 'Accuracy',
                       trControl = ctrl,
                       preProcess = c('nzv'),
                       tuneGrid = expand.grid(.fL = 0:2,
                                              .usekernel = T,
                                              .adjust = 1:3))

models <- list(FDA = FDA,
               KNN = KNN,
               MDA = MDA,
               NB = NB,
               NN = NN, 
               SVM = SVM)

summary_data <- models %>%
  purrr::map(~ predict(.x, TestingPreds)) %>%
  as_tibble %>%
  mutate(Obs = TestingObs) %>%
  gather(key = Model, value = Pred, -Obs) %>%
  mutate_if(is.factor,as.character) %>%
  mutate(match = Obs==Pred) %>%
  mutate(Obs = as.factor(Obs), Pred = as.factor(Pred))

oil_fit_plot <- summary_data %>%
  ggplot(aes(x = Obs, fill = match)) + geom_bar() + facet_wrap(~Model)

ggsave(plot = oil_fit_plot,
       filename = file.path(output_directory,"oil_fit_plot.png"),
       width = 12,
       height = 8,
       dpi = 100)

models %>%
  purrr::map_df(~ caret::confusionMatrix(predict(., TestingPreds),TestingObs)$overall %>%
               enframe,
               .id='id') %>%
  filter(name %in% c('Accuracy','Kappa')) %>%
  spread(key = id, value = value) %>%
  mutate_if(is.numeric, function(x) round(x,digits=3)) %>%
  knitr::kable(.)

summary_data %>%
  mutate(match = ifelse(match,1.0,0.0)) %>%
  group_by(Model,Obs) %>%
  summarise(group_score = sum(match)/n()) %>%
  summarise(Score = mean(group_score)) %>%
  mutate_if(is.numeric, function(x) round(x,digits=3)) %>%
  knitr::kable(.)
