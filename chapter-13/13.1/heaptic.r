library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
data(hepatic)

output_directory <- file.path(getwd(),"13.1")

set.seed(879)

TrainingIndices <- caret::createDataPartition(injury,
                                              times = 1,
                                              p = 0.7)$Resample1

injury_reorder <- injury %>%
  forcats::fct_relevel(c("None","Mild","Severe"))

biochem <- bio %>% bind_cols(chem)

caret::upSample(bio,injury_reorder)

TrainingPredsBio <- bio[TrainingIndices,]
TrainingPredsChem <- chem[TrainingIndices,]
TrainingPredsBioChem <- biochem[TrainingIndices,]
TrainingObs <- injury_reorder[TrainingIndices]

TestingPredsBio <- bio[-TrainingIndices,]
TestingPredsChem <- chem[-TrainingIndices,]
TestingPredsBioChem <- biochem[-TrainingIndices,]
TestingObs <- injury_reorder[-TrainingIndices]

ctrl <- caret::trainControl(method = "cv",
                            classProbs = T,
                            summaryFunction = caret::multiClassSummary,
                            returnResamp = 'all')

### Mixture Discriminant Analysis ----

caret::preProcess(TrainingPredsChem,method = c('nzv','corr')) %>%
  predict(., TrainingPredsChem) %>%
  str

MDA_Bio <- caret::train(TrainingPredsBio,
                        TrainingObs,
                        method = 'mda',
                        metric = 'Accuracy',
                        tuneGrid = expand.grid(.subclasses = 1:10),
                        trControl = ctrl,
                        preProcess = c('nzv','corr'))

MDA_Chem <- caret::train(TPC_PP,
                         TrainingObs,
                         method = 'mda',
                         metric = 'Accuracy',
                         tuneGrid = expand.grid(.subclasses = 1:10),
                         trControl = ctrl,
                         preProcess = c('nzv','corr'),
                         cutoff = 0.6) 

MDA_BioChem <- caret::train(TrainingPredsBioChem,
                            TrainingObs,
                            method = 'mda',
                            metric = 'Accuracy',
                            tuneGrid = expand.grid(.subclasses = 1:10),
                            trControl = ctrl,
                            preProcess = c('nzv','corr','pca'),
                            cutoff = 0.6,
                            pcaComp = 20) # very merciless predictor trimming

### Quadratic Discriminant Analysis - FAILS ----

QDA_Bio <- caret::train(TrainingPredsBio,
                        TrainingObs,
                        method = 'QdaCov',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        preProcess = c('nzv','corr','pca','center','scale'),
                        pcaComp = 5)

QDA_Chem<- caret::train(TrainingPredsChem,
                        TrainingObs,
                        method = 'QdaCov',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        preProcess = c('nzv','corr'))

QDA_BioChem <- caret::train(TrainingPredsBioChem,
                            TrainingObs,
                            method = 'QdaCov',
                            metric = 'Accuracy',
                            trControl = ctrl,
                            preProcess = c('nzv','corr'))

### Regularised Discriminant Analysis - FAILS ----

RDA_Bio <- caret::train(TrainingPredsBio,
                        TrainingObs,
                        method = 'rda',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        preProcess = c('nzv','corr','pca'),
                        cutoff = 0.5,
                        pcaComp = 10)

QDA_Chem<- caret::train(TrainingPredsChem,
                        TrainingObs,
                        method = 'rda',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        preProcess = c('nzv','corr','pca'),
                        cutoff = 0.5,
                        pcaComp = 10)

QDA_BioChem <- caret::train(TrainingPredsBioChem,
                            TrainingObs,
                            method = 'rda',
                            metric = 'Accuracy',
                            trControl = ctrl,
                            preProcess = c('nzv','corr','pca'),
                            cutoff = 0.5,
                            pcaComp = 10)

### Neural Networks ----

NN_Bio <- caret::train(TrainingPredsBio,
                       TrainingObs,
                       method = 'avNNet',
                       metric = 'Accuracy',
                       trControl = ctrl,
                       preProcess = c('center','scale','nzv','corr'),
                       maxit = 30,
                       tuneGrid = expand.grid(.size = seq(1,9,by=2),
                                              .decay = c(0, 0.1, 1, 2),
                                              .bag = T))


NN_Chem <- caret::train(TrainingPredsChem,
                        TrainingObs,
                        method = 'avNNet',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        preProcess = c('center','scale','nzv','corr'),
                        maxit = 30,
                        tuneGrid = expand.grid(.size = seq(1,9,by=2),
                                               .decay = c(0, 0.1, 1, 2),
                                               .bag = T))


NN_BioChem <- caret::train(TrainingPredsBioChem,
                           TrainingObs,
                           method = 'avNNet',
                           metric = 'Accuracy',
                           trControl = ctrl,
                           preProcess = c('center','scale','nzv','corr'),
                           maxit = 30,
                           tuneGrid = expand.grid(.size = seq(1,9,by=2),
                                                  .decay = c(0, 0.1, 1, 2),
                                                  .bag = T))


### Flexible Discriminant Analysis  ----

library(earth)

FDA_Bio <- caret::train(TrainingPredsBio,
                        TrainingObs,
                        method = 'fda',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        tuneGrid = expand.grid(.degree = 1,
                                               .nprune = 2:25))

FDA_Chem<- caret::train(TrainingPredsChem,
                        TrainingObs,
                        method = 'fda',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        tuneGrid = expand.grid(.degree = 1,
                                               .nprune = 2:25))

FDA_BioChem <- caret::train(TrainingPredsBioChem,
                            TrainingObs,
                            method = 'fda',
                            metric = 'Accuracy',
                            trControl = ctrl,
                            tuneGrid = expand.grid(.degree = 1,
                                                   .nprune = 2:25))

### Support Vector Machines ----

SVM_Bio <- caret::train(TrainingPredsBio,
                        TrainingObs,
                        method = 'svmRadial',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        tuneLength = 10,
                        preProcess = c('center','scale')
)


SVM_Chem <- caret::train(TrainingPredsChem,
                         TrainingObs,
                         method = 'svmRadial',
                         metric = 'Accuracy',
                         trControl = ctrl,
                         tuneLength = 10,
                         preProcess = c('center','scale')
)


SVM_BioChem <- caret::train(TrainingPredsBioChem,
                            TrainingObs,
                            method = 'svmRadial',
                            metric = 'Accuracy',
                            trControl = ctrl,
                            tuneLength = 10,
                            preProcess = c('center','scale')
)


### K Nearest Neighbours ----

KNN_Bio <- caret::train(TrainingPredsBio,
                        TrainingObs,
                        method = 'knn',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        tuneGrid = expand.grid(.k = 1:20),
                        preProcess = c('center','scale','nzv')
)


KNN_Chem <- caret::train(TrainingPredsChem,
                         TrainingObs,
                         method = 'knn',
                         metric = 'Accuracy',
                         trControl = ctrl,
                         tuneGrid = expand.grid(.k = 1:20),
                         preProcess = c('center','scale','nzv')
)


KNN_BioChem <- caret::train(TrainingPredsBioChem,
                            TrainingObs,
                            method = 'knn',
                            metric = 'Accuracy',
                            trControl = ctrl,
                            tuneGrid = expand.grid(.k = 1:20),
                            preProcess = c('center','scale','nzv')
)

### Naïve Bayes ----

NB_Bio <- caret::train(TrainingPredsBio,
                       TrainingObs,
                       method = 'nb',
                       metric = 'Accuracy',
                       trControl = ctrl,
                       preProcess = c('nzv'),
                       tuneGrid = expand.grid(.fL = 0:2,
                                              .usekernel = T,
                                              .adjust = 1:3))
NB_Chem <- caret::train(TrainingPredsChem,
                        TrainingObs,
                        method = 'nb',
                        metric = 'Accuracy',
                        trControl = ctrl,
                        preProcess = c('nzv'),
                        tuneGrid = expand.grid(.fL = 0:2,
                                               .usekernel = T,
                                               .adjust = 1:3))
NB_BioChem <- caret::train(TrainingPredsBioChem,
                           TrainingObs,
                           method = 'nb',
                           metric = 'Accuracy',
                           trControl = ctrl,
                           preProcess = c('nzv'),
                           tuneGrid = expand.grid(.fL = 0:2,
                                                  .usekernel = T,
                                                  .adjust = 1:3))
