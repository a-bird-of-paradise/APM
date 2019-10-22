library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
data(hepatic)

output_directory <- file.path(getwd(),"12.1")

set.seed(879)

TrainingIndices <- caret::createDataPartition(injury,
                                              times = 1,
                                              p = 0.7)$Resample1

injury_reorder <- injury %>%
  forcats::fct_relevel(c("None","Mild","Severe"))

biochem <- bio %>% bind_cols(chem)

TrainingPredsBio <- bio[TrainingIndices,]
TrainingPredsChem <- chem[TrainingIndices,]
TrainingPredsBioChem <- biochem[TrainingIndices,]
TrainingObs <- injury_reorder[TrainingIndices]

TestingPredsBio <- bio[-TrainingIndices,]
TestingPredsChem <- chem[-TrainingIndices,]
TestingPredsBioChem <- biochem[-TrainingIndices,]
TestingObs <- injury_reorder[-TrainingIndices]

histo_plot <- tibble(injury=injury_reorder) %>%
  ggplot(aes(x=injury,colour=injury,fill=injury)) + 
  geom_histogram(stat = "count")
ggsave(histo_plot,
       filename = file.path(output_directory,"histo-plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

ctrl <- caret::trainControl(method = "cv",
                            classProbs = T,
                            summaryFunction = caret::multiClassSummary)

# so class imbalance is kind of bad but not awful. so maybe confusion matrix scoring would
# do it. or perhaps AUC if there is a 3 class extension 

# pre processing: make a "thin" pre processor for sensitive classifiers,
# "mid" for those that don't like low variance ones 

ThinPreProcessorBio <- caret::preProcess(TrainingPredsBio,
                                         method = c("center",
                                                    "scale",
                                                    "nzv",
                                                    "corr"))
ThinPreProcessorChem <- caret::preProcess(TrainingPredsChem,
                                          method = c("center",
                                                     "scale",
                                                     "nzv",
                                                     "corr"))
ThinPreProcessorBioChem <- caret::preProcess(TrainingPredsBioChem,
                                             method = c("center",
                                                        "scale",
                                                        "nzv",
                                                        "corr"))
MidPreProcessorBio <- caret::preProcess(TrainingPredsBio,
                                        method = c("nzv"))         
MidPreProcessorChem <- caret::preProcess(TrainingPredsChem,
                                         method = c("nzv"))        
MidPreProcessorBioChem <- caret::preProcess(TrainingPredsBioChem,
                                            method = c("nzv"))                                                 

### logistic regression ----

LR_Bio <- caret::train(predict(ThinPreProcessorBio,TrainingPredsBio),
                       TrainingObs,
                       method = "multinom",
                       metric = "Accuracy",
                       trControl = ctrl,
                       tuneGrid = expand.grid(.decay = c(0, 0.0001, 0.001, 0.01, 0.1, 1)))

LR_Chem <- caret::train(predict(ThinPreProcessorChem,TrainingPredsChem),
                        TrainingObs,
                        method = "multinom",
                        metric = "Accuracy",
                        trControl = ctrl,
                        tuneGrid = expand.grid(.decay = c(0, 0.0001, 0.001, 0.01, 0.1, 1)))

LR_BioChem <- caret::train(predict(ThinPreProcessorBioChem,TrainingPredsBioChem),
                           TrainingObs,
                           method = "multinom",
                           metric = "Accuracy",
                           trControl = ctrl,
                           tuneGrid = expand.grid(.decay = c(0, 0.0001, 0.001, 0.01, 0.1, 1)))


LR_Bio_Conf <- caret::confusionMatrix(predict(LR_Bio,
                                              predict(ThinPreProcessorBio,TestingPredsBio)),
                                      TestingObs)

LR_Chem_Conf <- caret::confusionMatrix(predict(LR_Chem,
                                               predict(ThinPreProcessorChem,TestingPredsChem)),
                                       TestingObs)
LR_BioChem_Conf <- caret::confusionMatrix(predict(LR_BioChem,
                                                  predict(ThinPreProcessorBioChem,TestingPredsBioChem)),
                                          TestingObs)

### linear discriminant analysis ----

LDA_Bio <- caret::train(predict(MidPreProcessorBio,TrainingPredsBio),
                        TrainingObs,
                        method = "lda",
                        metric = "Accuracy",
                        trControl = ctrl)

LDA_Chem <- caret::train(predict(MidPreProcessorChem,TrainingPredsChem),
                         TrainingObs,
                         method = "lda",
                         metric = "Accuracy",
                         trControl = ctrl)

LDA_BioChem <- caret::train(predict(MidPreProcessorBioChem,TrainingPredsBioChem),
                            TrainingObs,
                            method = "lda",
                            metric = "Accuracy",
                            trControl = ctrl)


LDA_Bio_Conf <- caret::confusionMatrix(predict(LDA_Bio,
                                               predict(MidPreProcessorBio,TestingPredsBio)),
                                       TestingObs)

LDA_Chem_Conf <- caret::confusionMatrix(predict(LDA_Chem,
                                                predict(MidPreProcessorChem,TestingPredsChem)),
                                        TestingObs)

LDA_BioChem_Conf <- caret::confusionMatrix(predict(LDA_BioChem,
                                                   predict(MidPreProcessorBioChem,TestingPredsBioChem)),
                                           TestingObs)

### partial least squares discriminant analysis ----

PLSDA_Bio <- caret::train(predict(ThinPreProcessorBio,TrainingPredsBio),
                          TrainingObs,
                          method = "pls",
                          metric = "Accuracy",
                          trControl = ctrl,
                          tuneGrid = expand.grid(.ncomp = 1:20))

PLSDA_Chem <- caret::train(predict(ThinPreProcessorChem,TrainingPredsChem),
                           TrainingObs,
                           method = "pls",
                           metric = "Accuracy",
                           trControl = ctrl,
                           tuneGrid = expand.grid(.ncomp = 1:20))

PLSDA_BioChem <- caret::train(predict(ThinPreProcessorBioChem,TrainingPredsBioChem),
                              TrainingObs,
                              method = "pls",
                              metric = "Accuracy",
                              trControl = ctrl,
                              tuneGrid = expand.grid(.ncomp = 1:20))

PLSDA_Bio_Conf <- caret::confusionMatrix(predict(PLSDA_Bio,
                                                 predict(ThinPreProcessorBio,TestingPredsBio)),
                                         TestingObs)

PLSDA_Chem_Conf <- caret::confusionMatrix(predict(PLSDA_Chem,
                                                  predict(ThinPreProcessorChem,TestingPredsChem)),
                                          TestingObs)

PLSDA_BioChem_Conf <- caret::confusionMatrix(predict(PLSDA_BioChem,
                                                     predict(ThinPreProcessorBioChem,TestingPredsBioChem)),
                                             TestingObs)

### penalised regression ----

GLMPen_Bio <- caret::train(predict(ThinPreProcessorBio,TrainingPredsBio),
                           TrainingObs,
                           method = "glmnet",
                           metric = "Accuracy",
                           trControl = ctrl,
                           tuneGrid = expand.grid(.alpha = c(0.0,
                                                             0.1,
                                                             0.2,
                                                             0.4,
                                                             0.6,
                                                             0.8,
                                                             1.0),
                                                  .lambda = seq(.01, 2, length = 40)))

GLMPen_Chem <- caret::train(predict(ThinPreProcessorChem,TrainingPredsChem),
                            TrainingObs,
                            method = "glmnet",
                            metric = "Accuracy",
                            trControl = ctrl,
                            tuneGrid = expand.grid(.alpha = c(0.0,
                                                              0.1,
                                                              0.2,
                                                              0.4,
                                                              0.6,
                                                              0.8,
                                                              1.0),
                                                   .lambda = seq(.01, 2, length = 40)))

GLMPen_BioChem <- caret::train(predict(ThinPreProcessorBioChem,TrainingPredsBioChem),
                               TrainingObs,
                               method = "glmnet",
                               metric = "Accuracy",
                               trControl = ctrl,
                               tuneGrid = expand.grid(.alpha = c(0.0,
                                                                 0.1,
                                                                 0.2,
                                                                 0.4,
                                                                 0.6,
                                                                 0.8,
                                                                 1.0),
                                                      .lambda = seq(.01, 2, length = 40)))

GLMPen_Bio_Conf <- caret::confusionMatrix(predict(GLMPen_Bio,
                                                  predict(ThinPreProcessorBio,TestingPredsBio)),
                                          TestingObs)

GLMPen_Chem_Conf <- caret::confusionMatrix(predict(GLMPen_Chem,
                                                   predict(ThinPreProcessorChem,TestingPredsChem)),
                                           TestingObs)

GLMPen_BioChem_Conf <- caret::confusionMatrix(predict(GLMPen_BioChem,
                                                      predict(ThinPreProcessorBioChem,TestingPredsBioChem)),
                                              TestingObs)

### nearest scaled centroid ----

NSC_Bio <- caret::train(predict(MidPreProcessorBio,TrainingPredsBio),
                        TrainingObs,
                        method = "pam",
                        metric = "Accuracy",
                        trControl = ctrl,
                        tuneGrid = expand.grid(.threshold = 0:25))

NSC_Chem <- caret::train(predict(MidPreProcessorChem,TrainingPredsChem),
                         TrainingObs,
                         method = "pam",
                         metric = "Accuracy",
                         trControl = ctrl,
                         tuneGrid = expand.grid(.threshold = 0:25))


NSC_BioChem <- caret::train(predict(MidPreProcessorBioChem,TrainingPredsBioChem),
                            TrainingObs,
                            method = "pam",
                            metric = "Accuracy",
                            trControl = ctrl,
                            tuneGrid = expand.grid(.threshold = 0:25))


NSC_Bio_Conf <- caret::confusionMatrix(predict(NSC_Bio,
                                               predict(MidPreProcessorBio,TestingPredsBio)),
                                       TestingObs)

NSC_Chem_Conf <- caret::confusionMatrix(predict(NSC_Chem,
                                                predict(MidPreProcessorChem,TestingPredsChem)),
                                        TestingObs)

NSC_BioChem_Conf <- caret::confusionMatrix(predict(NSC_BioChem,
                                                   predict(MidPreProcessorBioChem,TestingPredsBioChem)),
                                           TestingObs)

# OK.  models

metric_table <- function(metric)
{
  list(LR = LR_Bio_Conf
     , LDA = LDA_Bio_Conf
     , PLSDA = PLSDA_Bio_Conf
     , NSC = NSC_Bio_Conf
     ,GLMPen = GLMPen_Bio_Conf) %>%
  purrr::map_df(~ .x$byClass["Class: None",metric]) %>%
  mutate(what = "Bio") %>%
  bind_rows(list(LR = LR_Chem_Conf
                 , LDA = LDA_Chem_Conf
                 , PLSDA = PLSDA_Chem_Conf
                 , NSC = NSC_Chem_Conf
                 ,GLMPen = GLMPen_Chem_Conf) %>%
              purrr::map_df(~ .x$byClass["Class: None",metric]) %>%
              mutate(what = "Chem")) %>%
  bind_rows(list(LR = LR_BioChem_Conf
                 , LDA = LDA_BioChem_Conf
                 , PLSDA = PLSDA_BioChem_Conf
                 , NSC = NSC_BioChem_Conf
                 ,GLMPen = GLMPen_BioChem_Conf) %>%
              purrr::map_df(~ .x$byClass["Class: None",metric]) %>%
              mutate(what = "BioChem")) 
}

metric_table("Neg Pred Value") %>%
  knitr::kable(.)
