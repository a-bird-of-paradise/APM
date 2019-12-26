library(tidyverse)
library(caret)
data(oil)

output_directory <- file.path(getwd(),"12.2")

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

PLSDA <- caret::train(TrainingPreds,
                      TrainingObs,
                      method = "pls",
                      tuneGrid = expand.grid(.ncomp = 1:8),
                      preProc = c("center","scale"),
                      metric = "Accuracy",
                      trControl = ctrl)

GLM <- caret::train(TrainingPreds,
                    TrainingObs,
                    method = "glmnet",
                    tuneGrid = expand.grid(.alpha = c(0,.1,.2,.4,.6,.8,1.0),
                                           .lambda = seq(.01,2,length = 40)),
                    preProc = c("center","scale"),
                    trControl = ctrl,
                    metric = "Accuracy")

NSC <- caret::train(x = TrainingPreds,
                    y = TrainingObs,
                    method = 'pam',
                    tuneGrid = expand.grid(.threshold = 1:25),
                    preProc = c('center','scale'),
                    metric = 'Accuracy')

# Now we have the models, form the predictions on the test set 

models <- list(
  NSC = NSC,
  GLM = GLM,
  LDA = LDA,
  PLSDA = PLSDA,
  LinReg = LinReg)

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

# and what is the scoring like? 

summary_data %>%
  mutate(match = ifelse(match,1.0,0.0)) %>%
  group_by(Model,Obs) %>%
  summarise(group_score = sum(match)/n()) %>%
  summarise(Score = mean(group_score)) %>%
  knitr::kable(.)
