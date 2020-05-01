library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)

# misc headers

output_dir <- file.path(getwd(),'19.2')

data(AlzheimerDisease)

set.seed(1249)

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

#let's see if parallel works

library(doMC)
registerDoMC(4)

# flatten factors
AlzDataFull <- predictors %>% as_tibble %>% mutate(diagnosis=diagnosis)
TrainingIndex <- caret::createDataPartition(AlzDataFull$diagnosis, p = 0.9)$Resample1

AlzDataTraining <- AlzDataFull[TrainingIndex,]
AlzDataTest <- AlzDataFull[-TrainingIndex,]

index <- createMultiFolds(AlzDataFull$diagnosis[TrainingIndex], times = 5)

# control stuffs


ctrl <- rfeControl(method = "repeatedcv", repeats = 5,
                   saveDetails = TRUE,
                   index = index,
                   returnResamp = "final",
                   verbose = T)

ctrlCV <- rfeControl(method = "cv",
                   saveDetails = TRUE,
                   index = index,
                   returnResamp = "final",
                   verbose = T)

fullCtrl <- trainControl(method = "repeatedcv",
                         repeats = 5,
                         summaryFunction = fiveStats,
                         classProbs = TRUE,
                         index = index,
                         verboseIter = T)

# rfe - lda on 'full' data

ctrl$functions <- caret::ldaFuncs
ctrl$functions$summary <- fiveStats

ldaRFE <- rfe(diagnosis ~ .,
              AlzDataTraining,
              sizes = 1:(length(names(AlzDataTraining)) + 5), # flatten factor
              metric = "ROC",
              tol = 1.0e-12,
              rfeControl = ctrl)
ldaRFE

cvCtrl <- trainControl(method = "cv",
                       verboseIter = T,
                       classProbs = TRUE,
                       allowParallel = FALSE)

ctrl$functions <- caret::caretFuncs
ctrl$functions$summary <- fiveStats

ctrlCV$functions <- caret::caretFuncs
ctrlCV$functions$summary <- fiveStats

sparseLDA_full <- rfe(diagnosis ~ .,
                      AlzDataTraining,
                      #sizes = 1:(length(names(AlzDataTraining)) + 5), # flatten factor
                      rfeControl = ctrlCV,
                      metric = "ROC",
                      ## Now arguments to train() are used.
                      method = "sparseLDA",
                      tuneLength = 5,
                      trControl = cvCtrl)
sparseLDA_full

theplot<- ldaRFE$results %>% as_tibble %>% select(Variables,ROC,Sens,Spec,Kappa) %>%
  gather(key = key, value = value, -Variables) %>%
  ggplot(aes(x = Variables, y = value, colour = key)) + geom_point() + geom_line() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'bottom')

ggsave(plot = theplot,
       file = file.path(output_dir,'plot.png'),
       width = 8, height = 6, dpi = 100)
