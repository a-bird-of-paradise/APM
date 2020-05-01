library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)

# misc headers

output_dir <- file.path(getwd(),'19.1')

data(AlzheimerDisease)

set.seed(1249)

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

library(doMC)
registerDoMC(4)

# flatten factors
AlzDataFull <- predictors %>% as_tibble %>% mutate(diagnosis=diagnosis)
TrainingIndex <- caret::createDataPartition(AlzDataFull$diagnosis, p = 0.9)$Resample1

AlzDataTraining <- AlzDataFull[TrainingIndex,]
AlzDataTest <- AlzDataFull[-TrainingIndex,]

index <- createMultiFolds(AlzDataFull$diagnosis[TrainingIndex], times = 5)

all_plot <- ggcorrplot::ggcorrplot(cor(AlzDataTraining %>% 
                             select(-Genotype, -diagnosis)), 
                       hc.order = T)

ggsave(plot = all_plot,
       file = file.path(output_dir,'all_plot.png'),
       width = 8, height = 8, dpi = 100)


# identify and kill highly correlated predictors 

redundant_preds <- caret::findCorrelation(AlzDataTraining %>% 
                                            select(-Genotype,-diagnosis),
                                          cutoff = 0.9,
                                          names = T)
num_useful_preds <- AlzDataTraining %>%
  select(-diagnosis, -redundant_preds) %>%
  names %>%
  length


some_plot <- ggcorrplot::ggcorrplot(cor(AlzDataTraining %>%
                             select(-Genotype, -diagnosis, -redundant_preds)), 
                       hc.order = T)

ggsave(plot = some_plot,
       file = file.path(output_dir,'some_plot.png'),
       width = 8, height = 8, dpi = 100)


ctrl <- rfeControl(method = "repeatedcv", repeats = 5,
                   saveDetails = TRUE,
                   index = index,
                   returnResamp = "final")

fullCtrl <- trainControl(method = "repeatedcv",
                         repeats = 5,
                         summaryFunction = fiveStats,
                         classProbs = TRUE,
                         index = index)

ctrl$functions <- rfFuncs
ctrl$functions$summary <- fiveStats

rfRFE <- rfe(AlzDataTraining %>% select(-redundant_preds,-diagnosis),
             AlzDataTraining$diagnosis,
             sizes = 1:num_useful_preds,
             metric = "ROC",
             ntree = 1000,
             rfeControl = ctrl)
rfRFE

ctrl$functions <- ldaFuncs
ctrl$functions$summary <- fiveStats

ldaRFE <- rfe(diagnosis ~ .,
              AlzDataTraining %>% select(-redundant_preds),
              sizes = 1:(num_useful_preds + 5), # flatten factor
              metric = "ROC",
              tol = 1.0e-12,
              rfeControl = ctrl)
ldaRFE

ctrl$functions <- nbFuncs
ctrl$functions$summary <- fiveStats

nbRFE <- rfe(AlzDataTraining %>% select(-redundant_preds,-diagnosis),
             AlzDataTraining$diagnosis,
             sizes = 1:num_useful_preds,
             metric = "ROC",
             rfeControl = ctrl)
nbRFE

cvCtrl <- trainControl(method = "cv",
                       verboseIter = FALSE,
                       classProbs = TRUE,
                       allowParallel = FALSE) # goes as n^2 i.e. too slow

ctrl$functions <- caretFuncs
ctrl$functions$summary <- fiveStats

svmRFE <- rfe(diagnosis ~ .,
              AlzDataTraining %>% select(-redundant_preds),
              sizes = 1:(num_useful_preds + 5), # flatten factor
              rfeControl = ctrl,
              metric = "ROC", 
              ## Now arguments to train() are used.
              method = "svmRadial",
              tuneLength = 12, 
              preProc = c("center", "scale"),
              trControl = cvCtrl)
svmRFE

ctrl$functions <- lrFuncs
ctrl$functions$summary <- fiveStats

lrRFE <- rfe(diagnosis ~ .,
             AlzDataTraining %>% select(-redundant_preds),
             sizes = 1:(num_useful_preds + 5), # flatten factor
             metric = "ROC",
             rfeControl = ctrl)
lrRFE

ctrl$functions <- caretFuncs
ctrl$functions$summary <- fiveStats

knnRFE <- rfe(diagnosis ~ .,
              AlzDataTraining %>% select(-redundant_preds),
              sizes = 1:(num_useful_preds + 5), # flatten factor
              metric = "ROC",
              method = "knn",
              tuneLength = 20,
              preProc = c("center", "scale"),
              trControl = cvCtrl,
              rfeControl = ctrl)
knnRFE

RFEs <- list(knn = knnRFE,
             rf = rfRFE,
             lr = lrRFE,
             nb = nbRFE,
             svm = svmRFE,
             lda = ldaRFE)

# check nothing too crazy

RFEs %>%
  purrr::map_dbl(~ AlzDataTest %>%
                   mutate(pred = predict(.x,AlzDataTest)$pred) %>%
                   select(diagnosis,pred) %>%
                   mutate(match = ifelse(diagnosis == pred,1.0,0.0)) %>%
                   summarise(match = sum(match), n = n()) %>%
                   mutate(accuracy = match / n) %>%
                   pull(accuracy)) %>%
  enframe %>%
  knitr::kable(.)

conv_plot <- RFEs %>%
  purrr::map_df( ~ .x$results %>% select(Variables,ROC), .id='id') %>%
  ggplot(aes(x = Variables, y = ROC)) + facet_wrap(~id) + geom_point() + geom_line()

ggsave(plot = conv_plot,
       file = file.path(output_dir,'conv_plot.png'),
       width = 8, height = 6, dpi = 100)

RFEs %>%
  purrr::map_df( ~ .x$results %>% select(Variables,ROC), .id='id') %>%
  write_csv(file.path(output_dir,'answers.csv'))

lrRFE$fit
