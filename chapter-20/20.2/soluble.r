library(tidyverse)
library(caret)

output_dir <- file.path(getwd(),'20.2')

set.seed(98163)

# load data and bin outcomes

library(AppliedPredictiveModeling)
data(solubility)

trainData <- solTrainXtrans
lowcut <- mean(solTrainY) - sd(solTrainY)
highcut <- lowcut + 2* sd(solTrainY)

breakpoints <- c(min(solTrainY),
                 lowcut,
                 highcut,
                 max(solTrainY))

groupNames <- c('Insoluble','Indeterminate','Soluble')

solTrainYBin <- cut(solTrainY, breaks = breakpoints, include.lowest = T, labels = groupNames)
solTestYBin <- cut(solTestY, breaks = breakpoints, include.lowest = T, labels = groupNames)

IndexBin <- caret::createFolds(solTrainYBin, returnTrain = T)

ctrlBin <- caret::trainControl(method = 'cv',
                               index = IndexBin,
                               classProbs = T,
                               savePredictions = T,
                               verboseIter = T,
                               summaryFunction = caret::multiClassSummary)

lda <- caret::train(x = solTrainXtrans,
                    y = solTrainYBin,
                    method = 'lda',
                    metric = 'Mean_Balanced_Accuracy',
                    trControl = ctrlBin,
                    preProcess = c('nzv','corr'),
                    tuneLength = 10)

svm <- caret::train(x = solTrainXtrans,
                    y = solTrainYBin,
                    method = 'svmRadial',
                    metric = 'Mean_Balanced_Accuracy',
                    trControl = ctrlBin,
                    preProcess = c('nzv','center','scale','corr'))

rpart <- caret::train(x = solTrainXtrans,
                      y = solTrainYBin,
                      method = 'rpart',
                      metric = 'Mean_Balanced_Accuracy',
                      tuneLength = 30,
                      trControl = ctrlBin)

models.3bin <- list(lda = lda, svm = svm, rpart = rpart)

models.3bin %>%
  purrr::map_dbl(~ caret::multiClassSummary(data.frame(obs = solTestYBin,
                                                       pred = predict(.x,solTestXtrans)),
                                            lev = levels(solTestYBin))[['Mean_Balanced_Accuracy']]) %>%
  enframe %>%
  knitr::kable(.)


# 2bin versions 

filtered_train_index <- which(solTrainYBin %in% c('Soluble','Insoluble'))
filtered_test_index <- which(solTestYBin %in% c('Soluble','Insoluble'))

filtered_solTrainX <- solTrainX[filtered_train_index,]
filtered_solTrainY <- solTrainYBin[filtered_train_index] %>% fct_drop
filtered_solTestX <- solTestX[filtered_test_index,]
filtered_solTestY <- solTestYBin[filtered_test_index] %>% fct_drop


IndexBin2 <- caret::createFolds(filtered_solTrainY, returnTrain = T)

ctrlBin2 <- caret::trainControl(method = 'cv',
                                index = IndexBin2,
                                classProbs = T,
                                savePredictions = T,
                                verboseIter = T,
                                summaryFunction = caret::multiClassSummary)


lda2 <- caret::train(x = filtered_solTrainX,
                     y = filtered_solTrainY,
                     method = 'lda',
                     metric = 'Balanced_Accuracy',
                     trControl = ctrlBin2,
                     preProcess = c('nzv','corr'))

svm2 <- caret::train(x = filtered_solTrainX,
                     y = filtered_solTrainY,
                     method = 'svmRadial',
                     metric = 'Balanced_Accuracy',
                     trControl = ctrlBin2,
                     preProcess = c('nzv','center','scale','corr'))

rpart2 <- caret::train(x = filtered_solTrainX,
                       y = filtered_solTrainY,
                       method = 'rpart',
                       metric = 'Balanced_Accuracy',
                       tuneLength = 30,
                       trControl = ctrlBin2)

models.2bin <- list(lda = lda2, svm = svm2, rpart = rpart2)

models.2bin %>%
  purrr::map_dbl(~ caret::multiClassSummary(data.frame(pred = predict(.x,filtered_solTestX),
                                                       obs = filtered_solTestY),
                                            lev = levels(filtered_solTestY))[['Balanced_Accuracy']]) %>%
  enframe %>%
  mutate(bin = 2) %>%
  bind_rows(models.3bin %>%
              purrr::map_dbl(~ caret::multiClassSummary(data.frame(obs = solTestYBin,
                                                                   pred = predict(.x,solTestXtrans)),
                                                        lev = levels(solTestYBin))[['Mean_Balanced_Accuracy']]) %>%
              enframe  %>%
              mutate(bin = 3)) %>%
  spread(key = bin, value = value) %>%
  knitr::kable(.)

models.3bin %>%
  purrr::map_df(~ caret::confusionMatrix(predict(.x,solTestXtrans),solTestYBin)$byClass %>%
                  as.data.frame %>% 
                  rownames_to_column %>%
                  mutate(class = str_match(rowname, 'Class: (.*)')[,2]) %>%
                  select(class,Sensitivity,Specificity) %>%
                  filter(class %in% c('Soluble','Insoluble')),
                .id='id') %>%
  knitr::kable(.)

models.2bin %>%
  purrr::map_df(~ caret::confusionMatrix(predict(.x,filtered_solTestX),
                                         filtered_solTestY)$byClass %>%
                  enframe,
                .id='id') %>%
  filter(name %in% c('Sensitivity','Specificity')) %>%
  spread(key = name, value =value) %>%
  knitr::kable(.)

the_plot <- filtered_solTestX %>%
  mutate(obs = filtered_solTestY,
         rowid = row_number()) %>%
  modelr::add_predictions(lda,type='prob') %>%
  mutate(bin3.pred = ifelse(obs == 'Soluble', pred$Soluble, pred$Insoluble) / 
           (pred$Soluble+pred$Insoluble+pred$Indeterminate)) %>%
  select(-pred) %>%
  modelr::add_predictions(lda2,type = 'prob')  %>%
  mutate(bin2.pred = ifelse(obs == 'Soluble', pred$Soluble, pred$Insoluble)) %>%
  select(-pred) %>%
  select(rowid,obs,bin2.pred,bin3.pred) %>%
  mutate(model = 'lda') %>%
  bind_rows(filtered_solTestX %>%
              mutate(obs = filtered_solTestY,
                     rowid = row_number()) %>%
              modelr::add_predictions(rpart,type='prob') %>%
              mutate(bin3.pred = ifelse(obs == 'Soluble', pred$Soluble, pred$Insoluble)) %>%
              select(-pred) %>%
              modelr::add_predictions(rpart2,type = 'prob')  %>%
              mutate(bin2.pred = ifelse(obs == 'Soluble', pred$Soluble, pred$Insoluble)) %>%
              select(-pred) %>%
              select(rowid,obs,bin2.pred,bin3.pred) %>%
              mutate(model = 'rpart')) %>%
  bind_rows(tibble(bin3.pred = filtered_solTestX %>%
                     modelr::add_predictions(svm,type='prob') %>%
                     mutate(obs = filtered_solTestY) %>%
                     mutate(bin3.pred = ifelse(obs == 'Soluble',pred$Soluble,pred$Insoluble)) %>%
                     pull(bin3.pred),
                   bin2.pred = filtered_solTestX %>%
                     modelr::add_predictions(svm2,type='prob') %>%
                     mutate(obs = filtered_solTestY) %>%
                     mutate(bin2.pred = ifelse(obs == 'Soluble',pred$Soluble,pred$Insoluble)) %>%
                     pull(bin2.pred),
                   obs = filtered_solTestY,
                   model = 'svm')) %>%
  ggplot(aes(x=bin2.pred, y = bin3.pred)) + 
  facet_grid(model ~ obs) +
  geom_jitter(width=0.02,height=0.02,alpha=0.5,size=1) +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent)

ggsave(plot = the_plot,
       filename = file.path(output_dir,'plot.png'),
       width = 8, height = 6, dpi = 100)
