library(AppliedPredictiveModeling)
library(tidyverse)
data("ChemicalManufacturingProcess")

output_directory <- file.path(getwd(),"7.5")

set.seed(199882254)

# borrowing preprocessing from last time we looked at this data set

Yield <-ChemicalManufacturingProcess$Yield
Predictors <- ChemicalManufacturingProcess %>% select(-Yield)

TrainingInd <- caret::createDataPartition(Yield, p=0.7)$Resample1
TestInd <- setdiff(1:length(Yield),TrainingInd)

TestYield <- Yield[TestInd]
TrainingYield <- Yield[TrainingInd]

TestPredictors <- Predictors[TestInd,]
TrainingPredictors <- Predictors[TrainingInd,]

PreProcessor <- caret::preProcess(Predictors,
                                  method = c("center","scale","medianImpute",
                                             "BoxCox",'nzv','corr'))

TrainingPredictorsPP <- predict(PreProcessor,TrainingPredictors)
TestPredictorsPP <- predict(PreProcessor,TestPredictors)


# SVM

SVMModel <- caret::train(TrainingPredictorsPP,
                         TrainingYield,
                         method = "svmRadial",
                         tuneLength = 20,
                         trControl = caret::trainControl(method = "cv"))

# KNN 

KNNModel <- caret::train(TrainingPredictorsPP,
                         TrainingYield,
                         method = "knn",
                         tuneGrid = data.frame(.k = 1:20),
                         trControl = caret::trainControl(method = "cv"))

# NNets

NNetModel <- caret::train(TrainingPredictorsPP,
                          TrainingYield,
                          method = "avNNet",
                          tuneGrid = expand.grid(.decay = c(0.001, 0.01, 0.1),
                                                 .size = 1:3,
                                                 .bag = F),
                          trControl = caret::trainControl(method = "cv",
                                                          verboseIter = TRUE),
                          linout = T,
                          trace = F,
                          MaxNWts = 10 * (ncol(TrainingPredictorsPP) + 1) * 10+1,
                          maxIt = 50,
                          repeats = 100)

# MARS

MarsModel <- caret::train(TrainingPredictorsPP %>% as.data.frame,
                          TrainingYield,
                          method = "earth",
                          trControl = caret::trainControl(method = "cv",
                                                          verboseIter = T),
                          tuneGrid = expand.grid(.nprune = 1:40,
                                                 .degree = 1:3))

comparison_plot <- predict(SVMModel,TestPredictorsPP) %>%
  enframe %>%
  bind_cols(obs = TestYield) %>%
  mutate(what = "SVM") %>%
  select(-name) %>%
  bind_rows(predict(KNNModel,TestPredictorsPP) %>%
              enframe %>%
              bind_cols(obs = TestYield) %>%
              mutate(what = "KNN") %>%
              select(-name) )  %>%
  bind_rows(predict(MarsModel,TestPredictorsPP) %>%
              enframe %>%
              bind_cols(obs = TestYield) %>%
              mutate(what = "MARS") %>%
              select(-name) )%>%
  bind_rows(predict(NNetModel,TestPredictorsPP) %>%
              enframe %>%
              bind_cols(obs = TestYield) %>%
              mutate(what = "AvNNet") %>%
              select(-name) ) %>%
  ggplot(aes(x=value,y=obs)) + geom_point() + geom_abline() + facet_wrap(~ what)

ggsave(comparison_plot,
       filename = file.path(output_directory,"comparison.png"),
       width = 8,
       height = 6,
       dpi = 100)

postResample(predict(KNNModel,TestPredictorsPP),TestYield) %>%
  enframe %>%
  spread(key=name,value=value) %>%
  mutate(what = "KNN") %>%
  bind_rows(postResample(predict(SVMModel,TestPredictorsPP),TestYield) %>%
              enframe %>%
              spread(key=name,value=value) %>%
              mutate(what = "SVM")) %>%
  bind_rows(postResample(predict(NNetModel,TestPredictorsPP),TestYield) %>%
              enframe %>%
              spread(key=name,value=value) %>%
              mutate(what = "AvNNet")) %>%
  bind_rows(postResample(predict(MarsModel,TestPredictorsPP),TestYield) %>%
              enframe %>%
              spread(key=name,value=value) %>%
              mutate(what = "Mars")) %>%
  knitr::kable(.)

caret::varImp(MarsModel)$importance %>% rownames_to_column %>% mutate(what = "MARS") %>%
  bind_rows(caret::varImp(NNetModel)$importance %>% rownames_to_column %>% mutate(what = "AvNNet") ) %>%
  bind_rows(caret::varImp(KNNModel)$importance %>% rownames_to_column %>% mutate(what = "KNN") ) %>%
  bind_rows(caret::varImp(SVMModel)$importance %>% rownames_to_column %>% mutate(what = "SVM") ) %>%
  spread(key=what,value=Overall) %>%
  arrange(-MARS) %>%
  head(10) %>%
  knitr::kable(.)

