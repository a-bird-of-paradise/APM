library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)

data("ChemicalManufacturingProcess")

output_folder <- file.path(getwd(),"8.7")

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


# simple tree 

simple_tree_model <- caret::train(TrainingPredictorsPP,
                                  TrainingYield,
                                  method = "rpart",
                                  tuneLength = 10,
                                  trControl = caret::trainControl(method = "cv"))

# bagged tree

bagged_tree_model <- caret::train(TrainingPredictorsPP,
                                  TrainingYield,
                                  method = "treebag",
                                  trControl = caret::trainControl(method = "cv",
                                                                  verboseIter = T))

# random forest

random_forest_model <- caret::train(TrainingPredictorsPP,
                                    TrainingYield,
                                    method = "rf",
                                    tuneLength = 10,
                                    trControl = caret::trainControl(method = "cv",
                                                                    verboseIter = T))

# gradient boosted tree 

grad_boosted_tree_model <- caret::train(TrainingPredictorsPP,
                                        TrainingYield,
                                        method = "gbm",
                                        tuneGrid = expand.grid(.interaction.depth = 
                                                                 seq(1, 11, by = 2),
                                                               .n.trees = seq(100,2000,by=50),
                                                               .shrinkage = c(0.01,0.1),
                                                               .n.minobsinnode = 10),
                                        trControl = caret::trainControl(method = "cv",
                                                                        verboseIter = T),
                                        verbose = F)

# cubist 

cubist_model <- caret::train(TrainingPredictorsPP,
                             TrainingYield,
                             method = "cubist",
                             trControl = caret::trainControl(method = "cv",
                                                             verboseIter = T),
                             tuneGrid = expand.grid(.committees = c(1,10,20,50,100),
                                                    .neighbors = c(0,5,9)))

models <- list(simple_tree = simple_tree_model,
               bagged_tree = bagged_tree_model,
               random_forest = random_forest_model,
               grad_boosted_tree = grad_boosted_tree_model,
               cubist = cubist_model)

pred_plot <- models %>%
  purrr::map_df(predict,
                newdata = TestPredictorsPP) %>%
  bind_cols(list(obs = TestYield)) %>%
  gather(key=key,value=pred,-obs) %>%
  ggplot(aes(x=pred,y=obs)) + geom_point() + geom_abline() + facet_wrap(~key)

rmse_plot <- resamples(models)$values %>%
  gather(key=key,value=RMSE,-Resample) %>%
  separate(key, into = c("model","measure"), sep = "~") %>%
  filter(measure == "RMSE") %>%
  ggplot(aes(x=RMSE,y=model)) + geom_boxplot()

ggsave(pred_plot,
       filename = file.path(output_folder,"pred_plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

ggsave(rmse_plot,
       filename = file.path(output_folder,"rmse_plot.png"),
       width = 8,
       height = 6,
       dpi = 100)

caret::varImp(cubist_model)$importance %>%
  rownames_to_column("Variable") %>%
  arrange(desc(Overall)) %>%
  head(n = 10)  %>%
  knitr::kable(.)

png(filename = file.path(output_folder,"tree_plot.png"),
    width = 800,
    height = 600)
simple_tree_model$finalModel %>%
  partykit::as.party(.) %>%
  plot
dev.off()

