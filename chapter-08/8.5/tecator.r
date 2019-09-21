library(tidyverse)
library(caret)

data(tecator)

set.seed(1817161514)

output_folder <- file.path(getwd(),"8.5")

absorp_preds <- absorp %>%
  as.tibble

fats <- endpoints[,2] %>% as.vector

training_indices <- caret::createDataPartition(fats,p = 0.7)[[1]]

training_preds <- absorp_preds[training_indices,]
training_obs <- fats[training_indices]

testing_preds <- absorp_preds[-training_indices,]
testing_obs <- fats[-training_indices]

# simple tree 

simple_tree_model <- caret::train(training_preds,
                                  training_obs,
                                  method = "rpart",
                                  tuneLength = 10,
                                  trControl = caret::trainControl(method = "cv"))

# bagged tree

bagged_tree_model <- caret::train(training_preds,
                                  training_obs,
                                  method = "treebag",
                                  trControl = caret::trainControl(method = "cv"))

# random forest

random_forest_model <- caret::train(training_preds,
                                    training_obs,
                                    method = "rf",
                                    tuneLength = 10,
                                    trControl = caret::trainControl(method = "cv",
                                                                    verboseIter = T))

# gradient boosted tree 

grad_boosted_tree_model <- caret::train(training_preds,
                                        training_obs,
                                        method = "gbm",
                                        tuneGrid = expand.grid(.interaction.depth = 
                                                                 seq(1, 7, by = 2),
                                                               .n.trees = seq(100,1000,by=50),
                                                               .shrinkage = c(0.01,0.1),
                                                               .n.minobsinnode = 10),
                                        trControl = caret::trainControl(method = "cv",
                                                                        verboseIter = T),
                                        verbose = F)

# cubist 

cubist_model <- caret::train(training_preds,
                             training_obs,
                             method = "cubist",
                             trControl = caret::trainControl(method = "cv",
                                                             verboseIter = T),
                             tuneGrid = expand.grid(.committees = c(1,10,20,50),
                                                    .neighbors = c(0,5,9)))

models <- list(simple_tree = simple_tree_model,
               bagged_tree = bagged_tree_model,
               random_forest = random_forest_model,
               grad_boosted_tree = grad_boosted_tree_model,
               cubist = cubist_model)

pred_plot <- models %>%
  purrr::map_df(predict,
             newdata = testing_preds) %>%
  bind_cols(list(obs = testing_obs)) %>%
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
