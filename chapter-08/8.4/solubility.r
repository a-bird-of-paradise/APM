library(tidyverse)
library(AppliedPredictiveModeling)
data(solubility)

output_folder <- file.path(getwd(),"8.4")

set.seed(8876)

dataset <- tibble(X = solTrainXtrans$MolWeight,
                  Y = solTrainY)

simple_tree <- rpart::rpart(Y ~ X, data = dataset)

tree_plot <- tibble(pred = predict(simple_tree, newdata = data.frame(X = solTestXtrans$MolWeight)),
       obs = solTestY) %>%
  ggplot(aes(x=pred,y=obs)) + geom_point() + geom_abline()


rf <- randomForest::randomForest(Y ~ X, dataset)

rf_plot <- tibble(pred = predict(rf, newdata = data.frame(X = solTestXtrans$MolWeight)),
       obs = solTestY) %>%
  ggplot(aes(x=pred,y=obs)) + geom_point() + geom_abline()


cubist_single <- Cubist::cubist(dataset %>% select(X),dataset$Y)
cubist_multi <- Cubist::cubist(dataset %>% select(X),dataset$Y, committees = 3)

cubist_single_noadj <- tibble(pred = predict(cubist_single, 
                                             newdata = data.frame(X = solTestXtrans$MolWeight)),
                              obs = solTestY)

cubist_single_adj <- tibble(pred = predict(cubist_single, 
                                           newdata = data.frame(X = solTestXtrans$MolWeight),
                                           neighbours = 9),
                            obs = solTestY)

cubist_multi_noadj <- tibble(pred = predict(cubist_multi, 
                                            newdata = data.frame(X = solTestXtrans$MolWeight)),
                             obs = solTestY)

cubist_multi_adj <- tibble(pred = predict(cubist_multi, 
                                          newdata = data.frame(X = solTestXtrans$MolWeight),
                                          neighbours = 9),
                           obs = solTestY)

cubist_plot <- cubist_single_adj %>%
  mutate(SA = "Single",
         AN = "Adj") %>%
  bind_rows(cubist_single_noadj %>%
              mutate(SA = "Single",
                     AN = "NoAdj")) %>%
  bind_rows(cubist_multi_adj %>%
              mutate(SA = "Multi",
                     AN = "Adj")) %>%
  bind_rows(cubist_multi_noadj %>%
              mutate(SA = "Multi",
                     AN = "NoAdj")) %>%
  ggplot(aes(x = pred, y = obs)) + facet_grid(SA ~ AN) + geom_point() + geom_abline()

ggsave(tree_plot,
       filename = file.path(output_folder,"tree.png"),
       width = 8,
       height = 6,
       dpi = 100)
ggsave(rf_plot,
       filename = file.path(output_folder,"rf.png"),
       width = 8,
       height = 6,
       dpi = 100)
ggsave(cubist_plot,
       filename = file.path(output_folder,"cubist.png"),
       width = 8,
       height = 6,
       dpi = 100)
