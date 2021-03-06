library(tidyverse)
library(caret)
library(mlbench)

set.seed(555321)

simulated <- mlbench.friedman1(200, sd = 1)
simulated <- cbind(simulated$x, simulated$y) %>% as.tibble
colnames(simulated)[ncol(simulated)] <- "y"

rfModel <- randomForest::randomForest(y ~ .,
                                      data = simulated,
                                      importance = T,
                                      ntree = 1000)

caret::varImp(rfModel, scale = F) %>%
  rownames_to_column("variable") %>%
  dplyr::rename("Importance" = "Overall") %>%
  knitr::kable(.)

simulated$V11 <- simulated$V1 + rnorm(200)* 0.1

cor(simulated %>% select(V1, V11), 
    method = "spearman")

rfModel2 <- randomForest::randomForest(y ~ .,
                                       data = simulated,
                                       importance = T,
                                       ntree = 1000)

caret::varImp(rfModel2, scale = F) %>%
  rownames_to_column("variable") %>%
  dplyr::rename("Importance" = "Overall") %>%
  knitr::kable(.)simulated$V11 <- simulated$V1 + rnorm(200)* 0.1


simulated$V12 <- simulated$V1 + rnorm(200)* 0.1

cor(simulated %>% select(V1, V11, V12), 
    method = "spearman")

rfModel3 <- randomForest::randomForest(y ~ .,
                                       data = simulated,
                                       importance = T,
                                       ntree = 1000)

caret::varImp(rfModel3, scale = F) %>%
  rownames_to_column("variable") %>%
  dplyr::rename("Importance" = "Overall") %>%
  knitr::kable(.)

list(RF1 = rfModel,
     RF2 = rfModel2,
     RF3 = rfModel3) %>%
  purrr::map_df(~ caret::varImp(.x, scale = T) %>%
                  rownames_to_column("name") %>%
                  dplyr::rename("Importance" = "Overall"),
                .id="id") %>%
  mutate(name = stringr::str_extract(name, '[0-9]+') %>%
           as.integer) %>%
  spread(key=id,value=Importance) %>%
  knitr::kable(.)

cforestModel1 <- party::cforest(y ~ .,
                                data = simulated %>% select(-V11, -V12),
                                controls = party::cforest_control(ntree = 1000))

cforestModel2 <- party::cforest(y ~ .,
                                data = simulated %>% select(-V12),
                                controls = party::cforest_control(ntree = 1000))

cforestModel3 <- party::cforest(y ~ .,
                                data = simulated,
                                controls = party::cforest_control(ntree = 1000))

list(CF1 = cforestModel1,
     CF2 = cforestModel2,
     CF3 = cforestModel3) %>%
  purrr::map_df(~ party::varimp(.x) %>% enframe,
                .id = "id") %>%
  spread(key = id, value = value) %>%
  mutate(name = stringr::str_extract(name, '[0-9]+') %>%
           as.integer) %>%
  arrange(name) %>%
  knitr::kable(.)

list(CF1 = cforestModel1,
     CF2 = cforestModel2,
     CF3 = cforestModel3) %>%
  purrr::map_df(~ party::varimp(.x, conditional = T) %>% enframe,
                .id = "id") %>%
  spread(key = id, value = value) %>%
  mutate(name = stringr::str_extract(name, '[0-9]+') %>%
           as.integer) %>%
  arrange(name) %>%
  knitr::kable(.)

# boosted tree

library(gbm)

gbmModel1 <- gbm::gbm(y ~ .,
                      data = simulated %>% select(-V11,-V12),
                      distribution = "gaussian",
                      n.trees = 100)

gbmModel2 <- gbm::gbm(y ~ .,
                      data = simulated %>% select(-V12),
                      distribution = "gaussian")

gbmModel3 <- gbm::gbm(y ~ .,
                      data = simulated,
                      distribution = "gaussian")

list(GB1 = gbmModel1,
     GB2 = gbmModel2,
     GB3 = gbmModel3) %>%
  purrr::map_df(~ caret::varImp(.x, numTrees = 100, scale = T) %>% 
                  rownames_to_column("name"),
                .id = "id") %>%
  spread(key = id, value = Overall) %>%
  mutate(name = stringr::str_extract(name, '[0-9]+') %>%
           as.integer) %>%
  arrange(name) %>%
  knitr::kable(.)

# cubist 

library(Cubist)

cubModel1 <- Cubist::cubist( simulated %>% select(-y, -V11, -V12),
                             simulated$y)
cubModel2 <- Cubist::cubist( simulated %>% select(-y, -V12),
                             simulated$y)
cubModel3 <- Cubist::cubist( simulated %>% select(-y),
                             simulated$y)

list(GB1 = cubModel1,
     GB2 = cubModel2,
     GB3 = cubModel3) %>%
  purrr::map_df(~ caret::varImp(.x) %>% rownames_to_column("name"),
                .id = "id") %>%
  spread(key = id, value = Overall) %>%
  mutate(name = stringr::str_extract(name, '[0-9]+') %>%
           as.integer) %>%
  arrange(name) %>%
  knitr::kable(.)

# how do the models think they did? 
