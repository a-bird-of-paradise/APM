library(tidyverse)
library(caret)
library(ggcorrplot)
data(BloodBrain)

bbbDescr %>%
  as.tibble %>%
  count

bbbDescr %>%
  as.tibble %>%
  complete.cases

# no NAs... any low power predictors?

LowVarPredictors <- names(bbbDescr)[caret::nearZeroVar(bbbDescr)]

corrs <- bbbDescr %>%
  as.tibble %>%
  cor(method = "spearman") 

ggcorrplot(corrs)

RedundantPreds <- names(bbbDescr)[caret::findCorrelation(corrs,cutoff = 0.75)]

remove.preds <- union(LowVarPredictors,RedundantPreds)

bbbDescr %>%
  as.tibble %>%
  select(-remove.preds) %>%
  cor(method = "spearman") %>%
  ggcorrplot

# ok try PCA...? 

BBBdf <- bbbDescr %>% 
  as.tibble %>%
  select(-remove.preds) %>%
  as.data.frame

PCAObj <- caret::preProcess(x = BBBdf,
                  method = c("pca"))

data_set <- tibble(logBBB = logBBB) %>%
  bind_cols(predict(PCAObj,newdata = BBBdf))

# so end up with 29 predictors from original set of 134
# this was after:
# removing near zero variance predictors
# removing very highly correlated predictors 
# using PCA to compress the sample space 

zzz <- 20

lm_mod <- lm(logBBB ~ PC1 + PC2 + PC3,data_set)

data_set %>%
  as.tibble %>%
  bind_cols(tibble(pred = predict(lm_mod,
        new_data = data_set))) %>%
  select(logBBB,pred,!!sym(paste0("PC",zzz))) %>%
  gather(key=key,value=value,-!!sym(paste0("PC",zzz))) %>%
  ggplot(aes(x = !!sym(paste0("PC",zzz)),
             y = value,
             colour = key)) +
  geom_point()

broom::tidy(lm_mod)
summary(lm_mod)

broom::augment(lm_mod)
broom::glance(lm_mod)

