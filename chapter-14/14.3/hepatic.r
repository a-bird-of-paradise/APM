library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
data(hepatic)

set.seed(714)

indx <- caret::createFolds(injury,returnTrain = T)
ctrl <- caret::trainControl(method = 'cv', index = indx)
mtry <- c(5,10,25,50,75,100)

rfCART <- caret::train(chem, injury,
                       method = 'rf',
                       metric = 'Kappa',
                       ntree = 1000,
                       tuneGrid = expand.grid(.mtry = mtry))

rfCForest <- caret::train(chem, injury,
                          method = 'cforest',
                          metric = 'Kappa',
                          tuneGrid = expand.grid(.mtry = mtry))

rfCART$results %>%
  mutate(what = 'CART') %>%
  bind_rows(rfCForest$results %>% mutate(what = 'CF')) %>%
  group_by(what) %>%
  filter(Kappa==max(Kappa)) %>%
  knitr::kable(.)

rfCART$time$everything['elapsed']
rfCForest$times$everything['elapsed']

vcart <- varImp(rfCART)
vcf <- varImp(rfCForest)

t20 <- vcart$importance %>%
  rownames_to_column %>%
  as_tibble %>%
  mutate(what = 'CART') %>%
  bind_rows(vcf$importance %>%
              rownames_to_column %>%
              as_tibble %>%
              mutate(what = 'CF')) %>%
  group_by(what) %>%
  mutate(rank = rank(-Overall, ties.method = 'first')) %>%
  filter(rank <= 20) %>%
  select(-Overall) %>%
  spread(key = what, value = rowname)

setdiff(t20$CART,t20$CF)
setdiff(t20$CF,t20$CART)
intersect(t20$CART,t20$CF)

rfp <- vcart$importance %>%
  rownames_to_column %>%
  as_tibble %>%
  mutate(what = 'CART') %>%
  bind_rows(vcf$importance %>%
              rownames_to_column %>%
              as_tibble %>%
              mutate(what = 'CF')) %>%
  spread(key = what, value = Overall) %>%
  ggplot(aes(x = CART, y = CF)) + geom_label(aes(label=rowname))

ggsave(plot = rfp,
       filename = file.path('14.3/compare.png'),
       width = 12,
       height = 8,
       dpi = 100)
