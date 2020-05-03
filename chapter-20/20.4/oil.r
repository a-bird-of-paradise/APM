library(tidyverse)
library(caret)

data(oil)
set.seed(314)

sampleRows<-caret::createDataPartition(oilType)$Resample1

fattyAcidsTrain <- fattyAcids[sampleRows,]
fattyAcidsTest <- fattyAcids[-sampleRows,]

oilTypeTrain <- oilType[sampleRows]
oilTypeTest <- oilType[-sampleRows]

indx <- caret::createFolds(oilTypeTrain, returnTrain = T)
ctrl <- caret::trainControl(method = 'cv', index = indx, verboseIter = T)

mtryVals <- seq(1,ncol(fattyAcidsTrain))
mtryGrid <- data.frame(.mtry = mtryVals)

rfTune <- caret::train(x = fattyAcidsTrain,
                       y = oilTypeTrain,
                       method = 'rf',
                       tuneGrid = mtryGrid,
                       ntree = 1000,
                       importance = T,
                       trControl = ctrl)

ImportanceOrder <- order(rfTune$finalModel$importance[,1], decreasing = T)
top20 <- rownames(rfTune$finalModel$importance[ImportanceOrder,])[1:20] %>% 
  enframe %>%
  filter(!is.na(value)) %>%
  pull(value)

permutefattyAcidsTrain <- apply(fattyAcidsTrain,2,sample)

fattyAcidsSim <- rbind(fattyAcidsTrain, permutefattyAcidsTrain)
groupVals <- c('Training','Random')

groupY <- factor(rep(groupVals, each = nrow(fattyAcidsTrain)))

rfFatClass <- caret::train(x = fattyAcidsSim,
                           y = groupY,
                           method = 'rf',
                           tuneLength = 7, 
                           ntree = 1000,
                           control = caret::trainControl(method = 'LGOCV'))

FatTestGroupProbs <- predict(rfFatClass, fattyAcidsTest, type = 'prob')

FatTestGroupProbs %>%
  as_tibble %>%
  mutate(obs = oilTypeTest,
         pred = ifelse(Random > 0.5, 'Random','Training')) %>%
  group_by(obs,pred) %>%
  summarise(n=n()) %>%
  spread(key = pred, value =n)


pca <- prcomp(fattyAcidsTest)

trainpca <- predict(pca,fattyAcidsTrain) %>%
  as_tibble %>%
  select(PC1,PC2,PC3)

plotdata <- prcomp(fattyAcidsTest)$x %>%
  as_tibble %>%
  select(PC1,PC2,PC3) %>%
  bind_cols(FatTestGroupProbs %>%
              as_tibble %>%
              mutate(obs = oilTypeTest,
                     pred = ifelse(Random > 0.5, 'Random','Training')))

plot1 <- plotdata %>%
  ggplot(aes(x=PC1,y=PC2)) + 
  geom_point(aes(colour=pred)) +
  geom_density2d(data = trainpca)

plot2 <- plotdata %>%
  ggplot(aes(x=PC1,y=PC3)) + 
  geom_point(aes(colour=pred)) +
  geom_density2d(data = trainpca)

ggsave(plot = plot1,
       filename = file.path(getwd(),'20.4/plot1.png'),
       width = 8, height = 6, dpi = 100)
ggsave(plot = plot2,
       filename = file.path(getwd(),'20.4/plot2.png'),
       width = 8, height = 6, dpi = 100)