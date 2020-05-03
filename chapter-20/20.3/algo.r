library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)

data(solubility)

set.seed(8761)

indx <- caret::createFolds(solTrainY, returnTrain = T)

ctrl <- caret::trainControl(method = 'cv', index = indx, verboseIter = T)

mtryVals <- floor(seq(10, ncol(solTrainXtrans), length = 10))
mtryGrid <- data.frame(.mtry = mtryVals)

rfTune <- caret::train(x=solTrainXtrans,
                       y=solTrainY,
                       method = 'rf',
                       tuneGrid = mtryGrid,
                       ntree = 1000,
                       importance = T,
                       trControl = ctrl)

ImportanceOrder <- order(rfTune$finalModel$importance[,1], decreasing = T)

top20 <- rownames(rfTune$finalModel$importance[ImportanceOrder,])[1:20]

solTrainXimp <- solTrainX %>% select(top20)
solTestXimp <- solTestX %>% select(top20)

permutesolTrainXimp <- apply(solTrainXimp, 2, sample)

solSimX <- rbind(solTrainXimp, permutesolTrainXimp)

groupVals <- c('Training','Random')

groupY <- factor(rep(groupVals, each = nrow(solTrainX)))

rfSolClass <- caret::train(x = solSimX,
                           y = groupY,
                           method = 'rf',
                           tuneLength = 5,
                           trees = 1000,
                           control = caret::trainControl(method = 'LGOCV',
                                                         verboseIter = T))

solTestGroupProbs <- predict(rfSolClass, solTestXimp, type = 'prob')

TestPlot <- solTestGroupProbs %>%
  ggplot(aes(x = Training)) + geom_density(adjust = 0.2) +
  scale_x_continuous(labels = scales::percent, 
                     name = 'Probability in training',
                     limits = c(0,1)) +
  ggtitle('Probabilty test set elements in training set')

ggsave(plot = TestPlot,
       filename = file.path(getwd(),'20.3/plot.png'),
       width = 8, height = 6, dpi = 100)  
