library(AppliedPredictiveModeling)
data(permeability)

library(tidyverse)
library(caret)

fingerprints_NoNZV <- fingerprints[,-caret::nearZeroVar(fingerprints)]

ncol(fingerprints)
ncol(fingerprints_NoNZV)

# 388 variables left with meaningful variance 

set.seed(8)

TrainingInd <- caret::createDataPartition(permeability, p = 0.7)[[1]]
TestInd <- setdiff(1:length(permeability), TrainingInd)

TrainingFingerprints <- fingerprints_NoNZV[TrainingInd,]
TrainingPermeability <- permeability[TrainingInd]

TestingFingerprints <- fingerprints_NoNZV[TestInd,]
TestingPermeability <- permeability[TestInd]

TrainingController <- caret::trainControl(method = "cv",
                                          number = 5)

PLSTrain <- caret::train(x = TrainingFingerprints,
                         y = TrainingPermeability,
                         method = 'pls',
                         tuneLength = 50,
                         trControl = TrainingController#,
                         #preProc = c('center','scale')
                         )

PLS_ncomp <- PLSTrain$results %>%
  as.tibble %>%
  filter(RMSE == min(RMSE)) %>%
  pull(ncomp)

PLSResults <- PLSTrain$results %>%
  as.tibble %>%
  filter(ncomp == PLS_ncomp) %>%
  select(RMSE, Rsquared, MAE) %>%
  mutate(what = "Training") %>%
  bind_rows(
    predict(PLSTrain,TestingFingerprints) %>%
      as.tibble %>%
      mutate(obs = TestingPermeability) %>%
      rename(pred = value) %>%
      as.data.frame %>%
      caret::defaultSummary(.) %>%
      bind_rows %>%
      mutate(what = "Testing"))


predict(PLSTrain,TestingFingerprints) %>%
  as.tibble %>%
  mutate(obs = TestingPermeability) %>%
  ggplot(aes(x=obs,y=value)) + geom_point() + geom_abline()

# ok this isn't too good - negative predictions are unphysical, dispersion very bad.
# try some other models: 

RidgeTrain <- caret::train(x = TrainingFingerprints,
                           y = TrainingPermeability,
                           method = 'ridge',
                           tuneGrid = data.frame(lambda = 10 ** seq(-2,0,0.1)),
                           trControl = TrainingController#,
                           #preProc = c('center','scale')
                           )

RidgeTrain$results %>%
  as.tibble %>%
  ggplot(aes(x=lambda,y=RMSE)) + geom_point()

RidgeLambda <- RidgeTrain$results %>%
  as.tibble %>%
  filter(RMSE == min(RMSE)) %>%
  pull(lambda)

RidgeResults <- RidgeTrain$results %>%
  as.tibble %>%
  filter(lambda == RidgeLambda) %>%
  select(RMSE, Rsquared, MAE) %>%
  mutate(what = "Training") %>%
  bind_rows(
    predict(RidgeTrain,TestingFingerprints) %>%
      as.tibble %>%
      mutate(obs = TestingPermeability) %>%
      rename(pred = value) %>%
      as.data.frame %>%
      caret::defaultSummary(.) %>%
      bind_rows %>%
      mutate(what = "Testing"))

predict(RidgeTrain,TestingFingerprints) %>%
  as.tibble %>%
  mutate(obs = TestingPermeability) %>%
  ggplot(aes(x=obs,y=value)) + geom_point() + geom_abline()

# better, but still poor. what about lasso? 

LassoTrain <- caret::train(x = TrainingFingerprints,
                           y = TrainingPermeability,
                           method = 'glmnet',
                           tuneGrid = expand.grid(alpha = 1, 
                                                  lambda = 10 ** seq(-2,2,by=0.1)),
                           trControl = TrainingController,
                           preProc = c('center','scale')
                           )

LassoTrain$results %>%
  as.tibble %>%
  ggplot(aes(x=lambda,y=RMSE)) + geom_point() + scale_x_log10()

LassoFraction <- LassoTrain$results %>%
  as.tibble %>%
  filter(RMSE == min(RMSE)) %>%
  pull(lambda)

LassoResults <- LassoTrain$results %>%
  as.tibble %>%
  filter(lambda == LassoFraction) %>%
  select(RMSE, Rsquared, MAE) %>%
  mutate(what = "Training") %>%
  bind_rows(
    predict(LassoTrain,TestingFingerprints) %>%
      as.tibble %>%
      mutate(obs = TestingPermeability) %>%
      rename(pred = value) %>%
      as.data.frame %>%
      caret::defaultSummary(.) %>%
      bind_rows %>%
      mutate(what = "Testing"))

predict(LassoTrain,TestingFingerprints) %>%
  as.tibble %>%
  mutate(obs = TestingPermeability) %>%
  ggplot(aes(x=obs,y=value)) + geom_point() + geom_abline()

# a bit better but still mince, really. try elastic net last. Otherwise could simply 
# not have an adequate model here 

EnetTrain <- caret::train(x = TrainingFingerprints,
                           y = TrainingPermeability,
                           method = 'enet',
                           tuneGrid = expand.grid(lambda = 10 ** seq(-1,0,by=0.1),
                                                 fraction = seq(0.01,0.2,by=0.01)),
                           trControl = TrainingController,
                           preProc = c('center','scale'))

EnetTrain$results %>% 
  as.tibble %>%
  ggplot(aes(x=lambda,y=RMSE,colour=fraction,group=fraction)) + geom_point() + geom_line()

EnetParam <- EnetTrain$results %>%
  filter(RMSE == min(RMSE)) %>%
  select(lambda,fraction)

ENetResults <- EnetTrain$results %>%
  as.tibble %>%
  inner_join(EnetParam,
             by = c("lambda","fraction")) %>%
  select(RMSE, Rsquared, MAE) %>%
  mutate(what = "Training") %>%
  bind_rows(
    predict(EnetTrain,TestingFingerprints) %>%
      as.tibble %>%
      mutate(obs = TestingPermeability) %>%
      rename(pred = value) %>%
      as.data.frame %>%
      caret::defaultSummary(.) %>%
      bind_rows %>%
      mutate(what = "Testing"))

predict(EnetTrain,TestingFingerprints) %>%
  as.tibble %>%
  mutate(obs = TestingPermeability) %>%
  ggplot(aes(x=obs,y=value)) + geom_point() + geom_abline()

# none of these are actually any good, so do not propose for production use. 

carets <- list(PLS = PLSTrain,
               ridge = RidgeTrain,
               lasso = LassoTrain,
               ENet = EnetTrain)

prediction_plot <- carets %>%
  purrr::map_df(~ predict(.x, TestingFingerprints)) %>%
  mutate(obs = TestingPermeability) %>%
  gather(key=model,value=pred,-obs) %>%
  ggplot(aes(x=obs,y=pred)) + geom_point() + geom_abline() + facet_wrap(~ model) 

prediction_plot_log <- prediction_plot + scale_x_log10() + scale_y_log10()

results <- list(Ridge = RidgeResults,
                PLS = PLSResults,
                lasso = LassoResults,
                ENet = ENetResults) %>%
  bind_rows(.id="model")

knitr::kable(results)

compare_plot <- results %>%
  gather(key=metric,value=value,-model,-what) %>%
  ggplot(aes(x = value, y = model, colour = what)) + 
  facet_wrap(~ metric, ncol = 1) + 
  geom_point()

ggsave(plot = prediction_plot,
       filename = file.path("6.2", "prediction_plot.png"),
       width = 6,
       height = 4,
       dpi = 100)

ggsave(plot = prediction_plot_log,
       filename = file.path("6.2", "prediction_plot_log.png"),
       width = 6,
       height = 4,
       dpi = 100)

ggsave(plot = compare_plot,
       filename = file.path("6.2", "compare_plot.png"),
       width = 6,
       height = 4,
       dpi = 100)

