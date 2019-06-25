library(AppliedPredictiveModeling)
library(tidyverse)
data("ChemicalManufacturingProcess")
library(doMC)
registerDoMC(8)

set.seed(1298)

ChemicalManufacturingProcess %>%
  select(-Yield) %>%
  gather(key=key,value=value) %>%
  filter(is.na(value)) %>%
  group_by(key) %>%
  summarise(n=n()) %>%
  arrange(-n)

# ok , use a nearest neighbour imputation method to fill in the gaps. 
# additionally, scale and centre predictors. 

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

# fit an elastic net model because why not? 

ENetTrain <- caret::train(TrainingPredictorsPP,
                          TrainingYield,
                          method = "enet",
                          trControl = caret::trainControl(method = "repeatedcv",
                                                          repeats = 5),
                          tuneGrid = expand.grid(lambda = 10 ** seq(-5,5,by=1),
                                                 fraction = seq(0.01,0.99,by=0.05)))

TunedFraction <- ENetTrain$bestTune$fraction
TunedLambda <- ENetTrain$bestTune$lambda

TunePlot <- ENetTrain$results %>%
  as.tibble %>%
  mutate(fraction = as.factor(fraction)) %>% 
  ggplot(aes(x=lambda,y=RMSE,colour=fraction,group=fraction)) +
  geom_point() + 
  geom_line() + 
  scale_x_log10()

ggsave(plot = TunePlot,
       filename = "6.3/tune-plot.png",
       width = 6,
       height = 4,
       dpi = 100)

caret::defaultSummary(data.frame(pred = predict(ENetTrain,TestPredictorsPP),
                                 obs = TestYield)) %>%
  bind_rows() %>%
  knitr::kable(.)

fractions <- seq(0.01, 0.99, by = 0.1)
names(fractions)<-fractions


coef_plot <- fractions %>%
  purrr::map_df(~ predict(ENetTrain$finalModel,
                          s = .x,
                          type = 'coef',
                          mode = 'fraction')[['coefficients']] %>%
                  enframe,
                .id='id') %>%
  mutate(id=as.numeric(id)) %>%
  ggplot(aes(x=id,y=value,colour=name)) + geom_point() + geom_line()

ggsave(plot = coef_plot,
       filename = "6.3/coef-plot.png",
       width = 12,
       height = 8,
       dpi = 100)

top6 <- predict(ENetTrain$finalModel,
                s = TunedFraction,
                type = 'coef',
                mode = 'fraction') %>%
  `[[`('coefficients') %>% 
  enframe %>%
  arrange(-abs(value)) %>%
  head %>%
  pull(name) 

predict(ENetTrain$finalModel,
        s = TunedFraction,
        type = 'coef',
        mode = 'fraction') %>%
  `[[`('coefficients') %>% 
  enframe %>%
  arrange(-abs(value)) %>%
  head %>%
  knitr::kable(.)

names(top6)<-top6

training_plot <- TrainingPredictors %>%
  bind_cols(pred = predict(ENetTrain,TrainingPredictorsPP),
            obs = TrainingYield) %>%
  select(top6,pred,obs) %>%
  gather(key=key,value=value,-pred,-obs) %>%
  group_by(key) %>%
  mutate(value_pct = ntile(value,10)) %>%
  ungroup %>%
  group_by(key,value_pct) %>%
  summarise_all(funs(mean)) %>%
  ungroup %>%
  gather(key=measure,value=what,-key,-value,-value_pct) %>%
  ggplot(aes(x=value_pct,y=what,colour=measure)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ key, scales = "free")

Test_Plot <- TestPredictors %>%
  bind_cols(pred = predict(ENetTrain,TestPredictorsPP),
            obs = TestYield) %>%
  select(top6,pred,obs) %>%
  gather(key=key,value=value,-pred,-obs) %>%
  group_by(key) %>%
  mutate(value_pct = ntile(value,10)) %>%
  ungroup %>%
  group_by(key,value_pct) %>%
  summarise_all(funs(mean)) %>%
  ungroup %>%
  gather(key=measure,value=what,-key,-value,-value_pct) %>%
  ggplot(aes(x=value_pct,y=what,colour=measure)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ key, scales = "free")

ggsave(plot = training_plot,
       filename = '6.3/training-plot.png',
       width = 9,
       height = 6,
       dpi = 100)

ggsave(plot = test_plot,
       filename = '6.3/test-plot.png',
       width = 9,
       height = 6,
       dpi = 100)

BaseCase <- TrainingPredictorsPP %>%
  summarise_all(funs(median))

Steps <- TrainingPredictorsPP %>%
  gather(key=key,value=value) %>%
  group_by(key) %>%
  summarise_all(funs(min,max)) %>%
  filter(key %in% top6) %>%
  mutate(step = (max - min) / 9) %>%
  crossing(i = 0:9) %>%
  mutate(value = min + i * step ) %>%
  select(key,value)

BaseLevels <- top6 %>%
  purrr::map(~ BaseCase %>%
               select(-.x) %>%
               crossing(!!.x := Steps %>%
                          filter(key == .x) %>%
                          pull(value))%>%
               rownames_to_column)

names(TrainingPredictorsPP)

pred_plot <- BaseLevels %>%
  purrr::map_df(~ .x %>% 
                  select(names(TrainingPredictorsPP),rowname) %>%
                  bind_cols(pred = predict(ENetTrain,.)),
                .id="id") %>%
  gather(key=key,value=value,-rowname,-pred,-id) %>%
  filter(key == id) %>%
  ggplot(aes(x = value, y = pred)) + geom_point() + geom_line() + facet_wrap(~ id)

ggsave(plot = pred_plot,
       filename = '6.3/pred-plot.png',
       width = 9,
       height = 6,
       dpi = 100)

TrainingPredictorsPP %>%
  gather(key=key,value=value) %>%
  group_by(key) %>%
  summarise_all(funs(min,max)) %>%
  filter(key %in% top6) %>%
  inner_join(predict(ENetTrain$finalModel,
                     s = TunedFraction,
                     type = 'coef',
                     mode = 'fraction') %>%
               `[[`('coefficients') %>% 
               enframe,
             by=c("key"="name")) %>%
  mutate(range = max-min) %>%
  arrange(value) %>%
  knitr::kable(.)
