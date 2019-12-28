library(tidyverse)
library(caret)
library(C50)
data(churn)

output_directory <- file.path(getwd(),'14.2')
dir.create(output_directory,showWarnings = F)

set.seed(1988772)

# add extra predictors 

churnTrain.extra <- churnTrain %>%
  mutate(TotalCharge = total_day_charge + total_eve_charge + total_night_charge + total_intl_charge,
         HighCharge = as.factor(ifelse(TotalCharge >= 75, 'yes','no')),
         ManyCalls = as.factor(ifelse(number_customer_service_calls >= 3, 'yes','no')),
         total_day_charge_shift = pmax(0,total_day_charge-40),
         total_day_minutes_shift = pmax(0, total_day_minutes - 200),
         total_night_minutes_capped = pmin(total_night_minutes, 200)) %>%
  select(-state)

churnTest.extra <- churnTest %>%
  mutate(TotalCharge = total_day_charge + total_eve_charge + total_night_charge + total_intl_charge,
         HighCharge = as.factor(ifelse(TotalCharge >= 75, 'yes','no')),
         ManyCalls = as.factor(ifelse(number_customer_service_calls >= 3, 'yes','no')),
         total_day_charge_shift = pmax(0,total_day_charge-40),
         total_day_minutes_shift = pmax(0, total_day_minutes - 200),
         total_night_minutes_capped = pmin(total_night_minutes, 200)) %>%
  select(-state)

# use  dummy variables to get rid of factor issues 

churnTrain.dum <- dummyVars(churn ~ ., churnTrain.extra)
churnTest.dum <- dummyVars(churn ~ ., churnTest.extra)

TrainingPreds <- churnTrain.extra %>% select(-churn)
TrainingObs <- churnTrain.extra %>% pull(churn)
TestingPreds <- churnTest.extra %>% select(-churn)
TestingObs <- churnTest.extra %>% pull(churn)
TrainingPreds.dum <- predict(churnTrain.dum,
                             newdata = churnTrain.extra) 
TestingPreds.dum <- predict(churnTest.dum,
                            newdata = churnTest.extra) 

ctrl <- caret::trainControl(method = "cv",
                            classProbs = TRUE,
                            summaryFunction = caret::twoClassSummary)

# some trees. plain, C4.5, and C5.0

CART.Grouped <- caret::train(TrainingPreds,
                             TrainingObs,
                             method = 'rpart',
                             trControl = ctrl,
                             tuneLength = 10,
                             metric = 'Sens')

CART.Split <- caret::train(TrainingPreds.dum,
                           TrainingObs,
                           method = 'rpart',
                           trControl = ctrl,
                           tuneLength = 10,
                           metric = 'Sens')

J48.Grouped <- caret::train(TrainingPreds,
                            TrainingObs,
                            method = 'J48',
                            trControl = ctrl,
                            metric = 'Sens')

J48.Split <- caret::train(TrainingPreds.dum,
                          TrainingObs,
                          method = 'J48',
                          trControl = ctrl,
                          metric = 'Sens')


C50.Grouped <- caret::train(TrainingPreds,
                            TrainingObs,
                            method = 'C5.0',
                            trControl = ctrl,
                            metric = 'Sens',
                            tuneGrid = expand.grid(.model = 'tree',
                                                   .winnow = c(T,F),
                                                   .trials = c(1,10,20,30)))

C50.Split <- caret::train(TrainingPreds.dum,
                          TrainingObs,
                          method = 'C5.0',
                          trControl = ctrl,
                          metric = 'Sens',
                          tuneGrid = expand.grid(.model = 'tree',
                                                 .winnow = c(T,F),
                                                 .trials = c(1,10,20,30)))

tree.grouped.preds <- list(CART.Grouped = CART.Grouped,
                           J48.Grouped = J48.Grouped,
                           C50.Grouped = C50.Grouped) %>%
  purrr::map(~ predict(., TestingPreds)) 

tree.split.preds   <- list(CART.Split = CART.Split,
                           J48.Split = J48.Split,
                           C50.Split = C50.Split) %>%
  purrr::map(~ predict(., TestingPreds.dum)) 

tree.preds <- c(tree.grouped.preds, tree.split.preds)

tree_pred_plot <- tree.preds %>%
  purrr::map_df(~ caret::confusionMatrix(., TestingObs)$overall %>%
                  enframe,
                .id='id') %>%
  separate(id, into = c('model','data')) %>%
  filter(name %in% c('Accuracy','Kappa')) %>%
  ggplot(aes(x = model, y = value, colour = data, group = data)) +
  geom_point() + 
  geom_line() + 
  facet_grid(name ~ .)

ggsave(plot = tree_pred_plot, 
       filename = file.path(output_directory,'tree_pred_plot.png'),
       width = 12, height = 8, dpi = 100)

### bagged trees:

CART.Bagged.Grouped <- caret::train(TrainingPreds,
                                    TrainingObs,
                                    method = 'treebag',
                                    trControl = ctrl,
                                    tuneLength = 10,
                                    metric = 'Sens')


CART.Bagged.Split <- caret::train(TrainingPreds.dum,
                                  TrainingObs,
                                  method = 'treebag',
                                  trControl = ctrl,
                                  tuneLength = 10,
                                  metric = 'Sens')

CART.Boosted.Grouped <- caret::train(TrainingPreds,
                                     TrainingObs,
                                     method = 'gbm',
                                     trControl = ctrl,
                                     tuneLength = 10,
                                     metric = 'Sens')

CART.Boosted.Split<- caret::train(TrainingPreds.dum,
                                  TrainingObs,
                                  method = 'gbm',
                                  trControl = ctrl,
                                  tuneLength = 10,
                                  metric = 'Sens')

tree.type.preds.grouped <- list(
  CART.Raw.Grouped = CART.Grouped,
  CART.Bagged.Grouped = CART.Bagged.Grouped,
  CART.Boosted.Grouped = CART.Boosted.Grouped) %>%
  purrr::map(predict, newdata = TestingPreds)

tree.type.preds.split <- list(
  CART.Raw.Split = CART.Split,
  CART.Bagged.Split = CART.Bagged.Split,
  CART.Boosted.Split = CART.Boosted.Split) %>%
  purrr::map(predict, newdata = TestingPreds.dum)

tree.type.preds <- c(tree.type.preds.grouped, tree.type.preds.split)

tree_type_pred_plot <- tree.type.preds %>%
  purrr::map_df(~ caret::confusionMatrix(., TestingObs)$overall %>%
                  enframe,
                .id='id') %>%
  filter(name %in% c('Accuracy','Kappa')) %>%
  separate(id, c('cart','model','data')) %>%
  ggplot(aes(x = model, y = value, colour = data, group = data)) +
  geom_point() + 
  geom_line() + 
  facet_grid(name ~ .)

ggsave(plot = tree_type_pred_plot,
       filename = file.path(output_directory,'tree_type_pred_plot.png'),
       width = 12, height = 8, dpi = 100)

# ok random forest time 

RF.Grouped <- caret::train(TrainingPreds,
                           TrainingObs,
                           method = 'rf',
                           trControl = ctrl,
                           tuneLength = 10,
                           metric = 'Sens')

RF.Split <- caret::train(TrainingPreds.dum,
                         TrainingObs,
                         method = 'rf',
                         trControl = ctrl,
                         tuneLength = 10,
                         metric = 'Sens')

Grouped.Models <- list(
  CART.Grouped = CART.Grouped,
  Bagged.Grouped = CART.Bagged.Grouped,
  Boosted.Grouped = CART.Boosted.Grouped,
  J48.Grouped = J48.Grouped,
  C50.Grouped = C50.Grouped,
  RF.Grouped = RF.Grouped)

Grouped.Models.preds <- Grouped.Models %>%
  purrr::map(predict, newdata = TestingPreds)

Grouped.Models.probs <- Grouped.Models %>%
  purrr::map(predict, newdata = TestingPreds, type = 'prob')

Grouped.Models.perf <- Grouped.Models.preds %>%
  purrr::map_df(~ caret::confusionMatrix(., TestingObs)$overall %>%
                  enframe,
                .id = 'id') %>%
  filter(name %in% c('Accuracy','Kappa'))

Grouped.Models.lift <- Grouped.Models.probs %>%
  purrr::map_df(~ as_tibble(.) %>%
                  mutate(obs = TestingObs) %>%
                  rowid_to_column,
                .id = 'id') %>%
  select(id, prob = yes, obs, rowid) %>%
  spread(key = id, value = prob) %>%
  caret::lift(obs ~  CART.Grouped + Bagged.Grouped + Boosted.Grouped +
              J48.Grouped +C50.Grouped + RF.Grouped,
              data = .)

grouped_lift_plot <- ggplot(Grouped.Models.lift$data)+
  geom_line(aes(CumTestedPct, CumEventPct, color = liftModelVar))+
  xlab("% Samples tested")+
  ylab("% Samples found")+
  scale_colour_discrete(guide = guide_legend(title = "method"))+
  geom_polygon(data = data.frame(x = c(0, Grouped.Models.lift$pct, 100, 0),
                                 y = c(0, 100, 100, 0)),
               aes(x = x, y = y), alpha = 0.1) +
  ggtitle('Lift curve')


Split.Models <- list(
  CART.Split = CART.Split,
  Bagged.Split = CART.Bagged.Split,
  Boosted.Split = CART.Boosted.Split,
  J48.Split = J48.Split,
  C50.Split = C50.Split,
  RF.Split = RF.Split)

Split.Models.preds <- Split.Models %>%
  purrr::map(predict, newdata = TestingPreds.dum)

Split.Models.probs <- Split.Models %>%
  purrr::map(predict, newdata = TestingPreds.dum, type = 'prob')

Split.Models.perf <- Split.Models.preds %>%
  purrr::map_df(~ caret::confusionMatrix(., TestingObs)$overall %>%
                  enframe,
                .id = 'id') %>%
  filter(name %in% c('Accuracy','Kappa'))

Split.Models.lift <- Split.Models.probs %>%
  purrr::map_df(~ as_tibble(.) %>%
                  mutate(obs = TestingObs) %>%
                  rowid_to_column,
                .id = 'id') %>%
  select(id, prob = yes, obs, rowid) %>%
  spread(key = id, value = prob) %>%
  caret::lift(obs ~  CART.Split + Bagged.Split + Boosted.Split +
                J48.Split +C50.Split + RF.Split,
              data = .)

split_lift_plot <- ggplot(Split.Models.lift$data)+
  geom_line(aes(CumTestedPct, CumEventPct, color = liftModelVar))+
  xlab("% Samples tested")+
  ylab("% Samples found")+
  scale_colour_discrete(guide = guide_legend(title = "method"))+
  geom_polygon(data = data.frame(x = c(0, Split.Models.lift$pct, 100, 0),
                                 y = c(0, 100, 100, 0)),
               aes(x = x, y = y), alpha = 0.1) +
  ggtitle('Lift curve')

# ok

Split.Models.perf %>% 
  bind_rows(Grouped.Models.perf) %>%
  spread(key = name, value = value) %>%
  separate(id, 'model','data') %>%
  arrange(-Kappa) %>%
  write_csv(file.path(output_directory, 'perf-table.csv'))

read_csv(file.path(output_directory, 'perf-table.csv')) %>%
  knitr::kable(.)

ggsave(plot = grouped_lift_plot,
       filename = file.path(output_directory,'grouped_lift_plot.png'),
       width = 12, height = 8, dpi = 100)

ggsave(plot = split_lift_plot,
       filename = file.path(output_directory,'split_lift_plot.png'),
       width = 12, height = 8, dpi = 100)

png(filename = file.path(output_directory,'tree_plot.png'))
rpart.plot::rpart.plot(CART.Grouped$finalModel)
dev.off()
       
       
       
