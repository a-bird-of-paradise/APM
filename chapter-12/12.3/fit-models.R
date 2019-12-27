library(tidyverse)
library(caret)
library(C50)
data(churn)

output_directory <- file.path(getwd(),'12.3')
dir.create(output_directory,showWarnings = F)

set.seed(1988772)

# add extra predictors 

churnTrain.extra <- churnTrain %>%
  mutate(TotalCharge = total_day_charge + total_eve_charge + total_night_charge + total_intl_charge,
         HighCharge = as.factor(ifelse(TotalCharge >= 75, 'yes','no')),
         ManyCalls = as.factor(ifelse(number_customer_service_calls >= 3, 'yes','no')),
         total_day_charge_shift = pmax(0,total_day_charge-40),
         total_day_minutes_shift = pmax(0, total_day_minutes - 200),
         total_night_minutes_capped = pmin(total_night_minutes, 200))

churnTest.extra <- churnTest %>%
  mutate(TotalCharge = total_day_charge + total_eve_charge + total_night_charge + total_intl_charge,
         HighCharge = as.factor(ifelse(TotalCharge >= 75, 'yes','no')),
         ManyCalls = as.factor(ifelse(number_customer_service_calls >= 3, 'yes','no')),
         total_day_charge_shift = pmax(0,total_day_charge-40),
         total_day_minutes_shift = pmax(0, total_day_minutes - 200),
         total_night_minutes_capped = pmin(total_night_minutes, 200))

# use  dummy variables to get rid of factor issues 

churn.dum <- dummyVars(churn ~ ., churnTrain.extra)

TrainingPreds <- churnTrain.extra %>% select(-churn)
TrainingObs <- churnTrain.extra %>% pull(churn)
TestingPreds <- churnTest.extra %>% select(-churn)
TestingObs <- churnTest.extra %>% pull(churn)
TrainingPreds.dum <- predict(churn.dum,
                             newdata = churnTrain.extra) 
TestingPreds.dum <- predict(churn.dum,
                            newdata = churnTest.extra) 

ctrl <- caret::trainControl(method = "cv",
                            classProbs = TRUE,
                            summaryFunction = caret::twoClassSummary)

LinReg <- caret::train(TrainingPreds.dum,
                       TrainingObs,
                       method = "glmStepAIC",
                       metric = "Sens",
                       trControl = ctrl,
                       preProc = c("center","scale","nzv","corr"),
                       direction = 'forward')

LDA <- caret::train(TrainingPreds.dum,
                    TrainingObs,
                    method = "stepLDA",
                    metric = "Sens",
                    trControl = ctrl,
                    preProc = c("center","scale"))

PLSDA <- caret::train(TrainingPreds.dum,
                      TrainingObs,
                      method = "pls",
                      tuneGrid = expand.grid(.ncomp = 1:50),
                      preProc = c("center","scale"),
                      metric = "Sens",
                      trControl = ctrl)

GLM <- caret::train(TrainingPreds.dum,
                    TrainingObs,
                    method = "glmnet",
                    tuneGrid = expand.grid(.alpha = c(0,.1,.2,.4,.6,.8,1.0),
                                           .lambda = seq(.01,2,length = 40)),
                    preProc = c("center","scale"),
                    trControl = ctrl,
                    metric = "Sens")

NSC <- caret::train(x = TrainingPreds.dum,
                    y = TrainingObs,
                    method = 'pam',
                    tuneGrid = expand.grid(.threshold = 1:25),
                    preProc = c('center','scale'),
                    metric = 'Sens',
                    trControl = ctrl)

Models <- list(
  LinReg = LinReg,
  GLM = GLM,
  NSC = NSC,
  LDA = LDA,
  PLSDA = PLSDA
)

# summary performance statistics

perf_stats <- Models %>%
  map_df(~ confusionMatrix(predict(., newdata = TestingPreds.dum),
                           TestingObs)$overall %>%
           enframe,
         .id = 'id')

class_stats <- Models %>%
  map_df(~ confusionMatrix(predict(., newdata = TestingPreds.dum),
                           TestingObs)$byClass %>%
           enframe,
         .id = 'id')

# lift curve

lift_stats <- Models %>%
  map_df(~ predict(.x, newdata = TestingPreds.dum, type = 'prob') %>%
           as_tibble %>%
           mutate(obs = TestingObs) %>%
           rowid_to_column,
         .id = 'id') %>%
  select(id,prob=yes,obs,rowid) %>%
  spread(key = id, value = prob) %>%
  caret::lift(obs ~ GLM + LDA + LinReg + NSC + PLSDA, data = .)

lift_curve <- ggplot(lift_stats$data)+
  geom_line(aes(CumTestedPct, CumEventPct, color = liftModelVar))+
  xlab("% Samples tested")+
  ylab("% Samples found")+
  scale_colour_discrete(guide = guide_legend(title = "method"))+
  geom_polygon(data = data.frame(x = c(0, lift_stats$pct, 100, 0),
                                 y = c(0, 100, 100, 0)),
               aes(x = x, y = y), alpha = 0.1) +
  ggtitle('Lift curve')

# calibration curve

calibration_stats <- Models %>%
  map_df(~ predict(.x, newdata = TestingPreds.dum, type = 'prob') %>%
           as_tibble %>%
           mutate(obs = TestingObs) %>%
           rowid_to_column,
         .id = 'id') %>%
  select(id,prob=yes,obs,rowid) %>%
  spread(key = id, value = prob) %>%
  caret::calibration(obs ~ GLM + LDA + LinReg + NSC + PLSDA, data = .)

calibration_plot <- calibration_stats$data %>%
  ggplot(aes(x=midpoint,y=Percent,colour=calibModelVar)) + 
  geom_point() + 
  geom_line() + 
  geom_abline() + 
  scale_x_continuous(name = 'Bin midpoint') + 
  scale_y_continuous(name = 'Observed Event Percentage') + 
  ggtitle('Calibration plot')

# ok reporting

perf_stats %>%
  spread(key = id, value = value) %>%
  filter(name %in% c('Accuracy','Kappa')) %>%
  mutate_if(is.numeric, function(x) round(x,3)) %>%
  knitr::kable(.)

class_stats %>%
  spread(key = id, value = value) %>%
  mutate_if(is.numeric, function(x) round(x,3)) %>%
  knitr::kable(.)

predictor_names <- setdiff(names(datum),c('pred','obs'))

this_pred <- 'TotalCharge'

datum <- TestingPreds %>%
  mutate(pred = predict(LinReg, newdata = TestingPreds.dum),
         obs = TestingObs)

factor_plot <- datum %>%
  select(x = this_pred, obs, pred) %>%
  ggplot(aes(x=x, fill = pred)) + 
  geom_histogram(colour = 'black') + 
  facet_grid(obs ~ .) +
  ggtitle(this_pred)

ggsave(plot = lift_curve,
       filename = file.path(output_directory,'lift_curve.png'),
       width = 12, height = 8, dpi = 100)
ggsave(plot = calibration_plot,
       filename = file.path(output_directory,'calibration_plot.png'),
       width = 12, height = 8, dpi = 100)
ggsave(plot = factor_plot,
       filename = file.path(output_directory,'factor_plot.png'),
       width = 12, height = 8, dpi = 100)
