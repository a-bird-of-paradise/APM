library(tidyverse)
library(caret)
library(arules)
library(RANN)
library(plotROC)

# setup ----

set.seed(8761)

output_directory <- file.path(getwd(),'16.1')

data("AdultUCI")

data_t <- AdultUCI %>%
  as_tibble %>% 
  select(-fnlwgt,-`education-num`) %>%
  filter(!is.na(income)) %>%
  mutate(cap_gain_bin = as_factor(ifelse(is.na(`capital-gain`),NA(),
                                         ifelse(`capital-gain`==0, 'None',
                                                ifelse(`capital-gain` < 5000, 'Some', 'Lots')))))%>%
  mutate(cap_loss_bin = as_factor(ifelse(is.na(`capital-loss`),NA(),
                                         ifelse(`capital-loss`==0, 'None',
                                                ifelse(`capital-loss` < 5000, 'Some', 'Lots'))))) %>%
  mutate(origin = ifelse(is.na(`native-country`),'Other',
                         ifelse(`native-country` == 'United-States','US',
                                ifelse(`native-country` == 'Mexico','Mexico','Other'))) %>%
           as_factor) %>%
  mutate_if(is.factor,fct_explicit_na)

summary(data_t)

# want to split data into training / testing using income... ----

TrainingIndices <- caret::createDataPartition(y = data_t$income,
                                              p = 0.7)$Resample1

TrainingPredictors <- data_t[TrainingIndices,] %>% select(-income)
TrainingOutcome <- data_t[TrainingIndices,] %>% select(income)

TestingPredictors <- data_t[-TrainingIndices,] %>% select(-income)
TestingOutcome <- data_t[-TrainingIndices,] %>% select(income)

# examine one ways ----

plots <- names(TrainingPredictors) %>%
  `names<-`(.,.) %>%
  purrr::map(~ tibble(!!.x := TrainingPredictors %>% select(.x) %>% deframe,
                      y = TrainingOutcome %>% deframe) %>%
               ggplot(aes(x = get(.x), fill = y, colour = y)) + 
               geom_bar() + 
               xlab(.x) +
               ggtitle(.x))

names(TrainingPredictors) %>%
  `names<-`(.,.) %>%
  purrr::map(~ ggsave(plots[[.x]],
                      width = 8,
                      height = 6,
                      dpi = 100,
                      filename = file.path(output_directory,
                                           paste0(.x,'.png'))))

# dump redundant cols ----

TrainingPredictors <- TrainingPredictors %>%
  select(-`native-country`,-`capital-gain`,-`capital-loss`) 

TestingPredictors <- TestingPredictors %>%
  select(-`native-country`,-`capital-gain`,-`capital-loss`) 

to_check <- TrainingPredictors %>%
  select_if(is.factor) %>%
  names %>%
  `names<-`(.,.)

# check consistency i.e. any factor levels in training and not in testing or vice versa ----

to_check %>%
  purrr::map(~ TrainingPredictors %>%
               group_by(!!.x := get(.x)) %>%
               summarise(training = n()) %>%
               full_join(TestingPredictors %>%
                           group_by(!!.x := get(.x)) %>%
                           summarise(testing = n()),
                         by = .x)
  )

# finally mutate into dummy variables b/c some models can't handle non numeric data ----

DummyVarMaker <- caret::dummyVars(~ ., 
                                  TrainingPredictors,
                                  fullRank = T)
TrainingPredictors_D <- predict(DummyVarMaker,TrainingPredictors)
TestingPredictors_D <- predict(DummyVarMaker,TestingPredictors)

# lets fit some models. use kappa as the thing. ----

ctrl <- caret::trainControl(method = 'repeatedcv', 
                            repeats = 5, 
                            classProbs = T, 
                            verboseIter = TRUE,
                            savePredictions = T)

# GLM ----

glm <- caret::train(TrainingPredictors_D %>% as.data.frame,
                    TrainingOutcome$income, 
                    method = 'glm', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('center','scale','knnImpute','nzv'))

glm_data <- tibble(obs = TestingOutcome$income,
                   pred = predict(glm,TestingPredictors_D),
                   IsSmall = ifelse(obs == 'small', 1, 0),
                   ProbSmall = predict(glm, TestingPredictors_D, type = 'prob')$small,
                   Bucket = as.integer(ceiling(ProbSmall*20))/20.0,
                   Model = 'GLM')

# PLS. ----

pls <- caret::train(TrainingPredictors_D,
                    TrainingOutcome$income,
                    method = 'pls', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    tuneGrid = expand.grid(.ncomp = 1:10),
                    preProcess = c('center','scale','knnImpute'))

pls_data <- tibble(obs = TestingOutcome$income,
                   pred = predict(pls,TestingPredictors_D),
                   IsSmall = ifelse(obs == 'small', 1, 0),
                   ProbSmall = predict(pls, TestingPredictors_D, type = 'prob')$small,
                   Bucket = as.integer(ceiling(ProbSmall*20))/20.0,
                   Model = 'PLS')

# calibration issues, so try again 

pls_calib_data <- tibble(class = TrainingOutcome$income,
                         pls_prob = predict(pls,TrainingPredictors_D, type = 'prob')$small)

pls_sigmoid <- glm(class ~ pls_prob, 
                   data = pls_calib_data %>% mutate(class = fct_relevel(class, 'large','small')),
                   family = 'binomial') 

pls_data_calibrated <- pls_data %>%
  mutate(ProbSmall = predict(pls_sigmoid, newdata = tibble(class = obs,
                                                           pls_prob = ProbSmall),
                             type = 'response'),
         pred = factor(ifelse(ProbSmall >= 0.5, 'small', 'large'), levels = c('small','large')),
         IsSmall = ifelse(obs == 'small', 1, 0),
         Bucket = as.integer(ceiling(ProbSmall*20))/20.0,
         Model = 'PLS-Calibrated')


# FDA ----

fda <- caret::train(TrainingPredictors_D,
                    TrainingOutcome$income,
                    method = 'fda', 
                    metric = 'Kappa',
                    trControl = ctrl)

fda_data <- tibble(obs = TestingOutcome$income,
                   pred = predict(fda,TestingPredictors_D),
                   IsSmall = ifelse(obs == 'small', 1, 0),
                   ProbSmall = predict(fda, TestingPredictors_D, type = 'prob')$small,
                   Bucket = as.integer(ceiling(ProbSmall*20))/20.0,
                   Model = 'FDA')
# lda ----

lda <- caret::train(TrainingPredictors_D,
                    TrainingOutcome$income,
                    method = 'lda',
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('center','scale','knnImpute','nzv'))

lda_data <- tibble(obs = TestingOutcome$income,
                   pred = predict(lda,TestingPredictors_D),
                   IsSmall = ifelse(obs == 'small', 1, 0),
                   ProbSmall = predict(lda, TestingPredictors_D, type = 'prob')$small,
                   Bucket = as.integer(ceiling(ProbSmall*20))/20.0,
                   Model = 'LDA')

# xgboost ----

xgbTree <- caret::train(TrainingPredictors_D,
                        TrainingOutcome$income,
                        method = 'xgbTree',
                        metric = 'Kappa',
                        trControl = ctrl,
                        preProcess = c('center','scale','knnImpute','nzv'))

saveRDS(xgbTree,'xgbTree.rds')

xgbt_data <- tibble(obs = TestingOutcome$income,
                    pred = predict(xgbTree,TestingPredictors_D),
                    IsSmall = ifelse(obs == 'small', 1, 0),
                    ProbSmall = predict(xgbTree, TestingPredictors_D, type = 'prob')$small,
                    Bucket = as.integer(ceiling(ProbSmall*20))/20.0,
                    Model = 'XGBoost')

# ok let's look at some results ----

all_data <- 
  glm_data %>%
  bind_rows(pls_data) %>%
  bind_rows(pls_data_calibrated) %>%
  bind_rows(lda_data) %>%
  bind_rows(fda_data) %>%
  bind_rows(xgbt_data)

models <- all_data %>% distinct(Model) %>% pull(Model) %>% `names<-`(.,.)

models %>%
  purrr::map_df(~ tibble( Kappa = caret::confusionMatrix(all_data %>% 
                                                           filter(Model == .x) %>% 
                                                           pull(pred),
                                                         all_data %>% 
                                                           filter(Model == .x) %>% 
                                                           pull(obs))$overall[['Kappa']],
                          Model = .x)) %>%
  select(Model,Kappa) %>%
  arrange(Kappa) %>%
  knitr::kable()

roc_curves <- all_data %>%
  ggplot(aes(d = IsSmall, m = ProbSmall, colour = Model)) + 
  geom_roc(n.cuts = 0) + 
  geom_abline()

roc_curves_zoom <- roc_curves +
  scale_x_continuous(limits = c(0.1,0.25)) + 
  scale_y_continuous(limits = c(0.75,0.9))

histo_plots <- all_data %>%
  ggplot(aes(x = ProbSmall, fill = obs)) + 
  geom_histogram(binwidth = 0.01) + 
  facet_grid(Model ~ obs, scales = 'free')

histo_plots_log <- histo_plots + scale_y_log10()

calibration_plot <- all_data %>%
  group_by(Bucket,Model) %>%
  summarise(OEP = sum(ifelse(obs == 'small',1,0)/n()),
            n=n()) %>%
  ggplot(aes(x=Bucket,y=OEP, colour = Model)) + 
  geom_point(aes(size=n)) + 
  geom_line() + 
  geom_abline() + 
  scale_x_continuous(limits=c(0,1),name = "Expected probability of small") + 
  scale_y_continuous(limits=c(0,1),name = "Actual probability of small")

ggsave(plot = roc_curves,
       filename = file.path(output_directory, 'roc_curves.png'),
       width = 8, height = 6, dpi = 100)
ggsave(plot = roc_curves_zoom,
       filename = file.path(output_directory, 'roc_curves_zoom.png'),
       width = 8, height = 6, dpi = 100)

ggsave(plot = histo_plots,
       filename = file.path(output_directory, 'histo_plots.png'),
       width = 8, height = 6, dpi = 100)
ggsave(plot = histo_plots_log,
       filename = file.path(output_directory, 'histo_plots_log.png'),
       width = 8, height = 6, dpi = 100)

ggsave(plot = calibration_plot,
       filename = file.path(output_directory, 'calibration_plot.png'),
       width = 8, height = 6, dpi = 100)

# phew. now mess with the cutoffs 

kappa_func <- function(threshold,the_model) {
  caret::confusionMatrix(TestingOutcome$income,
                         factor(ifelse(predict(the_model, 
                                               TestingPredictors_D, 
                                               type = 'prob')$small >= threshold, 
                                       'small','large'),
                                levels = c('small','large'))) %>%
    `$`(overall) %>%
    `[[`('Kappa')}

xgbt_func <- purrr::partial(kappa_func, the_model = xgbTree)
fda_func <- purrr::partial(kappa_func, the_model = fda)
lda_func <- purrr::partial(kappa_func, the_model = lda)
pls_func <- purrr::partial(kappa_func, the_model = pls)
glm_func <- purrr::partial(kappa_func, the_model = glm)

best_kappa_table <- optimise(xgbt_func,interval = c(0,1), maximum = T) %>% 
  enframe %>% 
  unnest(cols = value) %>% 
  mutate(what = 'XGBoost') %>%
  bind_rows(optimise(fda_func,interval = c(0,1), maximum = T) %>% 
              enframe %>% 
              unnest(cols = value) %>% 
              mutate(what = 'FDA') ) %>%
  bind_rows(optimise(lda_func,interval = c(0,1), maximum = T) %>% 
              enframe %>% 
              unnest(cols = value) %>% 
              mutate(what = 'LDA') ) %>%
  bind_rows(optimise(pls_func,interval = c(0,1), maximum = T) %>% 
              enframe %>% 
              unnest(cols = value) %>% 
              mutate(what = 'PLS') ) %>%
  bind_rows(optimise(glm_func,interval = c(0,1), maximum = T) %>% 
              enframe %>% 
              unnest(cols = value) %>% 
              mutate(what = 'GLM'))

default_kappa_table <- tibble(Kappa = caret::confusionMatrix(TestingOutcome$income,
                                                             predict(xgbTree,TestingPredictors_D)) %>%
                                `$`(overall) %>%
                                `[[`('Kappa'),
                              what = 'XGBoost') %>%
  bind_rows(tibble(Kappa = caret::confusionMatrix(TestingOutcome$income,
                                                  predict(pls,TestingPredictors_D)) %>%
                     `$`(overall) %>%
                     `[[`('Kappa'),
                   what = 'PLS'))%>%
  bind_rows(tibble(Kappa = caret::confusionMatrix(TestingOutcome$income,
                                                  predict(glm,TestingPredictors_D)) %>%
                     `$`(overall) %>%
                     `[[`('Kappa'),
                   what = 'GLM'))%>%
  bind_rows(tibble(Kappa = caret::confusionMatrix(TestingOutcome$income,
                                                  predict(fda,TestingPredictors_D)) %>%
                     `$`(overall) %>%
                     `[[`('Kappa'),
                   what = 'FDA'))%>%
  bind_rows(tibble(Kappa = caret::confusionMatrix(TestingOutcome$income,
                                                  predict(lda,TestingPredictors_D)) %>%
                     `$`(overall) %>%
                     `[[`('Kappa'),
                   what = 'LDA'))

default_kappa_table %>%
  dplyr::rename(value = Kappa) %>%
  mutate(name = 'DefaultKappa') %>%
  bind_rows(best_kappa_table) %>%
  spread(key = name, value = value) %>%
  dplyr::rename(BestCutoff = maximum,
                BestKappa = objective) %>%
  arrange(desc(BestKappa)) %>%
  knitr::kable()


