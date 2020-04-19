library(tidyverse)
library(caret)

set.seed(4004)

output_directory <- file.path(getwd(),'16.2')

source(file.path(output_directory,'plot-funcs.r'))

Clothing_Store <- read_csv(file.path(output_directory,'Clothing_Store'),
                           col_types = cols(HHKEY = col_character(), # drop this
                                            ZIP_CODE = col_integer(), # coarsen this
                                            CC_CARD = col_factor(),
                                            PROMOS = col_integer(),
                                            RESP = col_factor(),
                                            VALPHON = col_factor(),
                                            WEB = col_factor(),
                                            PC_CALC20 = col_integer(),
                                            DAYS = col_integer(),
                                            STYLES = col_integer(),
                                            COUPONS = col_integer(),
                                            STORES = col_integer(),
                                            MAILED = col_integer(),
                                            RESPONDED = col_integer(),
                                            CLUSTYPE = col_integer())) %>%
  select(-HHKEY) %>%
  mutate(RESP = fct_recode(RESP, true = '1', false = '0'),
         RESP = fct_relevel(RESP, 'true'))

summary(Clothing_Store)

Training_Ind <- caret::createDataPartition(Clothing_Store$RESP, p=0.7)[[1]]

Training_Data <- Clothing_Store[Training_Ind,]
Testing_Data <- Clothing_Store[-Training_Ind,]

rating_variables <- Training_Data %>%
  select(-RESP) %>%
  names %>%
  `names<-`(.,.) 

number_plots <- rating_variables %>%
  purrr::map(~ one_way_plotter(Training_Data %>%
                                 mutate(ZIP_CODE = as_factor(as.integer(ZIP_CODE/1000)),
                                        COUPONS = pmin(COUPONS,10),
                                        PROMOS = pmin(PROMOS,30),
                                        STORES = pmin(STORES,10),
                                        FRE = pmin(FRE,30),
                                        AMSPEND = pmin(AMSPEND,1),
                                        AXSPEND = pmin(AXSPEND,1),
                                        AVRG = pmin(AVRG,500),
                                        MON = pmin(MON,5000)), 
                               .x))

rating_variables %>%
  purrr::map(~ ggsave(plot = number_plots[[.x]],
                      filename = file.path(output_directory, paste0(.x,'.png')),
                      width = 8,
                      height = 6, 
                      dpi = 100))

corr_plot <- Training_Data %>%
  select_if(is.numeric) %>%
  cor %>%
  ggcorrplot::ggcorrplot(.)

ggsave(plot = corr_plot,
       filename = file.path(output_directory,'corr_plot.png'),
       width = 8, 
       height = 8, 
       dpi = 100)

# ok time to do some fitting

# lets fit some models. use Sens as the thing. ----

NoProbSummaryFunc <- function(data, 
                              lev = levels(data$obs),
                              model = NULL)
{
  return( c(caret::postResample(data[,'pred'],data[,'obs']),
            Sens = caret::sensitivity(data[,'pred'],data[,'obs'],lev[1]),
            Spec = caret::specificity(data[,'pred'],data[,'obs'],lev[2])
  )
  )
}

ProfitFunc <- function(data, 
                       lev = levels(data$obs),
                       model = NULL)
{
  profit <- data %>%
    mutate(profit = ifelse(pred == 'true' & obs == 'true', 29,
                           ifelse(pred == 'true' & obs == 'false', -1,
                                  ifelse(pred == 'false' & obs == 'false', 0, -30)))) %>%
    summarise(profit = sum(profit)) %>%
    pull(profit)
  
  return(c( twoClassSummary(data,lev,model),
            Profit = profit))
}

ctrl <- caret::trainControl(method = 'cv', 
                            number = 5, 
                            classProbs = T, 
                            verboseIter = TRUE,
                            savePredictions = T, 
                            summaryFunction = ProfitFunc)

ctrl_NoProb <- caret::trainControl(method = 'cv', 
                                   number = 5, 
                                   classProbs = F, 
                                   verboseIter = TRUE,
                                   savePredictions = T, 
                                   summaryFunction = NoProbSummaryFunc)
# penalty matrix - willing to accept true preds if actually false, 
# but not false preds if actually true. Prefer correct to either. 
# may have to transpose / reciprocate depending on method!

#aggressively downsample to flood this with trues. maybe better? 

Training_Data_DS <- downSample_custom(Training_Data %>% select(-RESP),
                                      Training_Data %>% pull(RESP),
                                      yname = 'RESP',
                                      frac = 0.2)

rel_cost <- 20
costs <- matrix(c(0,rel_cost,1,0), 
                nrow = 2)

rownames(costs) <- levels(Training_Data$RESP)
colnames(costs) <- levels(Training_Data$RESP)

glm <- caret::train(RESP ~ .,
                    data = Training_Data,
                    method = 'glm', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('center','scale','knnImpute','nzv','corr'))
glm_DS <- caret::train(RESP ~ .,
                       data = Training_Data_DS,
                       method = 'glm', 
                       metric = 'Kappa',
                       trControl = ctrl,
                       preProcess = c('center','scale','knnImpute','nzv','corr'))

glmnet <- caret::train(RESP ~ .,
                       data = Training_Data,
                       method = 'glmnet', 
                       metric = 'Kappa',
                       trControl = ctrl,
                       preProcess = c('center','scale','knnImpute','nzv','corr'),
                       tuneGrid = expand.grid(.alpha = seq(0,0.2, length = 5),
                                              .lambda = seq(.01, 2, length = 5)))

glmnet_DS <- caret::train(RESP ~ .,
                          data = Training_Data_DS,
                          method = 'glmnet', 
                          metric = 'Kappa',
                          trControl = ctrl,
                          preProcess = c('center','scale','knnImpute','nzv','corr'),
                          tuneGrid = expand.grid(.alpha = seq(0,0.2, length = 5),
                                                 .lambda = seq(.01, 2, length = 5)))

pls <- caret::train(RESP ~ .,
                    data = Training_Data,
                    method = 'pls', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('center','scale'),
                    tuneLength = 15)
pls_DS <- caret::train(RESP ~ .,
                       data = Training_Data_DS,
                       method = 'pls', 
                       metric = 'Kappa',
                       trControl = ctrl,
                       preProcess = c('center','scale'),
                       tuneLength = 15)

lda <- caret::train(RESP ~ .,
                    data = Training_Data,
                    method = 'lda', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('nzv'))

lda_DS <- caret::train(RESP ~ .,
                       data = Training_Data_DS,
                       method = 'lda', 
                       metric = 'Kappa',
                       trControl = ctrl,
                       preProcess = c('nzv'))

qda <- caret::train(RESP ~ .,
                    data = Training_Data,
                    method = 'qda', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('nzv'))

qda_DS <- caret::train(RESP ~ .,
                       data = Training_Data_DS,
                       method = 'qda', 
                       metric = 'Kappa',
                       trControl = ctrl,
                       preProcess = c('nzv'))

fda <- caret::train(RESP ~ .,
                    data = Training_Data,
                    method = 'fda', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('center','scale'),
                    tuneGrid = expand.grid(.nprune = c(2, 8, 14, 25),
                                           .degree = 1:1)) # more helps but slow

fda_DS <- caret::train(RESP ~ .,
                       data = Training_Data_DS,
                       method = 'fda', 
                       metric = 'Kappa',
                       trControl = ctrl,
                       preProcess = c('center','scale'),
                       tuneGrid = expand.grid(.nprune = c(2, 8, 14, 25),
                                              .degree = 1:2)) # more helps but slow

mda <- caret::train(RESP ~ .,
                    data = Training_Data,
                    method = 'mda', 
                    metric = 'Kappa',
                    trControl = ctrl,
                    preProcess = c('center','scale'),
                    tuneGrid = expand.grid(.subclasses = 1:20)) # more helps but slow
mda_DS <- caret::train(RESP ~ .,
                       data = Training_Data_DS,
                       method = 'mda', 
                       metric = 'Kappa',
                       trControl = ctrl,
                       preProcess = c('center','scale'),
                       tuneGrid = expand.grid(.subclasses = 1:20)) # more helps but slow

nb <- caret::train(RESP ~ .,
                   data = Training_Data,
                   method = 'nb', 
                   metric = 'Kappa',
                   trControl = ctrl,
                   preProcess = c('nzv'))

nb_DS <- caret::train(RESP ~ .,
                      data = Training_Data_DS,
                      method = 'nb', 
                      metric = 'Kappa',
                      trControl = ctrl,
                      preProcess = c('nzv'))

tree <- caret::train(RESP ~ .,
                     data = Training_Data,
                     method = 'rpart1SE', 
                     metric = 'Kappa',
                     trControl = ctrl)

tree_DS <- caret::train(RESP ~ .,
                        data = Training_Data_DS,
                        method = 'rpart1SE', 
                        metric = 'Kappa',
                        trControl = ctrl)

J48 <- caret::train(RESP ~ .,
                    data = Training_Data,
                    method = 'J48', 
                    metric = 'Kappa',
                    trControl = ctrl)

J48_DS <- caret::train(RESP ~ .,
                       data = Training_Data_DS,
                       method = 'J48', 
                       metric = 'Kappa',
                       trControl = ctrl)

C5.0 <- caret::train(RESP ~ .,
                     data = Training_Data,
                     method = 'C5.0', 
                     metric = 'Kappa',
                     trControl = ctrl)

C5.0_DS <- caret::train(RESP ~ .,
                        data = Training_Data_DS,
                        method = 'C5.0', 
                        metric = 'Kappa',
                        trControl = ctrl)

C5.0Cost <- caret::train(RESP ~ .,
                         data = Training_Data,
                         method = 'C5.0', 
                         metric = 'Kappa',
                         trControl = ctrl_NoProb,
                         cost = costs)

C5.0Cost_DS <- caret::train(RESP ~ .,
                            data = Training_Data,
                            method = 'C5.0', 
                            metric = 'Kappa',
                            trControl = ctrl_NoProb,
                            cost = costs)

rf <- caret::train(RESP ~ .,
                   data = Training_Data,
                   method = 'rf', 
                   metric = 'Kappa',
                   trControl = ctrl)

rf_DS <- caret::train(RESP ~ .,
                      data = Training_Data_DS,
                      method = 'rf', 
                      metric = 'Kappa',
                      trControl = ctrl)


model_names <- c('C5.0','fda','glm','glmnet','J48',
                 'lda','mda','nb','pls','qda','rf',
                 'tree','C5.0Cost',
                 'C5.0_DS','fda_DS','glm_DS','glmnet_DS','J48_DS',
                 'lda_DS','mda_DS','nb_DS','pls_DS','qda_DS','rf_DS',
                 'tree_DS','C5.0Cost_DS') %>%
  `names<-`(.,.)

statistics <- model_names %>%
  purrr::map_df(~ caret::confusionMatrix(predict(eval(sym(.x)),
                                                 Testing_Data),
                                         Testing_Data$RESP)$byClass %>%
                  enframe,
                .id = 'id')

statistics %>%
  spread(key = id, value = value) %>%
  View

statistics %>%
  filter(name %in% c('Sensitivity','Specificity')) %>%
  spread(key = name, value = value) %>%
  mutate(score = Sensitivity + Specificity - 1) %>%
  arrange(desc(score)) %>% 
  View


for_lift_data <- tibble(obs = Testing_Data$RESP,
                        perfect = ifelse(obs == 'true',1,0),
                        random = runif(length(Testing_Data$RESP)),
                        glm = predict(glm, Testing_Data, type = 'prob')$true,
                        glmnet = predict(glmnet, Testing_Data, type = 'prob')$true,
                        pls = predict(pls, Testing_Data, type = 'prob')$true,
                        lda = predict(lda, Testing_Data, type = 'prob')$true,
                        qda = predict(qda, Testing_Data, type = 'prob')$true,
                        glm_DS = predict(glm_DS, Testing_Data, type = 'prob')$true,
                        glmnet_DS = predict(glmnet_DS, Testing_Data, type = 'prob')$true,
                        pls_DS = predict(pls_DS, Testing_Data, type = 'prob')$true,
                        lda_DS = predict(lda_DS, Testing_Data, type = 'prob')$true,
                        qda_DS = predict(qda_DS, Testing_Data, type = 'prob')$true,
                        tree = predict(tree, Testing_Data, type = 'prob')$true,
                        tree_DS = predict(tree_DS, Testing_Data, type = 'prob')$true,
                        C50 = predict(C5.0, Testing_Data, type = 'prob')$true,
                        C50_DS = predict(C5.0_DS, Testing_Data, type = 'prob')$true,
                        J48 = predict(J48, Testing_Data, type = 'prob')$true,
                        J48_DS = predict(J48_DS, Testing_Data, type = 'prob')$true,
                        fda = predict(fda, Testing_Data, type = 'prob')$true,
                        fda_DS = predict(fda_DS, Testing_Data, type = 'prob')$true,
                        mda = predict(mda, Testing_Data, type = 'prob')$true,
                        mda_DS = predict(mda_DS, Testing_Data, type = 'prob')$true,
                        nb = predict(nb, Testing_Data, type = 'prob')$true,
                        nb_DS = predict(nb_DS, Testing_Data, type = 'prob')$true,
                        rf = predict(rf, Testing_Data, type = 'prob')$true,
                        rf_DS = predict(rf_DS, Testing_Data, type = 'prob')$true)

lift_data <- for_lift_data %>%
  mutate_at(vars(-obs), ~ rank(., ties.method = 'first') / n()) %>%
  gather(key = model, value = value, -obs) %>%
  group_by(model) %>%
  arrange(model,desc(value)) %>%
  mutate(pc_tested = row_number() / n(),
         pc_found = cumsum(ifelse(obs == 'true',1,0))/sum(ifelse(obs == 'true',1,0)))  %>%
  separate(model, into = c('model','type'), fill = 'right') %>%
  mutate(type = ifelse(is.na(type),'Full',type))

model_plot <- lift_data %>%
  ggplot(aes(x = pc_tested, y = pc_found, colour = type)) + 
  geom_line() + 
  facet_wrap(~model)

ggsave(plot = model_plot, 
       filename = file.path(output_directory, 'model_plot.png'),
       width = 8, height = 8, dpi = 100)

lift_plot <- lift_data %>%
  ggplot(aes(x = pc_tested, y = pc_found, colour = model)) + 
  geom_line(aes(linetype = type))

ggsave(plot = lift_plot, 
       filename = file.path(output_directory, 'lift_plot.png'),
       width = 8, height = 8, dpi = 100)

for_lift_data %>%
  select(-obs) %>%
  cor %>%
  ggcorrplot::ggcorrplot(.)

lift_data %>%
  filter(pc_found >= 0.6) %>%
  group_by(model,type) %>%
  filter(pc_tested == min(pc_tested)) %>%
  select(model,type,pc_tested) %>%
  spread(key = type, value = pc_tested) %>%
  arrange(Full) %>%
  knitr::kable(.)

thresholds <- seq(0.01,0.99, by = 0.01) %>% `names<-`(.,.)

models <- setdiff(for_lift_data %>% names,
        c('obs','perfect','random')) %>%
  `names<-`(.,.)

the_list <- expand.grid(thesholds = thresholds,models=models) %>% 
  as_tibble %>%
  mutate_if(is.factor,as.character)

my_summary_func <- function(m_, t_)
{  
  for_lift_data %>%
    select(obs, prob = m_) %>%
    mutate(pred = ifelse(prob >= t_, 'true','false'),
           TN = obs == 'false' & pred == 'false',
           FN = obs == 'true' & pred == 'false',
           TP = obs == 'true' & pred == 'true',
           FP = obs == 'false' & pred == 'true') %>%
    select(TN,FN,TP,FP) %>%
    gather %>%
    mutate(value = ifelse(value,1,0)) %>%
    group_by(key) %>%
    summarise(value = sum(value)/n()) %>%
    mutate(model = m_, threshold = t_)
}

threshold_investigation <- purrr::map2_df(the_list$models,
            the_list$thesholds,
            my_summary_func)

thresh_plot <- threshold_investigation %>%
  spread(key = key, value = value) %>%
  mutate(sensitivity = TP / (TP + FN),
         specificity = TN / (TN + FP)) %>%
  separate(model, c('model','type')) %>%
  mutate(type = ifelse(is.na(type),'Full','DS')) %>%
  select(model,type,threshold,sensitivity,specificity) %>% 
  gather(key = measure, value = value, -model, -type, -threshold) %>%
  ggplot(aes(x = threshold, y = value, colour = measure)) + 
  geom_line(aes(linetype = type)) + facet_wrap(~ model)

ggsave(plot = thresh_plot, 
       filename = file.path(output_directory, 'thresh_plot.png'),
       width = 8, height = 8, dpi = 100)
