library(caret)
library(pls)
library(MASS)
library(glmnet)
library(elasticnet)
library(tidyverse)

library(doMC)
registerDoMC(cores=4)

data(tecator)

# want to know effective dimension... so use PCA to squash columns

Absorp <- absorp %>% as.tibble
thresh <- 0.99

Endpoints <- endpoints %>% as.tibble
names(Endpoints) <- c("moisture","fat","protein")

AbsorpPCA <- caret::preProcess(Absorp, method = c("pca"), thresh = thresh)
print(paste0("Need ", 
             AbsorpPCA$numComp, 
             " components to capture ", 
             thresh * 100,
             "% of the variance"))

# make training and test sets 

set.seed(234987)

TrainInd <- caret::createDataPartition(Endpoints$fat, p=0.7)[[1]]
TestInd <- setdiff(1:length(Endpoints$fat), TrainInd)

TrainAbsorp <- Absorp[TrainInd,]
TrainFat <- Endpoints[TrainInd,] %>% select(fat)

TrainAbsorp %>%
  cor(method = "spearman") %>%
  ggcorrplot::ggcorrplot() %>%
  ggsave(plot = .,
         filename = "tecator-corr.png",
         width=6,
         height=6,
         dpi=100)

PreProcessor <- caret::preProcess(TrainAbsorp,
                                  method = c("center","scale"))

TrainAbsorpPP <- predict(PreProcessor, TrainAbsorp)

TrainData   <- bind_cols(TrainAbsorp  , TrainFat)
TrainDataPP <- bind_cols(TrainAbsorpPP, TrainFat)

TestAbsorp <- Absorp[-TrainInd,]
TestFat <- Endpoints[-TrainInd,] %>% select(fat)

TestAbsorpPP <- predict(PreProcessor, TestAbsorp)
TestData   <- bind_cols(TestAbsorp,   TestFat)
TestDataPP <- bind_cols(TestAbsorpPP, TestFat)

# Linear regression. Create some standard objects which will be interrogated en masse later

lm_fit <- lm(fat ~ ., TrainDataPP)

lm_pred <- broom::augment(lm_fit, newdata = TestDataPP) 

lm_pred_accuracy <- caret::defaultSummary(lm_pred %>% 
                                            select(obs = fat, pred = .fitted) %>% 
                                            as.data.frame) %>%
  bind_rows %>%
  mutate(model = "lm") %>%
  gather(key = stat, value = value, -model)

summary(lm_fit)$coefficients %>%
  as.tibble %>%
  mutate( cv = `Std. Error` / abs(Estimate)) %>%
  arrange(cv) # very large error bars around coefficients! too tense to use.

lm_plot <- lm_pred %>%
  select(.fitted) %>%
  bind_cols(TestFat) %>%
  ggplot(aes(x=fat,y=.fitted)) + geom_point() + geom_abline()

ggsave(plot = lm_plot,
       filename = 'lm_plot.png',
       width = 6,
       height = 4,
       dpi = 100)

# partial least squares

pls_fit <- plsr(fat ~ ., data = TrainDataPP)

pls_preds <- predict(pls_fit, TestAbsorpPP) %>% 
  drop %>% 
  as.tibble %>% 
  bind_cols(TestFat) %>%
  gather(key=ncomp,value=value,-fat) %>%
  mutate(ncomp = str_remove(ncomp, " comps") %>% as.integer)

pls_pred_accuracy <- setNames(1:100,1:100) %>%
  purrr::map_df(~pls_preds %>%
                  filter(ncomp == .x) %>%
                  select(obs = fat, pred = value) %>%
                  as.data.frame %>%
                  caret::defaultSummary(data = .) %>%
                  bind_rows,
                .id="ncomp") %>%
  mutate(ncomp=as.integer(ncomp)) %>%
  gather(key=stat,value=value,-ncomp) %>%
  mutate(model = 'pls')

pls_best_id <- pls_pred_accuracy %>% 
  filter(stat=="RMSE") %>% 
  filter(value==min(value)) %>% 
  pull(ncomp)

pls_plot <- pls_preds %>%
  filter(ncomp == pls_best_id) %>%
  ggplot(aes(x=fat,y=value)) + geom_point() + geom_abline()

ggsave(plot = pls_plot,
       filename = 'pls_plot.png',
       width = 6,
       height = 4,
       dpi = 100)

# ridge regression

ridge_lambdas <- 1:9 %>% 
  purrr::map(~ .x * 10 ** seq(-10,-1,by=1)) %>% 
  unlist %>% 
  sort
names(ridge_lambdas)<-ridge_lambdas

ridge_fit <- lm.ridge(fat ~ ., TrainDataPP, lambda = ridge_lambdas)

ridge_pred <- bind_cols(const = rep(1,times = length(TestFat$fat)),
                        TestAbsorpPP) %>%
  as.matrix %*% t(coef(ridge_fit)) %>%
  as.tibble %>%
  bind_cols(TestFat) %>%
  gather(key=key,value=value,-fat) %>%
  mutate(key=as.double(key))

ridge_pred_summaries <- ridge_pred %>%
  rename(obs=fat,pred=value) %>%
  group_by(key) %>%
  do(bind_rows(caret::defaultSummary(data.frame(.)))) %>%
  ungroup %>%
  gather(key=stat,value=value,-key) %>%
  mutate(model = 'ridge')

ridge_pred_summaries %>%
  ggplot(aes(x=key,y=value)) + 
  geom_point() + 
  scale_x_log10(breaks = 10 ** seq(-10,-1,1)) + 
  facet_grid(stat ~ ., scales = "free")

desired_ridge_lambda <- ridge_pred_summaries %>%
  filter(stat == "RMSE") %>%
  filter(value==min(value)) %>%
  pull(key)

ridge_plot <- ridge_pred %>%
  filter(key == desired_ridge_lambda) %>%
  ggplot(aes(x=fat,y=value)) + geom_point() + geom_abline()

ggsave(plot = ridge_plot,
       filename = 'ridge_plot.png',
       width = 6,
       height = 4,
       dpi = 100)

# lasso

lasso_fit <- glmnet(x = TrainAbsorpPP %>% as.matrix,
                    y = TrainFat %>% pull(fat),
                    alpha = 1)

lasso_lambdas <- c(10 ** seq(-10,-1,1),0)
lasso_lambdas <- 1:99 %>% 
  purrr::map(~ (.x/10) * 10 ** seq(-2,-1,by=1)) %>% 
  unlist %>% 
  sort %>%
  unique
names(lasso_lambdas) <- lasso_lambdas

lasso_coeffs <- coef(lasso_fit,lasso_lambdas)
colnames(lasso_coeffs) <- names(lasso_lambdas)

lasso_preds <- as.matrix(cbind(const = 1, TestAbsorpPP)) %*% 
  lasso_coeffs %>%
  as.matrix %>%
  as.tibble %>%
  bind_cols(TestFat) %>%
  gather(key=key,value=value,-fat) %>%
  rename(obs=fat,pred=value)

lasso_summaries <- lasso_preds %>%
  group_by(key) %>%
  do(bind_rows(caret::defaultSummary(data.frame(.)))) %>%
  ungroup %>%
  gather(key=stat,value=value,-key) %>%
  mutate(key=as.double(key),
         model = 'lasso')

lasso_best_lambda <- lasso_summaries %>%
  filter(stat == "RMSE") %>%
  filter(value == min(value)) %>%
  pull(key)

lasso_plot <- lasso_preds %>%
  filter(as.double(key) == lasso_best_lambda) %>%
  ggplot(aes(x=obs,y=pred))+geom_point()+geom_abline()

ggsave(plot = lasso_plot,
       filename = 'lasso_plot.png',
       width = 6,
       height = 4,
       dpi = 100)

# elastic net

enet_fractions <- seq(0.01,0.3,by=0.01)
enet_lambdas <- 10 ** seq(-20,-1,1)
names(enet_lambdas) <- enet_lambdas 

enet_fits <- enet_lambdas %>%
  purrr::map(~ enet(x = TrainAbsorpPP %>% as.matrix,
                    y = TrainFat %>% pull(fat),
                    lambda = .x))

enet_preds <- enet_fits %>%
  purrr::map_df(~ predict(.x,
                          TestAbsorpPP %>% as.matrix,
                          mode = 'fraction',
                          s = enet_fractions,
                          type = 'fit')$fit %>%
                  as.tibble %>%
                  `names<-`(enet_fractions) %>%
                  bind_cols(TestFat) %>%
                  gather(key=fraction,value=value,-fat),
                .id="lambda")

enet_summaries <- enet_preds %>%
  rename(obs=fat,pred=value) %>%
  group_by(lambda,fraction) %>%
  do(bind_rows(caret::defaultSummary(data.frame(.)))) %>%
  ungroup %>%
  gather(key=stat,value=value,-lambda,-fraction) %>%
  mutate(fraction = as.double(fraction),
         lambda = as.double(lambda),
         model = 'enet') 

enet_summaries %>%
  filter(lambda %in% 10 ** c(-19:-10)) %>%
  mutate(lambda = as.factor(lambda)) %>%
  ggplot(aes(x=fraction, y = value, colour = lambda)) + 
  facet_grid(stat ~ .) + geom_line() +
  scale_y_continuous(limits=c(NA,3))

enet_choose <- enet_summaries %>%
  filter(stat == 'RMSE') %>%
  filter(value==min(value)) %>%
  select(lambda,fraction)

enet_plot <- enet_preds %>%
  mutate(lambda = as.double(lambda),
         fraction = as.double(fraction)) %>%
  inner_join(enet_choose) %>%
  ggplot(aes(x=fat,y=value)) + geom_point() + geom_abline()

ggsave(plot = enet_plot,
       filename = 'enet_plot.png',
       width = 6,
       height = 4,
       dpi = 100)

# so look at how good manually created trainings are : 

lm_pred_accuracy %>%
  bind_rows(pls_pred_accuracy,
            ridge_pred_summaries,
            lasso_summaries,
            enet_summaries) %>%
  filter(stat == 'RMSE') %>%
  group_by(model) %>%
  filter(value == min(value)) %>%
  mutate(loglambda = log10(lambda)) %>% 
  select(-lambda) %>% 
  knitr::kable(.)

# now try the caret approach to determine tuning parameters 

CTC <- caret::trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5)

caret_lm <- caret::train(x = TrainAbsorpPP, 
                         y = TrainFat %>% pull(fat),
                         method = "lm",
                         trControl = CTC)

caret_pls <- caret::train(x = TrainAbsorpPP,
                          y = TrainFat %>% pull(fat),
                          method = "pls",
                          tuneLength = 100,
                          trControl = CTC)

caret_ridge <- caret::train(x = TrainAbsorpPP,
                            y = TrainFat %>% pull(fat),
                            method = "ridge",
                            trControl = CTC,
                            tuneGrid = data.frame(lambda = ridge_lambdas))

caret_lasso <- caret::train(x = TrainAbsorpPP,
                            y = TrainFat %>% pull(fat),
                            method = "lasso",
                            trControl = CTC,
                            tuneGrid = data.frame(fraction = lasso_lambdas))

caret_enet <- caret::train(x = TrainAbsorpPP,
                           y = TrainFat %>% pull(fat),
                           method = "enet",
                           trControl = CTC,
                           tuneGrid = tibble(fraction = enet_fractions) %>%
                             crossing(lambda = enet_lambdas) %>%
                             as.data.frame)

carets <- list(lm = caret_lm,
               lasso = caret_lasso,
               pls = caret_pls,
               ridge = caret_ridge,
               enet = caret_enet)

rcv_outputs <- carets %>% 
  purrr::map_df(~ .$results %>%
                  as.tibble,
                .id = 'model') %>% 
  mutate(RsquaredSE = RsquaredSD / sqrt(5 * 10))

rcv_outputs %>% distinct(model)

rcv_outputs %>%
  filter(model == 'lm')

kcv_lasso_plot <- rcv_outputs %>%
  filter(model == 'lasso') %>%
  select(RMSE,RsquaredSE,fraction) %>%
  gather(key=stat,value=value,-fraction) %>%
  ggplot(aes(x=fraction,y=value,colour=stat)) + 
  geom_point() + 
  scale_x_log10()

ggsave(plot = kcv_lasso_plot,
       filename = 'kcv_lasso_plot.png',
       width = 6, 
       height = 4,
       dpi = 100)

kcv_pls_plot <- rcv_outputs %>%
  filter(model == 'pls') %>%
  select(RMSE,RsquaredSE,ncomp) %>%
  gather(key=stat,value=value,-ncomp) %>%
  ggplot(aes(x=ncomp,y=value,colour=stat)) + 
  geom_point()

ggsave(plot = kcv_pls_plot,
       filename = 'kcv_pls_plot.png',
       width = 6, 
       height = 4,
       dpi = 100)

kcv_ridge_plot <- rcv_outputs %>%
  filter(model == 'ridge') %>%
  select(RMSE,RsquaredSE,lambda) %>% 
  gather(key=stat,value=value,-lambda) %>%
  ggplot(aes(x=lambda,y=value,colour=stat)) + 
  geom_point() + 
  scale_x_log10()

ggsave(plot = kcv_ridge_plot,
       filename = 'kcv_ridge_plot.png',
       width = 6, 
       height = 4,
       dpi = 100)

kcv_enet_plot <- rcv_outputs %>%
  filter(model == 'enet') %>%
  select(RMSE,fraction,lambda) %>%
  filter(lambda %in% 10 ** seq(-15,-5,1)) %>%
  mutate(lambda = as.factor(lambda)) %>%
  ggplot(aes(x=fraction,y=RMSE,colour=lambda)) +
  geom_point() + geom_line() +
  scale_y_continuous(limits=c(1,5))

ggsave(plot = kcv_enet_plot,
       filename = 'kcv_enet_plot.png',
       width = 6, 
       height = 4,
       dpi = 100)

caret_predictions <- carets %>%
  purrr::map_df(~ predict(., TestAbsorpPP) %>%
                  as.tibble %>%
                  bind_cols(TestFat),
                .id='id') %>%
  rename(pred = value, obs = fat)

caret_summary <- caret_predictions %>%
  group_by(id) %>%
  do(bind_rows(caret::defaultSummary(data.frame(.)))) 

knitr::kable(caret_summary)

caret_plot <- caret_predictions %>%
  ggplot(aes(x=obs,y=pred))+geom_point() + geom_abline() + facet_wrap(~ id) +
  ggtitle("Predictions on test set from caret workflow")

ggsave(plot = caret_plot,
       filename = 'caret_plot.png',
       width = 6,
       height = 4,
       dpi = 100)

# ok reporting time: do some coef plots 

coef_plot_lm <- coef(lm_fit) %>%
  enframe %>%
  mutate(name = str_replace(name, "V", "") %>% as.integer) %>%
  ggplot(aes(x=name,y=value)) + geom_point()

ggsave(plot = coef_plot_lm,
       filename = 'coef_plot_lm.png',
       width = 6,
       height = 4,
       dpi = 100)

coef_plot_ridge<-coef(ridge_fit) %>%
  as.data.frame %>%
  rownames_to_column %>%
  gather(key=key,value=value,-rowname) %>%
  as.tibble %>%
  mutate(rowname = as.double(rowname),
         key = as.integer(str_replace(key,"V",''))) %>%
  group_by(rowname) %>%
  ggplot(aes(x=rowname,y=value, colour = key, group = key)) + geom_line() +
  scale_x_log10(breaks = 10 ** seq(-15,-1,1),
                limits = c(1e-10,1e-5)) 

ggsave(plot = coef_plot_ridge,
       filename = 'coef_plot_ridge.png',
       width = 6,
       height = 4,
       dpi = 100)

coef_plot_lasso <- lasso_coeffs %>%
  as.matrix %>% 
  as.data.frame %>%
  rownames_to_column %>%
  mutate(rowname = as.integer(str_replace(rowname,"V",""))) %>% 
  as.tibble %>%
  gather(key=key,value=value,-rowname) %>%
  mutate(key=as.double(key)) %>% 
  ggplot(aes(x = key, y = value, colour = rowname, group = rowname)) + geom_line()

ggsave(plot = coef_plot_lasso,
       filename = 'coef_plot_lasso.png',
       width = 6,
       height = 4, 
       dpi = 100)


