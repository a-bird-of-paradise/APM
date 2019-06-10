library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
data("ChemicalManufacturingProcess")

set.seed(918273645)

Predictors <- ChemicalManufacturingProcess %>% select(-Yield)
Response <- ChemicalManufacturingProcess %>% pull(Yield)

x <- caret::train(Predictors,
                  Response,
                  method = "pls",
                  tuneLength = 10,
                  preProcess = c("pca","knnImpute"),
                  trControl = caret::trainControl(method = "repeatedcv",
                                                  repeats = 5))

data <- x$results %>%
  select(ncomp,Rsquared,RsquaredSD) %>%
  mutate(RsquaredSE = RsquaredSD / sqrt(5 * 10)) %>%
  select(-RsquaredSD) %>%
  mutate(source = "me") %>%
  as.tibble %>%
  bind_rows(tribble(
    ~ncomp     ,~Rsquared , ~RsquaredSE  
    ,          1 ,    0.444 ,      0.0272 
    ,          2 ,    0.500 ,      0.0298 
    ,          3 ,    0.533 ,      0.0302 
    ,          4 ,    0.545 ,      0.0308 
    ,          5 ,    0.542 ,      0.0322 
    ,          6 ,    0.537 ,      0.0327 
    ,          7 ,    0.534 ,      0.0333 
    ,          8 ,    0.534 ,      0.0330
    ,          9 ,    0.520 ,      0.0326 
    ,         10 ,    0.507 ,      0.0324 
  ) %>%
    mutate(source = "book")) %>%
  gather(key=key,value=value,-source,-ncomp)
  
data %>%  
  ggplot(aes(x=ncomp,y=value,colour=source))+
  geom_point()+
  geom_line()+
  facet_grid(key ~ .,scales="free")+
  scale_x_continuous(limits = c(1,10), breaks = 1:10)

wanted_rows <- data %>%
  group_by(source) %>%
  filter(key == "Rsquared") %>%
  filter(value==max(value)) %>%
  select(ncomp,source) %>%
  ungroup

data %>%
  inner_join(wanted_rows,
             by=c("ncomp","source")) %>%
  spread(key=key,value=value) %>%
  mutate(threshold = Rsquared - RsquaredSE) %>%
  select(source,threshold) %>%
  inner_join(data,
             by = c("source")) %>%
  filter(value >= threshold & key == "Rsquared") %>%
  group_by(source) %>%
  filter(ncomp == min(ncomp)) %>%
  ungroup %>%
  select(source,ncomp)
