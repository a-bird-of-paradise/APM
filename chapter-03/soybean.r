library(tidyverse)
library(e1071)
library(ggcorrplot)
library(mlbench)
library(caret)
data("Soybean")

setdiff(names(Soybean),"Class") %>%
  map(~ (ggplot(data = Soybean,
                aes(x=!!sym(.x),
                    fill = Class)) + 
           geom_histogram(stat="count", position = "stack")) %>%
        ggsave(plot = .,
               filename = paste0("soybean_hist_",.x,".png"),
               width = 8,
               height = 6))

# ok looking for interesting or otherwise informative data points here: 
# crop.hist
# seems to be that nas are predictive; lets have a look...
Soybean %>%
  select(Class,crop.hist) %>%
  filter(is.na(crop.hist)) %>%
  group_by(Class) %>%
  summarise(n=n())
# yup - na has power. look at them all...

na.summary <- Soybean %>%
  group_by(Class) %>%
  summarise_all(funs(sum(ifelse(is.na(.),1,0)))) %>%
  gather(key=key,value=value,-Class) %>%
  filter(value!=0) 

# so seems like certain missings are predictive of other things. suggests a tree based 
# approach so we can tease out informative NAs. 

na.summary %>%
  group_by(key) %>%
  summarise(value=sum(value)) %>%
  arrange(-value)

# ok how to handle missing data... 
# any predictors we can just remove?

nearZero <- names(Soybean)[caret::nearZeroVar(Soybean)]

Soybean %>%
  select(Class,nearZero) %>%
  gather(key=key,value=value,-Class) %>%
  group_by(Class,key,value) %>%
  summarise(n=n()) %>%
  spread(key=key,value=n) %>%
  filter(value!=0) %>%
  print(n=nrow(.))

# ok see if the three conditions have other things going on...

Soybean %>%
  filter(Class == "charcoal-rot")
Soybean %>%
  filter(sclerotia == 1) # retain this - diagnostic of charcoal-rot 

Soybean %>% 
  filter(Class %in% c("downy-mildew","powdery-mildew")) %>%
  group_by(Class,leaf.mild) %>%
  summarise(n=n())

Soybean %>%
  filter(leaf.mild != 1 & leaf.mild != 2) %>%
  group_by(Class) %>%
  summarise(n=n()) # retain this - diagnostic of the mildew types 

Soybean %>%
  filter(Class == "rhizoctonia-root-rot")

Soybean %>% filter(mycelium != 0) # remove this - other factors seem better

remove.predictors <- c("mycelium")

# now NA as a factor level is itself OK - so no need to fill in
# promote NA to its own level and make dummy vars 
# also some "ordinal" factors are really nominal in disguise
# so demote to normal factor to avoid extracting polynomial predictors

nominal.factors <- c("plant.stand","precip","temp","germ","leaf.size")

Soybean_P <- Soybean %>%
  as.tibble %>%
  mutate_at(vars(nominal.factors), as.integer) %>%
  mutate_at(vars(nominal.factors), as.factor) %>%
  mutate_if(is.factor,addNA) %>%
  select(-remove.predictors)

Soybean_Data <- Soybean_P %>%
  select(Class) %>%
  bind_cols(
    predict(caret::dummyVars( Class ~ ., data = Soybean_P),
            newdata = Soybean_P) %>% 
      as.tibble)

# now NA is treated as its own factor level
# because NA is predictive in some circumstances
# so could use e.g. trees to model this 