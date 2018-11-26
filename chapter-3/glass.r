library(tidyverse)
library(mlbench) # data
library(e1071) # skewness
data(Glass)

# ok the factor is the class so don't mutate it

glass <- Glass %>% 
  as.tibble %>%
  mutate(id = rownames(Glass) %>% as.integer)

# any significant skew ? 

glass %>%
  select(-id) %>%
  summarise_all(funs(skewness)) %>%
  gather(key=predictor,value=skewness) %>%
  mutate(skewness_rank = dense_rank(abs(skewness))) %>%
  arrange(-skewness_rank)

glass %>%
  gather(key=predictor,value=value,-id,-Type) %>%
  ggplot(aes(x=value)) + geom_density() + facet_wrap(~predictor,scales="free")

# some rare high K samples, let's have a look

glass %>% 
  filter(K > 2)
