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

(glass %>% mutate(Type = as.integer(Type)) %>%
    gather(key=predictor,value=value,-id) %>%
    ggplot(aes(x=value)) + geom_density() + facet_wrap(~predictor,scales="free")) %>%
  ggsave(filename = "all.png", width = 8, height = 6)

# some rare high K samples, let's have a look

glass %>% 
  filter(K > 2)

# ok dump density plots by type to disk for inspection

glass %>% 
  distinct(Type) %>% 
  pull %>%
  map(~ (
    glass %>%
      filter(Type == .x) %>%
      select(-Type) %>%
      gather(key=predictor,value=value,-id) %>%
      ggplot(aes(x=value)) + geom_density() + facet_wrap(~predictor,scales="free") 
  ) %>%
    ggsave(plot = .,
           filename = paste0(.x,".png"),
           width = 8,
           height = 6))

# correlations, total and within type

(glass %>% 
  select(-id,-Type) %>%
  cor(method = "spearman") %>%
  ggcorrplot::ggcorrplot(method = "circle") + 
  ggtitle("Total")) %>%
  ggsave(plot = ., filename = "corr_total.png",height=8,width=8)

glass %>% distinct(Type) %>% pull(Type) %>%
  purrr::map( ~ (glass %>%
                filter(Type == .x) %>%
                select(-id,-Type) %>%
                cor(method="spearman") %>%
                ggcorrplot::ggcorrplot(method = "circle") +
                  ggtitle(paste0("corr_",.x))) %>%
                ggsave(plot = .,
                       filename = paste0("corr_",.x,".png"),
                       height = 8,
                       width = 8))
  

              