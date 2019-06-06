library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
data("permeability")

fingerprints
permeability

fingerprints %>%
  as.tibble  %>%
  rowid_to_column %>%
  gather(key=key,value=value,-rowid) %>%
  select(-rowid) %>%
  group_by(key) %>%
  summarise_all(funs(mean,sd,min,max)) %>%
  arrange(-mean)
# ok plenty of zero variance predictors. remove them before anything else happens

low_var_names <- names(fingerprints %>% as.tibble)[caret::nearZeroVar(fingerprints)]

fing_ex_low_var <- fingerprints %>%
  as.tibble %>%
  select(-low_var_names)

corrs <- fing_ex_low_var %>% 
  cor(method = "pearson")

remove_names <- union(names(fing_ex_low_var)[caret::findCorrelation(corrs)],
                      low_var_names)

fingerprints %>%
  as.tibble %>%
  select(-remove_names) %>%
  cor(method = "pearson") %>%
  ggcorrplot::ggcorrplot()

parts <- caret::createDataPartition(permeability, times = 10, p = 0.8)
parts_all <- append(parts,
                    list(all = 1:nrow(permeability)))

names(parts_all) %>%
  purrr::map_df(~ permeability[parts_all[[.x]]] %>%
                  as.tibble %>%
                  mutate(id=.x)) %>%
  ggplot(aes(x=value,colour=id,fill=id)) + geom_density(alpha=0.1)
