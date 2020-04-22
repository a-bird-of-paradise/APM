library(tidyverse)
library(caret)

output_directory <- file.path(getwd(),'18.2')

data(oil)

answer <- caret::filterVarImp(fattyAcids,oilType) %>%
  rownames_to_column %>%
  rename(predictor = 'rowname') %>%
  gather(key = oiltype, value = value, -predictor) %>%
  ggplot(aes(x = predictor, y = value, colour = oiltype, group = oiltype)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent)

ggsave(plot = answer,
       filename = file.path(output_directory,'answer.png'),
       width = 8, height = 6, dpi = 100)
