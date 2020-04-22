library(tidyverse)
library(caret)
library(C50)
data(churn)

output_directory <- file.path(getwd(),'18.1')

training <- churnTrain %>%
  mutate(total_minutes = total_day_minutes + total_eve_minutes + total_night_minutes + total_intl_minutes,
         total_charge = total_day_charge + total_eve_charge + total_night_charge + total_intl_charge,
         total_calls = total_day_calls + total_eve_calls + total_night_calls + total_intl_calls)

corr_plot <-training %>%
  select(-churn,-area_code,-state,-international_plan,-voice_mail_plan) %>%
  cor %>%
  ggcorrplot::ggcorrplot(.)

ggsave(plot = corr_plot, 
       filename = file.path(output_directory, 'corr_plot.png'),
       width = 8, height = 8, dpi = 100)

individual_importances <- caret::filterVarImp(x = training %>% select(-churn),
                                              y = training %>% pull(churn)) %>%
  rownames_to_column %>%
  as_tibble %>%
  gather(key,value,-rowname) %>%
  filter(key == 'yes') %>%
  mutate(rank = rank(-value,ties.method = 'first')) %>%
  arrange(rank)

joint_importances <- CORElearn::attrEval( churn ~ ., 
                                          training, 
                                          'ReliefFequalK', 
                                          ReliefIterations = 500) %>% 
  enframe %>%
  mutate(rank = rank(-value, ties.method = 'first')) %>%
  arrange(rank)

compare_plot <- individual_importances %>%
  select(name = rowname, rank) %>%
  full_join(joint_importances %>% 
              select(name,rank),
            by = 'name',
            suffix = c('.ind','.joint')) %>%
  ggplot(aes(x=rank.ind,y=rank.joint)) + 
  geom_label(aes(label=name))

ggsave(plot = compare_plot, 
       filename = file.path(output_directory, 'compare_plot.png'),
       width = 8, height = 8, dpi = 100)

knitr::kable(joint_importances)



  
