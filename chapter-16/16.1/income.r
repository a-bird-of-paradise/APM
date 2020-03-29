library(tidyverse)
library(caret)
library(arules)

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
                                                ifelse(`capital-loss` < 5000, 'Some', 'Lots')))))


summary(data_t)
# want to split data into training / testing using income... 

TrainingIndices <- caret::createDataPartition(y = data_t$income,
                                              p = 0.7)$Resample1

TrainingPredictors <- data_t[TrainingIndices,] %>% select(-income)
TrainingOutcome <- data_t[TrainingIndices,] %>% select(income)

TestingPredictors <- data_t[-TrainingIndices,] %>% select(-income)
TestingOutcome <- data_t[-TrainingIndices,] %>% select(income)

# examine one ways

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

paste0('![img](16.1/',names(TrainingPredictors),'.png)\n') %>% cat
