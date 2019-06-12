library(tidyverse)
library(caret)

genresTrain <- readr::read_csv("genresTrain.csv") %>%
  mutate_if(is.character,as.factor)

# any bias in observations?

genresTrain %>%
  group_by(GENRE) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=GENRE,y=n))+geom_bar(stat="identity")

# what is the dist of preds by genre? 

predictor.names <- setdiff(names(genresTrain),"GENRE")

predictor.names %>%
  purrr::map(~ ggsave(filename = paste0(.x,".png"),
                      width = 8,
                      height = 6,
                      dpi = 100,
                      plot = genresTrain %>%
                        ggplot(aes(x=!!sym(.x))) + 
                        geom_density() + 
                        facet_wrap(~GENRE)))

# predictors look homogenous (admittedly, not pre processed yet)
# ok suggest doing stratified sampling in preparation for k fold cross validation,
# regardless of whatever 

data_parts <- caret::createDataPartition(y = genresTrain$GENRE,
                                         times = 10,
                                         p = 0.8)
names(data_parts) %>%
  purrr::map(~ ggsave(filename = paste0("part_genre_",.x,".png"),
                      width = 8,
                      height = 6,
                      dpi = 100,
                      plot = genresTrain[data_parts[[.x]],] %>% 
                        group_by(GENRE) %>%
                        summarise(n=n()) %>%
                        ggplot(aes(x=GENRE,y=n))+geom_bar(stat="identity")))
