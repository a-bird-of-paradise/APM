library(tidyverse)
library(caret)
data(oil)

set.seed(123987)

sample_ids <- 1:10
names(sample_ids) <- sample_ids

resample_base_r_plot <- sample_ids %>%
  purrr::map_df(~ sample(oilType, size = 60) %>% as.tibble,
                .id = "id") %>%
  bind_rows( oilType %>% as.tibble %>% mutate(id="all")) %>%
  mutate(id = forcats::as_factor(id)) %>%
  group_by(id,value) %>%
  summarise(n=n()) %>%
  ungroup %>%
  group_by(id) %>%
  mutate(share = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(x=value,y=share,fill=id)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("base R resample")

ggsave(plot = resample_base_r_plot,
       filename = "resample_base_r_plot.png",
       width = 5,
       height = 4,
       dpi = 100)

resample_caret_plot <- caret::createDataPartition(oilType, times = 10, p = 60/96, list = F) %>%
  as.tibble %>%
  gather(key=id,value=value) %>%
  mutate(value = oilType[value])  %>%
  bind_rows( oilType %>% as.tibble %>% mutate(id="all")) %>%
  mutate(id = forcats::as_factor(id)) %>%
  group_by(id,value) %>%
  summarise(n=n()) %>%
  ungroup %>%
  group_by(id) %>%
  mutate(share = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(x=value,y=share,fill=id)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("caret resample")

ggsave(plot = resample_caret_plot,
       filename = "rresample_caret_plot.png",
       width = 5,
       height = 4,
       dpi = 100)
