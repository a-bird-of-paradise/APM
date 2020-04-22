library(tidyverse)
library(caret)

library(AppliedPredictiveModeling)

data("abalone")

output_directory <- file.path(getwd(),'18.3')

type_plot <- abalone %>%
  select(Type,Rings) %>%
  ggplot(aes(x = Type, y = Rings, group = Type)) + geom_boxplot() + ggtitle('Type')

the_names <- setdiff(names(abalone), c('Type','Rings')) %>% `names<-`(.,.)

one_way_plotter_numeric <- function(dataset, zzz) 
{
  data_summary <- dataset %>%
    select(!!sym(zzz), Type, Rings) %>%
    mutate(bin = cut(!!sym(zzz), breaks = 30, ordered_result = T)) 
  
  xlabs <- data_summary %>% group_by(bin) %>% summarise(xpos = mean(!!sym(zzz))) %>% ungroup
  
  counts <- data_summary %>%
    group_by(bin,Type) %>%
    summarise(n = n())%>%
    ungroup %>%
    inner_join(xlabs , by = 'bin')
  
  rings <- data_summary %>% 
    group_by(bin,Type) %>%
    summarise(Rings = mean(Rings)) %>%
    ungroup %>%
    inner_join(xlabs, by = 'bin')
  
  rings_scale <- max(counts %>% group_by(bin) %>% summarise(n=sum(n)) %>% pull(n)) /
    max(rings$Rings)
  
  rings <- rings %>% mutate(Rings = Rings * rings_scale) 
  
  the_plot <- counts %>%
    ggplot(aes(x = xpos, y = n, colour = Type)) + 
    geom_bar(stat = 'identity', aes(fill = Type), position = 'dodge') +
    geom_line(data = rings, aes(x = xpos, y = Rings, colour = Type, group = Type)) + 
    geom_point(data = rings, aes(x = xpos, y = Rings, colour = Type)) +
    scale_y_continuous(sec.axis = sec_axis(~ . / rings_scale, name = 'Rings'), 
                       labels = scales::number_format(big.mark = ',', accuracy = 1)) + 
    scale_x_continuous(name = zzz) +
    ggtitle(zzz)
  
  return(the_plot)
}

plots <- the_names %>% purrr::map(one_way_plotter_numeric, 
                                  dataset = abalone %>% mutate(Height = pmin(0.3,Height)))

the_names %>% purrr::map(~ ggsave(plot = plots[[.x]],
                                  filename = file.path(output_directory,
                                                       paste0(.x,'.png')),
                                  width = 8, height = 6, dpi = 100))

corr_plot <- abalone %>%
  select(-Type) %>%
  cor %>%
  ggcorrplot::ggcorrplot(.)

ggsave(plot = corr_plot,
       filename = file.path(output_directory, 'corr_plot.png'),
       width = 6, height = 6, dpi = 100)

varimp <- caret::filterVarImp(x = abalone %>% select(-Rings), 
                              y = abalone %>% pull(Rings))

varimp %>% rownames_to_column %>% arrange(-Overall) %>% knitr::kable(.)

abalone %>% 
  select(-Type,-Rings) %>% 
  prcomp 
