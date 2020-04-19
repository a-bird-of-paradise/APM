one_way_plotter_numeric <- function(dataset, zzz) 
{
  data_summary <- dataset %>%
    select(!!sym(zzz), RESP) %>%
    mutate(bin = cut(!!sym(zzz), breaks = 30, ordered_result = T)) 
  
  xlabs <- data_summary %>% group_by(bin) %>% summarise(xpos = mean(!!sym(zzz))) %>% ungroup
  
  counts <- data_summary %>%
    group_by(bin,RESP) %>%
    summarise(n = n())%>%
    ungroup %>%
    inner_join(xlabs , by = 'bin')
  
  rates <- data_summary %>% 
    group_by(bin) %>%
    summarise(rate = sum(ifelse(RESP == 'true', 1, 0)) / n()) %>%
    ungroup %>%
    inner_join(xlabs, by = 'bin')
  
  rates_scale <- max(counts %>% group_by(bin) %>% summarise(n=sum(n)) %>% pull(n)) / max(rates$rate)
  
  rates <- rates %>% mutate(rate = rate * rates_scale) 
  
  the_plot <- counts %>%
    ggplot(aes(x = xpos, y = n)) + 
    geom_bar(stat = 'identity', aes(fill = RESP)) +
    geom_line(data = rates, aes(x = xpos, y = rate)) + 
    geom_point(data = rates, aes(x = xpos, y = rate)) +
    scale_y_continuous(sec.axis = sec_axis(~ . / rates_scale, name = 'Response Rate',
                                           labels = scales::percent_format(accuracy = 1)), 
                       labels = scales::number_format(big.mark = ',', accuracy = 1)) + 
    scale_x_continuous(name = zzz, labels = scales::number_format(big.mark = ',', accuracy = 1)) +
    ggtitle(zzz)
  
  return(the_plot)
}

one_way_plotter_factor <- function(dataset, zzz) 
{
  data_summary <- dataset %>%
    select(!!sym(zzz), RESP) 
  
  counts <- data_summary %>%
    group_by(!!sym(zzz),RESP) %>%
    summarise(n = n())%>%
    ungroup 
  
  rates <- data_summary %>% 
    group_by(!!sym(zzz)) %>%
    summarise(rate = sum(ifelse(RESP == 'true', 1, 0)) / n()) %>%
    ungroup 
  
  rates_scale <- max(counts %>% 
                       group_by(!!sym(zzz)) %>% 
                       summarise(n=sum(n)) %>% 
                       pull(n)) / max(rates$rate)
  
  rates <- rates %>% mutate(rate = rate * rates_scale) 
  
  the_plot <- counts %>%
    ggplot(aes(x = !!sym(zzz), y = n)) + 
    geom_bar(stat = 'identity', aes(fill = RESP)) +
    geom_line(data = rates, aes(x = !!sym(zzz), y = rate, group = 1)) + 
    geom_point(data = rates, aes(x = !!sym(zzz), y = rate)) +
    scale_y_continuous(sec.axis = sec_axis(~ . / rates_scale, name = 'Response Rate',
                                           labels = scales::percent_format(accuracy = 1)), 
                       labels = scales::number_format(big.mark = ',', accuracy = 1)) + 
    scale_x_discrete(name = zzz) +
    ggtitle(zzz)
  
  return(the_plot)
}

one_way_plotter <- function(dataset, zzz)
{
  type <- is.numeric(dataset %>% pull(!!sym(zzz)))
  
  if(type == T)
  {
    return(one_way_plotter_numeric(dataset,zzz))
  }
  
  return(one_way_plotter_factor(dataset,zzz))
}

# cloned from https://stackoverflow.com/questions/58501227/r-caret-plyr-how-to-modify-downsample-function-to-create-sampled-data-of-diffe

downSample_custom <- function(x, y, yname = "Class", frac = 1){
  lev <- levels(y)
  minClass <- min(table(y))
  lev_min <- levels(y)[which.min(table(y))]
  inds_down <- sample(which(y == lev[lev != lev_min]), size = minClass * frac) #sample the indexes of the more abundant class according to minClass * frac
  inds_minClass <- which(y == lev[lev == lev_min]) #take all the indexes of the lesser abundant class
  out <- data.frame(x, y)
  out <- out[sort(c(inds_down, inds_minClass)),]
  colnames(out)[ncol(out)] <- yname
  return(out)
} 
