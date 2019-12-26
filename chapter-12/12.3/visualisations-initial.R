library(tidyverse)
library(caret)
library(C50)
data(churn)

output_directory <- file.path(getwd(),'12.3')
dir.create(output_directory,showWarnings = F)

# ok let us do some one ways 

predictor_names <- setdiff(names(churnTrain),'churn')

predictors <- churnTrain[predictor_names]

# state: any obvious geographic thing

datum <- tibble(x = predictors$state,
                y = churnTrain$churn)

mapping <- read_csv(file.path(output_directory,"state-mapping.csv"))

state_data <- datum %>%
  mutate(y = ifelse(y == "yes",1.0,0.0)) %>%
  group_by(x) %>%
  summarise(y=mean(y), n = n()) %>%
  mutate(x = as.character(x)) %>%
  left_join(mapping,
            by = c('x'='Abbreviation')) %>%
  mutate(region = tolower(State)) %>%
  select(region,y,n)

state_plot <- ggplot() +
  geom_map(data = map_data('state'), map = map_data('state'),
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) + 
  geom_map(data = state_data, map = map_data('state'),
           aes(fill = y, map_id = region),
           color="#ffffff", size=0.15) + 
  scale_fill_continuous(low='thistle2', high='darkred',guide='colorbar') + 
  labs(x=NULL, y=NULL) + 
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme(panel.border = element_blank())  + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank())  + 
  theme(axis.text = element_blank())

ggsave(plot = state_plot,
       filename = file.path(output_directory, 'state_plot.png'),
       width = 8,
       height = 6,
       dpi = 100)

# state matters. why? maybe regulation, marketing campaigns, ...? next up: acc length

datum <- tibble(x = predictors$account_length,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('account_length')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'account_length.png'),
       width = 8, height = 6, dpi = 100)

# hmm not very much on a one way...

datum <- tibble(x = predictors$area_code,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  group_by(x) %>%
  summarise(y=mean(y), n = n()) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('area_code')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'area_code.png'),
       width = 8, height = 6, dpi = 100)

# negligible one way influence. next up 

datum <- tibble(x = predictors$international_plan,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  group_by(x) %>%
  summarise(y=mean(y), n = n()) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('international_plan')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'international_plan.png'),
       width = 8, height = 6, dpi = 100)

# international plan is important! 

datum <- tibble(x = predictors$voice_mail_plan,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  group_by(x) %>%
  summarise(y=mean(y), n = n()) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('voice_mail_plan')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'voice_mail_plan.png'),
       width = 8, height = 6, dpi = 100)

# Also important, but in the other way surprisingly to me at least. 

datum <- tibble(x = predictors$number_vmail_messages,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('number_vmail_messages')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'number_vmail_messages.png'),
       width = 8, height = 6, dpi = 100)

# hard to say. maybe if other things are put in perhaps 

datum <- tibble(x = predictors$total_day_minutes,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_day_minutes')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_day_minutes.png'),
       width = 8, height = 6, dpi = 100)

# ok this is strong, looks like a hinge around 200min

datum <- tibble(x = predictors$total_day_calls,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_day_calls')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_day_calls.png'),
       width = 8, height = 6, dpi = 100)

# hmm not very interesting

datum <- tibble(x = predictors$total_day_charge,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_day_charge')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_day_charge.png'),
       width = 8, height = 6, dpi = 100)

# very powerful, suspect strong correlation between this and calls though:

tibble(x = predictors$total_day_charge, y = predictors$total_day_calls) %>%
  cor

# hmm no!

ggsave(plot = tibble(total_day_charge=predictors$total_day_charge, 
       total_day_calls=predictors$total_day_calls) %>%
  ggplot(aes(x=total_day_charge,y=total_day_calls))+
  stat_bin2d(),
  filename = file.path(output_directory,'total_day_charge_vs_calls.png'),
  width = 8, height = 8, dpi = 100)
                       
ggsave(plot = tibble(total_day_charge=predictors$total_day_charge, 
       total_day_calls=predictors$total_day_calls,
       churn = churnTrain$churn) %>%
  mutate(churn = ifelse(churn == 'yes', 1, 0)) %>%
  ggplot(aes(x = total_day_charge, y= total_day_calls, z = churn)) + 
  stat_summary_2d(),
  filename = file.path(output_directory,'total_day_charge_vs_calls_churn.png'),
  width = 8, height = 8, dpi = 100)


# so interesting - definitely more churny at the fringes 

datum <- tibble(x = predictors$total_eve_minutes,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_eve_minutes')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_eve_minutes.png'),
       width = 8, height = 6, dpi = 100)

# ok this is strong, looks like a hinge around 200min

datum <- tibble(x = predictors$total_eve_calls,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_eve_calls')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_eve_calls.png'),
       width = 8, height = 6, dpi = 100)

# straight line again 

datum <- tibble(x = predictors$total_eve_charge,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_eve_charge')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_eve_charge.png'),
       width = 8, height = 6, dpi = 100)

# straight line effect here!



datum <- tibble(x = predictors$total_night_minutes,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_night_minutes')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_night_minutes.png'),
       width = 8, height = 6, dpi = 100)

# ok this is strong, looks like a hinge around 200min

datum <- tibble(x = predictors$total_night_calls,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_night_calls')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_night_calls.png'),
       width = 8, height = 6, dpi = 100)

# straight line again 

datum <- tibble(x = predictors$total_night_charge,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_night_charge')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_night_charge.png'),
       width = 8, height = 6, dpi = 100)

# straight line effect here!

datum <- tibble(x = predictors$total_intl_minutes,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_intl_minutes')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_intl_minutes.png'),
       width = 8, height = 6, dpi = 100)

# ok this is strong, looks like a hinge around 200min

datum <- tibble(x = predictors$total_intl_calls,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_intl_calls')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_intl_calls.png'),
       width = 8, height = 6, dpi = 100)

# straight line again 

datum <- tibble(x = predictors$total_intl_charge,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_intl_charge')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_intl_charge.png'),
       width = 8, height = 6, dpi = 100)

# straight line effect here!



datum <- tibble(x = predictors$number_customer_service_calls,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0))

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('number_customer_service_calls')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'number_customer_service_calls.png'),
       width = 8, height = 6, dpi = 100)

# ok. now try "totals"...

datum <- tibble(x1 = predictors$total_day_minutes,
                x2 = predictors$total_eve_minutes,
                x3 = predictors$total_night_minutes,
                x4 = predictors$total_intl_minutes,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0)) %>%
  mutate(x=x1+x2+x3+x4) %>%
  select(x,y)

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_minutes')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_minutes.png'),
       width = 8, height = 6, dpi = 100)


datum <- tibble(x1 = predictors$total_day_charge,
                x2 = predictors$total_eve_charge,
                x3 = predictors$total_night_charge,
                x4 = predictors$total_intl_charge,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0)) %>%
  mutate(x=x1+x2+x3+x4) %>%
  select(x,y)

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_charge')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_charge.png'),
       width = 8, height = 6, dpi = 100)


datum <- tibble(x1 = predictors$total_day_calls,
                x2 = predictors$total_eve_calls,
                x3 = predictors$total_night_calls,
                x4 = predictors$total_intl_calls,
                y = churnTrain$churn) %>%
  mutate(y = ifelse(y == 'yes',1.0,0.0)) %>%
  mutate(x=x1+x2+x3+x4) %>%
  select(x,y)

the_plot <- datum %>%
  mutate(xb = cut(x,30,ordered_result = T)) %>%
  group_by(xb) %>%
  summarise(y = mean(y),n=n(), x =mean(x)) %>%
  ggplot(aes(x=x,y=y,size=n))+geom_point()+ggtitle('total_calls')

ggsave(plot = the_plot,
       filename = file.path(output_directory,'total_calls.png'),
       width = 8, height = 6, dpi = 100)

predictors %>%
  select_if(is.numeric) %>%
  cor(method = 'spearman')

# so some almost perfectly correlated predictors to filter out 
# plus try putting in a floor-0 shift -200 on day calls
# and a marker on charge >= 75
