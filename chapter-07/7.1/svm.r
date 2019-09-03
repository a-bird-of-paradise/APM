library(tidyverse)
library(caret)
library(kernlab)

output_directory <- file.path(getwd(),"7.1")

set.seed(99283731)

x <- runif(100,min=2,max=10)

y <- sin(x) + rnorm(length(x))*0.25

simData <- tibble(x=x,y=y)

simPlot <- ggplot(simData,aes(x=x,y=y)) + geom_point()

ggsave(simPlot,
       filename = file.path(output_directory,"simPlot.png"),
       width = 8,
       height = 6,
       dpi = 100)

xGrid <- tibble(x = seq(2,10,length = 100))

trueData <- xGrid %>% mutate(true.y=sin(x))

testSVM <- ksvm(x=x,
                y=y,
                data = simData,
                kernel = "rbfdot",
                kpar = "automatic",
                C = 1,
                epsilon = 0.1)

Cs <- 2 ** (-4:4) %>% `names<-`(.,.)

CSVMs <- Cs %>%
  purrr::map_df(~ predict(ksvm(x=x,
                            y=y,
                            data = simData,
                            kernel = "rbfdot",
                            kpar = "automatic",
                            C = .x,
                            epsilon = 0.1),
                       newdata = xGrid) %>%
               as.vector
  )


svmCPlot <- xGrid %>%
  bind_cols(CSVMs) %>%
  gather(key=key,value=value,-x) %>%
  mutate(key=forcats::as_factor(key)) %>%
  left_join(trueData,by=c("x")) %>%
  ggplot(aes(x=x,y=value,colour=key)) + geom_point() + facet_wrap(~ key) +
  geom_line(aes(x=x,y=true.y),colour="black")

ggsave(svmCPlot,
       filename = file.path(output_directory,"svm-c-inv.png"),
       width = 8,
       height = 6,
       dpi = 100)

epsilons <- 10 ** (-8:0) %>% `names<-`(.,.)

eSVMs <- epsilons %>%
  purrr::map_df(~ predict(ksvm(x=x,
                               y=y,
                               data = simData,
                               kernel = "rbfdot",
                               kpar = "automatic",
                               C = 1,
                               epsilon = .x),
                          newdata = xGrid) %>%
                  as.vector
  )

svmePlot <- xGrid %>%
  bind_cols(eSVMs) %>%
  gather(key=key,value=value,-x) %>%
  mutate(key=forcats::as_factor(key)) %>%
  left_join(trueData,by=c("x")) %>%
  ggplot(aes(x=x,y=value,colour=key)) + geom_point() + facet_wrap(~ key) +
  geom_line(aes(x=x,y=true.y),colour="black")

ggsave(svmePlot,
       filename = file.path(output_directory,"svm-e-inv.png"),
       width = 8,
       height = 6,
       dpi = 100)

sigmas <- 2 ** (-6:2) %>% `names<-`(.,.)

sSVMs <- sigmas %>%
  purrr::map_df(~ predict(ksvm(x=x,
                               y=y,
                               data = simData,
                               kernel = "rbfdot",
                               kpar = list(sigma=.x),
                               C = 1,
                               epsilon = 1e-4),
                          newdata = xGrid) %>%
                  as.vector
  )

svmsPlot <- xGrid %>%
  bind_cols(sSVMs) %>%
  gather(key=key,value=value,-x) %>%
  mutate(key=forcats::as_factor(key)) %>%
  left_join(trueData,by=c("x")) %>%
  ggplot(aes(x=x,y=value,colour=key)) + geom_point() + facet_wrap(~ key) +
  geom_line(aes(x=x,y=true.y),colour="black")

ggsave(svmsPlot,
       filename = file.path(output_directory,"svm-s-inv.png"),
       width = 8,
       height = 6,
       dpi = 100)


