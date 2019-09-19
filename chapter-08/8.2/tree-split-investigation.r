library(tidyverse)

# Trees prefer to split at more granular variates, even if they're less material
# so let's make some coarse informative variables + granular noise and see where trees
# prefer to split 

# model is Y = X + sigma + 0*Z 
#
# X informative, coarse
# Z noise, granular 

set.seed(123)

sample_func <- function(...)
{
  candidate <- rpart::rpart(Y ~ X + Z,
               data = tibble(X = rep(1:2, each = 100),
                             sigma = rnorm(200, sd = 4),
                             Z = rnorm(200,sd = 8)) %>%
                 mutate(Y = X + sigma),
               control = rpart::rpart.control(maxdepth = 1)) %>%
    `$`(splits) %>%
    rownames %>%
    `[`(1)
  
  return(ifelse(is.null(candidate),"NULL",candidate))
}

1:1000 %>%
  purrr::map_chr(sample_func) %>%
  enframe %>% 
  group_by(value) %>%
  summarise(n=n()) %>%
  knitr::kable(.)
