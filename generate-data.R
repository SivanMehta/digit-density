library(dplyr)
library(ggplot2)

# converting to a base less than 10, because
# adding letters gets arbitrary and dumb
convert.to.base <- function(n, base) {
  num <- n
  result <- ""
  max.base <- log(num, base) %/% 1
  
  for(i in max.base:0) {
    place.value <- base ** i
    digit <- num %/% place.value
    num <- num - digit * place.value
    result <- paste(result, digit, sep = "")
  }
  
  return(result)
}

convert.vector.to.base <- function(v, base) {
  sapply(v, convert.to.base, base)
}

1:10 %>% convert.vector.to.base(2)

tibble(x = 1:10000) %>%
  mutate(contains.10 = grepl("4", convert.vector.to.base(x, 10))) %>%
  mutate(before.10 = cumsum(contains.10)) %>%
  mutate(density.10 = before.10 / x) %>%
  ggplot() +
  geom_line(aes(x = log(x), y = density.10), colour = 'blue') +
  labs(title = 'Proportion of previous numbers containing the number "4"', y = "proportion")
  