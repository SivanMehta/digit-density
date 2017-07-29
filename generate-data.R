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

tibble(x = 1:10000) %>%
  mutate(contains.10 = grepl("3", convert.vector.to.base(x, 10))) %>%
  mutate(before.10 = cumsum(contains.10)) %>%
  mutate(density.10 = before.10 / x) %>%
  
  mutate(contains.9 = grepl("3", convert.vector.to.base(x, 9))) %>%
  mutate(before.9 = cumsum(contains.9)) %>%
  mutate(density.9 = before.9 / x) %>%
  
  mutate(contains.8 = grepl("3", convert.vector.to.base(x, 8))) %>%
  mutate(before.8 = cumsum(contains.8)) %>%
  mutate(density.8 = before.8 / x) %>%
  
  mutate(contains.7 = grepl("3", convert.vector.to.base(x, 7))) %>%
  mutate(before.7 = cumsum(contains.7)) %>%
  mutate(density.7 = before.7 / x) %>%
  
  mutate(contains.6 = grepl("3", convert.vector.to.base(x, 6))) %>%
  mutate(before.6 = cumsum(contains.6)) %>%
  mutate(density.6 = before.6 / x) %>%
  
  mutate(contains.5 = grepl("3", convert.vector.to.base(x, 5))) %>%
  mutate(before.5 = cumsum(contains.5)) %>%
  mutate(density.5 = before.5 / x) %>%
  
  mutate(contains.4 = grepl("3", convert.vector.to.base(x, 4))) %>%
  mutate(before.4 = cumsum(contains.4)) %>%
  mutate(density.4 = before.4 / x) %>%
  
  ggplot() +
  geom_line(aes(x = log(x), y = density.10),colour = '#ffffff') +
  geom_line(aes(x = log(x), y = density.9), colour = '#dfdfdf') +
  geom_line(aes(x = log(x), y = density.8), colour = '#bfbfbf') +
  geom_line(aes(x = log(x), y = density.7), colour = '#9f9f9f') +
  geom_line(aes(x = log(x), y = density.6), colour = '#7f7f7f') +
  geom_line(aes(x = log(x), y = density.5), colour = '#5f5f5f') +
  geom_line(aes(x = log(x), y = density.4), colour = '#3f3f3f') +
  labs(title = 'Proportion of previous numbers containing the number "3", by base',
       subtitle = "Darker color indicates larger base system",
       y = "proportion")
  