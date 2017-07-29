library(dplyr)
library(ggplot2)

tibble(x = 1:10000) %>%
  mutate(contains.10 = grepl("4", as.character(x))) %>%
  mutate(contains.16 = grepl("4", base::`!.hexmode`(x))) %>%
  mutate(contains.8 = grepl("4", base::`!.octmode`(x))) %>%
  mutate(before.10 = cumsum(contains.10)) %>%
  mutate(before.16 = cumsum(contains.16)) %>%
  mutate(before.8 = cumsum(contains.8)) %>%
  mutate(density.10 = before.10 / x) %>%
  mutate(density.16 = before.16 / x) %>%
  mutate(density.8 = before.8 / x) %>%
  ggplot() +
  geom_line(aes(x = log(x), y = density.10), colour = 'red') +
  geom_line(aes(x = log(x), y = density.16), colour = 'blue') +
  geom_line(aes(x = log(x), y = density.8), colour = 'green') +
  labs(title = 'Proportion of previous numbers containing the number "4"', y = "proportion")
  