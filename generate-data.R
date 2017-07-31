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

density.for.base <- function(data, base) {
  contains <- grepl("3", convert.vector.to.base(data$x, base))
  before <- cumsum(contains)
  density <- before / data$x
  data[[paste('density', base, sep = '.')]] <- density
  return(data)
}

density.for.bases <- function(data, bases) {
 for(base in bases) {
   data <- data %>% density.for.base(base)
 }
  return(data)
}

tibble(x = 1:100000) %>%
  density.for.bases(4:10) %>%
  ggplot() +
  geom_line(aes(x = log(x), y = density.10),colour = '#ffffff') +
  geom_line(aes(x = log(x), y = density.9), colour = '#dfdfdf') +
  geom_line(aes(x = log(x), y = density.8), colour = '#bfbfbf') +
  geom_line(aes(x = log(x), y = density.7), colour = '#9f9f9f') +
  geom_line(aes(x = log(x), y = density.6), colour = '#7f7f7f') +
  geom_line(aes(x = log(x), y = density.5), colour = '#5f5f5f') +
  geom_line(aes(x = log(x), y = density.4), colour = '#3f3f3f') +
  labs(title = 'Proportion of previous numbers that contain the number "3", by base',
       subtitle = "Darker color indicates larger base system",
       y = "proportion")
  