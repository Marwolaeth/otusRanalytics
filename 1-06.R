convert_currency <- function(currency) {
  FX <- list(USD = 65.9961, EUR = 73.2227, JPY = 62.50)
  convert <- function(amount) {
    return(amount * FX[[currency]])
  }
  return(convert)
}

convert_USD <- convert_currency('USD')
convert_USD(100)

if(!require('pacman')) install.packages('pacman')
pacman::p_load(purrr, microbenchmark)

lst <- list(
  c(4, 2),
  c(10, 3),
  c(13, 4),
  c(21, 1)
)
lst2 <- transpose(lst)

rnorm_map <- function(.l, n = 100L, seed = 1111) {
  set.seed(seed = seed)
  purrr::map(
    .l,
    ~ rnorm(n = n, .[1], .[2])
  ) %>%
    do.call(c, .)
}

rnorm_map2 <- function(.l, n = 100L, seed = 1111) {
  set.seed(seed = seed)
  purrr::map2(
    .l[[1]],
    .l[[2]],
    ~ rnorm(n = n, .x, .y)
  ) %>%
    do.call(c, args = .)
}

identical(rnorm_map(lst), rnorm_map2(lst2))

system.time(rnorm_map(lst, n = 1000L))
system.time(rnorm_map2(lst2, n = 1000L))

mbm <- microbenchmark(
  rnorm_map(lst, n = 1000L), rnorm_map2(lst2, n = 1000L),
  times = 10000L
)
summary(mbm)
str(mbm, 1)