if (!require(pacman)) install.packages('pacman')
pacman::p_load(tidyverse)

generate <- list(
  n = c('with ads' = 18930, 'without ads' = 16180),
  p = c('with ads' = 980, 'without ads' = 420)
)

df <- map2(
  generate[['n']],
  generate[['p']],
  ~ rep(c(1, 0), c(.y, .x - .y))
) %>%
  map(as_tibble) %>%
  bind_rows(.id = 'group') %>%
  sample_frac(1)

df %>%
  group_by(group) %>%
  summarise(sum(value), mean(value))

# parameter of interest: sum

bootstrap <- function(
  x,
  parameter = mean,
  nsample = length(x),
  nboot = 10000,
  alpha = .95
) {
  parameter <- as.function(parameter)
  quantile(
    replicate(nboot, parameter(sample(x, nsample, replace = TRUE))),
    c((1 - alpha)/2, 1 - (1 - alpha)/2)
  )
}
bootstrap(rnorm(100, 100, 10))

df %>% filter(group == 'with ads') %>% pull(value) %>% bootstrap(nsample = 10000)
df %>% filter(group == 'without ads') %>% pull(value) %>% bootstrap(nsample = 10000)

df <- map2(
  c('with ads' = 111, 'without ads' = 120),
  c(.6, .4),
  ~ sample(c(TRUE, FALSE), size = .x, replace = TRUE, prob = c(.y, 1 - .y))
) %>%
  map(~ tibble(purchase = .)) %>%
  bind_rows(.id = 'group') %>%
  mutate(group = factor(group)) %>%
  sample_frac(1)
df

df %>%
  group_by(group) %>%
  summarise(n(), sum(purchase), mean(purchase))

df %>% group_by(group) %>% group_map(~ bootstrap(.$purchase, nsample = 100))

df %>%
  group_by(group) %>%
  group_map(
    ~ prop.test(
        sum(.$purchase),
        nrow(.),
        p = .45,
        conf.level = .99
      ) %>% getElement('conf.int')
  )
