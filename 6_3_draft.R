tidy_prop_test <- function(
  .tbl = NULL,
  .cols = if (!is.null(.tbl)) which(sapply(.tbl, is.factor))[1:2] else NULL,
  x = NULL,
  y = NULL,
  reference = max(
    getElement(levels(x), 1),
    getElement(levels(.tbl[[.cols[1]]]), 1)
  ),
  null_value = 0,
  alternative = c('two tailed', 'less', 'greater'),
  alpha = 0.95
) {
  if ((is.null(x) | is.null(y)) & !is.null(.tbl)) {
    .tbl <- .tbl[, .cols]
    x <- .tbl[[1]]
    y <- .tbl[[2]]
  }
  p1_hat <- mean(x == reference)
  p2_hat <- mean(y == reference)
  n1 <- length(x)
  n2 <- length(y)
  
  # Если при нулевой гипотезе p1 - p2 = 0, используется объединенная пропорция
  if (null_value == 0) {
    p_pool  <- (p1_hat*n1 + p2_hat*n2) / (n1 + n2)
    sf_cond <- c(p_pool*n1, p_pool*n2, (1-p_pool)*n1, (1-p_pool)*n2)
    SE <- sqrt(
      ((p_pool * (1-p_pool)) / n1) + ((p_pool * (1-p_pool)) / n2)
    )
  } else {
    sf_cond <- c(p1_hat*n1, p2_hat*n2, (1-p1_hat)*n1, (1-p2_hat)*n2)
    SE <- sqrt(
      ((p1_hat * (1-p1_hat)) / n1) + ((p2_hat * (1-p2_hat)) / n2)
    )
  }
  
  # Проверка условия
  sf_cond <- !(any(sf_cond < 10))
  if (!sf_cond) {
    warning(
      'Test results are unreliable: the success-failure condition is not met',
      call. = FALSE
    )
  }
  
  # Проверка гипотезы
  estimate <- p1_hat - p2_hat
  Z <- (abs(estimate) - null_value) / SE
  alternative <- match.arg(alternative)
  p_value <- switch(
    alternative,
    'two tailed' = 1 - pnorm(Z) + pnorm(-Z),
    'less'       = pnorm(-Z),
    'greater'    = pnorm(Z, lower.tail = FALSE)
  )
  
  # Доверительный интервал
  z_star <- qnorm(1 - (1 - alpha)/2)
  ME <- z_star * SE
  ci_lower <- estimate - ME
  ci_upper <- estimate + ME
  
  # Возвращаем в виде таблицы данных
  return(
    data.frame(
      p1_hat,
      p2_hat,
      diff_hat = estimate,
      sf_cond,
      se = SE,
      conf_level = alpha,
      ci_lower,
      ci_upper,
      null_value,
      statistic = Z,
      alternative,
      p_value
    )
  )
}

##
.tbl <- df_wide[1:13,]
.cols = c(A, B)

########

N <- 200 # Для обеих групп
p <- .3  # Для обеих групп
# set.seed(191222)
# A <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))
# B <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))

# set.seed(191222)
# A <- factor(
#   sample(
#     c('Agree', 'Disagree'),
#     size = N,
#     replace = TRUE,
#     prob = c(p, 1-p)
#   )
# )
# B <- factor(
#   sample(
#     c('Agree', 'Disagree'),
#     size = N,
#     replace = TRUE,
#     prob = c(p, 1-p)
#   )
# )

N <- 200
p <- 0.47
set.seed(191222)
A <- factor(
  sample(
    c('Agree', 'Disagree'),
    size = N,
    replace = TRUE,
    prob = c(p, 1-p)
  )
)
B <- factor(
  sample(
    c('Agree', 'Disagree'),
    size = N,
    replace = TRUE,
    prob = c(p, 1-p)
  )
)

c(mean(A == 'Agree'), mean(B == 'Agree'))

(the_test <- fisher.test(A, B, conf.level = .9))
# (tt <- tidy(the_test))
cbind(A = table(A), B = table(B))
(the_test <- prop.test(rbind(A = table(A), B = table(B)), correct = F))

# tidy_prop_test(A, B, alpha = .9)
tidy_prop_test(x = A, y = B, alpha = .9)

df <- map2(
  list(A, B),
  list(name(A), name(B)),
  ~ tibble(group = .y, response = .x)
) %>%
  bind_rows() %>%
  mutate(
    group = factor(group)
  ) %>%
  group_by(group) %>%
  mutate(
    trial = 1:n(),
    p_hat = cummean(response == 'Agree')
  )

df_wide <- df %>%
  ungroup() %>%
  pivot_wider(id_cols = trial, names_from = group, values_from = response)

df_wide %>% select(-trial) %>% summarise_all(~ mean(. == 'Agree'))

df_wide_test <- map(
  df_wide$trial,
  ~ slice(df_wide, 1:.x)
) %>%
  map_df(tidy_prop_test, alpha = .9) %>%
  as_tibble()
df_wide <- bind_cols(df_wide, df_wide_test)

any(df_wide$p_value < .09)

df_wide_plot <- df_wide %>%
  filter(!is.na(p_value) & sf_cond) %>%
  mutate(
    lowest  = ifelse(p_value > .1, 0, if_else(p1_hat < p2_hat, p1_hat, p2_hat)),
    highest = ifelse(p_value > .1, 0, if_else(p1_hat > p2_hat, p1_hat, p2_hat))
  )
min_cases <- min(df_wide_plot$trial)

df <- slice(df, min_cases:n())

ggplot(
  df,
  aes(x = trial, colour = group)
) +
  geom_line(aes(y = p_hat)) +
  geom_ribbon(
    data = df_wide_plot,
    aes(
      x = trial,
      ymin = lowest,
      ymax = highest
    ),
    inherit.aes = FALSE,
    fill = 'navy',
    alpha = .3
  ) +
  scale_x_continuous('№ испытания', breaks = seq(0, 200, 20)) +
  scale_y_continuous('Пропорции по группам', limits = c(.3, .6)) +
  scale_colour_manual('Группа', values = c('red3', 'navy')) +
  ggtitle('Подглядывание и ложноположительные выводы') +
  annotate('text', x = 125, y = .45, label = '← p < 0.10', hjust = 0) +
  theme_minimal() +
  theme(plot.margin = margin(4, 0, 0, 4))
