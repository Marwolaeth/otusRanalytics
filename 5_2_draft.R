if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  tidyverse,
  readr,
  broom,
  rpart,
  rpart.plot,
  rattle,
  randomForest,
  ModelMetrics,
  caret
)

get_subset <- function(x, .which) {
  '['(x, .which)
}

if (!file.exists('data/bank-additional-full.csv')) {
  if (!file.exists('data/bank-additional.zip')) {
    if (!dir.exists('data')) dir.create('data')
    download.file(
      'http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip',
      destfile = 'data/bank-additional.zip'
    )
  }
  unzip(
    'data/bank-additional.zip',
    files = c(
      'bank-additional/bank-additional-full.csv',
      'bank-additional/bank-additional-names.txt'
    ),
    exdir = 'data',
    junkpaths = TRUE
  )
}

data <- read_delim(
  'data/bank-additional-full.csv',
  delim = ';',
  locale = locale(decimal_mark = '.')
)

.cmatrix <- function(predicted, reference, print.tab, success.index = 2) {
  si <- as.integer(success.index)
  fi <- setdiff(c(1L, 2L), si)
  tab <- table(Predicted = predicted, Actual = reference)
  if (print.tab) print(tab, comment = FALSE, quote = FALSE, print.gap = 4)
  assign('TP', tab[si, si], pos = parent.frame())
  assign('FP', tab[si, fi], pos = parent.frame())
  assign('FN', tab[fi, si], pos = parent.frame())
  assign('TN', tab[fi, fi], pos = parent.frame())
}

# Единая функция для расчета всех индексов + показатель F1
evaluate_model <- function(
  pred,
  reference,
  # Currently not used
  measures = c('accuracy', 'precision', 'recall', 'specificity', 'f', 'f1'),
  print.tab = TRUE,
  success.index = 2
) {
  require(ModelMetrics)
  if (print.tab) cat('\nConfusion matrix:\n\n')
  .cmatrix(pred, reference, print.tab, success.index)
  if (print.tab) cat('\n\nModel scores:\n\n')
  return(
    c(
      Accuracy    = (TP + TN) / (TP + FP + FN + TN),
      Precision   = TP / (TP + FP),
      Recall      = TP / (TP + FN),
      Specificity = TN / (TN + FP),
      'F1 Score'  = 2*TP / (2*TP + FP + FN),
      AUC         = auc(actual = reference, predicted = pred)
    )
  )
}

data <- data %>%
# 999 means client was not previously contacted
# распространенная метка для пропущенных значений, например, в SPSS
# i.e. previous == 0 & poutcome == "nonexistent"
# see: tapply(data$pdays, data$poutcome, mean)
  mutate_if(is.character, as.factor) %>%
  # This input should only be included for benchmark purposes
  # and should be discarded if
  # the intention is to have a realistic predictive model
  select(-duration)

set.seed(1111)
training_samples <- data %>%
  pull(y) %>%
  createDataPartition(p = 0.7, list = FALSE)

# Выбираем уровни факторов, которых в наборе данных меньше 40
rare_values <- data %>%
  select_if(is.factor) %>%
  map(table) %>%
  map(~ subset(., subset = (. == min(.)), drop = FALSE)) %>%
  get_subset(map_lgl(., ~ . <= 40)) %>%
  map_chr(names)
rare_values

set.seed(1111)
rare_observations <- map2_df(
  names(rare_values),
  rare_values,
  ~ filter(data, get(.x) == .y) %>% sample_n(1)
)
rare_observations

data_train  <- data[ training_samples, ] %>% union(rare_observations)
data_test   <- data[-training_samples, ] %>% union(rare_observations)

rm(rare_values, rare_observations, training_samples)

model_init <- glm(y ~ ., data = data_train, family = binomial)
# model_init

model_init_details <- broom::augment(
  model_init,
  newdata = data_test,
  type.predict = 'response'
) %>%
  mutate(y_hat = factor(if_else(.fitted >= .5, 'yes', 'no')))

evaluate_model(model_init_details$y_hat, model_init_details$y)

###
car::vif(model_init)


multicor <- alias(model_init) %>%
  getElement('Complete') %>%
  dimnames() %>%
  getElement(1) %>%
  unique() %>%
  map(
    ~ str_detect(., names(data))
  ) %>%
  map_chr(~ names(data)[.])
multicor

frml <- formula(
  paste(
    'y ~ .',
    paste(
      map_chr(
        multicor,
        ~ paste0('-', .)
      ),
      collapse = ' '
    )
  )
)
frml

model_init <- glm(frml, data = data_train, family = binomial)

model_init_vif <- car::vif(model_init) %>%
  as.data.frame() %>%
  rownames_to_column('term') %>%
  as_tibble() %>%
  select(term, GVIF)
model_init_vif

useless_vars <- model_init_vif %>%
  filter(!(term %in% c('month', 'euribor3m', 'emp.var.rate')) & GVIF > 10) %>%
  pull(term) %>%
  c(multicor)
useless_vars

# model_init_tidy <- tidy(model_init) %>%
#   right_join(model_init_vif) %>%
#   filter(!is.na(p.value))
# 
# useless_vars <- model_init_tidy %>%
#   filter()

frml <- formula(
  paste(
    'y ~ .',
    paste(
      map_chr(
        useless_vars,
        ~ paste0('-', .)
      ),
      collapse = ' '
    )
  )
)
frml

model1 <- glm(frml, data = data_train, family = binomial)

model_details <- broom::augment(
  model1,
  newdata = data_test,
  type.predict = 'response'
) %>%
  mutate(y_hat = factor(if_else(.fitted >= .5, 'yes', 'no')))

evaluate_model(model_details$y_hat, model_details$y)


model_train_details <- broom::augment(
  model1,
  type.predict = 'response'
)
lossmatrix <- matrix(c(0, 2, 5, 0))
y_rounding <- rpart(
  y ~ .fitted,
  data = model_train_details,
  parms = list(
    prior = c(.75, .25),
    loss = lossmatrix
  )
)
y_rounding
fancyRpartPlot(y_rounding)

model_details <- broom::augment(
  model1,
  newdata = data_test,
  type.predict = 'response'
) %>%
  mutate(y_hat = factor(if_else(.fitted >= .2, 'yes', 'no')))

evaluate_model(model_details$y_hat, model_details$y)
# norm

car::vif(model1)

(insignificant_predictors <- model1 %>%
    tidy() %>%
    filter(p.value >= .05))

(insignificant_vars <- insignificant_predictors %>%
  filter(term %in% names(data_train)) %>%
  pull(term)
)

# (insignificant_levels <- names(data_train) %>%
#     map(~ grep(paste0('^', .), insignificant_predictors$term, value = TRUE)) %>%
#     set_names(names(data_train)) %>%
#     get_subset(map_int(., length) > 0)
# )

# pc <- princomp(data[, 15:19], cor = TRUE)
# pc
# str(pc)
# pc$loadings
# 
# pc <- princomp(data[, c('emp.var.rate', 'euribor3m', 'nr.employed')], cor = TRUE)
# pc
# str(pc)
# pc$loadings

(useless_vars <- c(useless_vars, insignificant_vars))

frml <- formula(
  paste(
    'y ~ .',
    paste(
      map_chr(
        useless_vars,
        ~ paste0('-', .)
      ),
      collapse = ' '
    )
  )
)
frml

model2 <- glm(frml, data = data_train, family = binomial)

model_details <- broom::augment(
  model2,
  newdata = data_test,
  type.predict = 'response'
) %>%
  mutate(y_hat = factor(if_else(.fitted >= .2, 'yes', 'no')))

evaluate_model(model_details$y_hat, model_details$y)
# ok
car::vif(model2)

(insignificant_predictors <- model2 %>%
    tidy() %>%
    filter(p.value >= .05))


##
wghts <- ifelse(data_train$y == 'yes', 3, 1)
model3 <- glm(frml, data = data_train, family = binomial, weights = wghts)

model_details <- broom::augment(
  model3,
  newdata = data_test,
  type.predict = 'response'
) %>%
  mutate(y_hat = factor(if_else(.fitted >= .2, 'yes', 'no')))

evaluate_model(model_details$y_hat, model_details$y)

model_train_details <- broom::augment(
  model3,
  type.predict = 'response'
)
lossmatrix <- matrix(c(0, 5, 2, 0))
y_rounding <- rpart(
  y ~ .fitted,
  data = model_train_details,
  parms = list(
    loss = lossmatrix,
    prior = c(.5, .5)
  )
)
y_rounding
fancyRpartPlot(y_rounding)

model_details <- broom::augment(
  model3,
  newdata = data_test,
  type.predict = 'response'
) %>%
  mutate(y_hat = factor(if_else(.fitted >= .39, 'yes', 'no')))

evaluate_model(model_details$y_hat, model_details$y)
# best so far

car::vif(model3)

# predict_round <- function(
#   x,
#   labels,
#   cuts = 10,
#   quantiles = seq(0, 1, by = .1),
#   logarithm = FALSE,
#   seed = 1111
# ) {
#   if (logarithm) y <- log(x) else y <- x
#   if (is.null(cuts) && !is.null(quantiles)) {
#     cuts <- quantile(y, probs = quantiles, na.rm = TRUE) 
#   }
#   if (logarithm) cuts <- exp(cuts)
#   x_cut <- cut(
#     x,
#     breaks = cuts,
#     ordered_result = TRUE
#   )
#   # table(x_cut)
#   tab <- prop.table(table(x_cut, labels), 1)
#   labels <- unique(factor(labels))
#   set.seed(seed)
#   y <- sapply(
#     x_cut,
#     function(xc) base::sample(labels, size = 1, prob = tab[xc,])
#   )
#   return(y)
# }
# 
# .prediction_table <- function(
#   ptable, cuts
# ) {
#   return(
#     structure(
#       list(
#         table  = ptable,
#         cuts   = cuts,
#       ),
#       class = 'ptable'
#     )
#   )
# }
# print.ptable <- function(x) print(x$table)
# 
# predict_round <- function(
#   x,
#   labels,
#   cuts = 10,
#   quantiles = seq(0, 1, by = .1),
#   logarithm = FALSE,
#   ptable = NULL,
#   return.ptable = FALSE,
#   seed = 1111
# ) {
#   if (logarithm) y <- log(x) else y <- x
#   if (is.null(cuts) && !is.null(quantiles)) {
#     cuts <- quantile(y, probs = quantiles, na.rm = TRUE) 
#   }
#   if (logarithm) cuts <- exp(cuts)
#   x_cut <- cut(
#     x,
#     breaks = cuts,
#     ordered_result = TRUE
#   )
#   # table(x_cut)
#   tab <- prop.table(table(x_cut, labels), 1)
#   if (return.ptable) return(.prediction_table(tab, sapply(levels(x_cut), summary)))
#   labels <- unique(factor(labels))
#   set.seed(seed)
#   y <- sapply(
#     x_cut,
#     function(xc) base::sample(labels, size = 1, prob = tab[xc,])
#   )
#   return(y)
# }
# 
# # y <- predict_round(x, model_train_details$y, cuts = 10)
# # tibble(x, y) %>% arrange(desc(x))
# # tapply(x, y, median)
# 
# model_details <- model_details %>%
#   mutate(y2_hat = predict_round(.fitted, y))
# 
# evaluate_model(model_details$y2_hat, model_details$y)

model_train_details <- broom::augment(
  model3,
  type.predict = 'response'
)
lossmatrix <- matrix(c(0, 1, 4, 0))
y_rounding <- rpart(
  y ~ .fitted,
  data = model_train_details,
  parms = list(
    loss = lossmatrix,
    prior = c(.5, .5)
  )
)
y_rounding
fancyRpartPlot(y_rounding)

pred <- predict(y_rounding, model_train_details, type = 'class')
evaluate_model(pred = pred, reference = model_train_details$y)

model_details <- broom::augment(
  model3,
  newdata = data_test,
  type.predict = 'response'
) %>%
  mutate(y_hat = predict(y_rounding, ., type = 'class'))

evaluate_model(model_details$y_hat, model_details$y)