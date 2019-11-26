if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  checkpoint,
  tidyverse,
  forcats,
  readr,
  dlookr,
  tree,
  rpart,
  rpart.plot,
  rattle
)

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
glimpse(data)
summary(data)

# Нужно привести текстовые значения к категориальным переменным,
# некоторые — к упорядоченным
# Согласно странице описания набора данных:
# http://archive.ics.uci.edu/ml/datasets/Bank+Marketing
data <- data %>%
  # mutate(
  #   education_info = if_else(
  #     education == 'unknown',
  #     'unknown',
  #     'known'
  #   )
  # ) %>%
  # mutate(
  #   education = if_else(
  #     education != 'unknown',
  #     education,
  #     NA_character_
  #     # Иначе ошибка: несовпадение классов
  #   )
  # ) %>%
  # 999 means client was not previously contacted
  # распространенная метка для пропущенных значений, например, в SPSS
  # i.e. previous == 0 & poutcome == "nonexistent"
  # see: tapply(data$pdays, data$poutcome, mean)
  mutate(
    pdays = if_else(
      pdays == 999,
      NaN,
      pdays
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  # mutate(
  #   education = factor(
  #     education,
  #     ordered = TRUE,
  #     levels = c(
  #       'illiterate',
  #       'basic.4y',
  #       'basic.6y',
  #       'basic.9y',
  #       'high.school',
  #       'professional.course',
  #       'university.degree'
  #     )
  #   )
  # ) %>%
  # this input should only be included for benchmark purposes
  # and should be discarded if
  # the intention is to have a realistic predictive model
  select(-duration) %>%
  mutate(y = relevel(y, 'yes'))
summary(data, maxsum = 12)

dlookr::find_outliers(data, rate = TRUE, index = FALSE)
hist(data$campaign, breaks = 30)
hist(data$previous, breaks = 30)

# Возраст — нормальная ситуация для данных, где представлены один или несколькр долгожителей
# Кампания — есть какое-то количество клиентов с аномально большим количеством контактов; скорее всего, ничего особенного в этом нет.

dlookr::describe(data)

data_target <- dlookr::target_by(data, y)

dlookr::relate(data_target, campaign)
dlookr::relate(data_target, pdays)
dlookr::relate(data_target, previous)
dlookr::relate(data_target, cons.conf.idx)
rm(data_target)

n <- nrow(data)
set.seed(1111)
test  <- sample(1:n, n / 3)
train <- -test

data_train <- data[train, ]
data_test  <- data[test,  ]

data$id = 1:nrow(data)

set.seed(1111)
data_train <- data %>%
  group_by(y) %>%
  sample_frac(.7) %>%
  ungroup()
summary(data_train)

data_test <- data %>%
  anti_join(data_train, by = 'id') %>%
  select(-id)

data_train <- select(data_train, -id)

nrow(data_train) + nrow(data_test) == nrow(data)

(tab <- table(
  A = replicate(10, sample(LETTERS[1:5], 1)),
  B = replicate(10, sample(letters[1:5], 1))
))
str(tab)

(t <- matrix(
  c('TP', 'FN', 'FP', 'TN'),
  ncol = 2,
  dimnames = list(
    Predicted = c('Yes', 'No'),
    Actual    = c('Yes', 'No')
  )
))

.cmatrix <- function(predicted, reference, print.tab) {
  tab <- table(Predicted = predicted, Actual = reference)
  if (print.tab) print(tab, comment = FALSE, quote = FALSE, print.gap = 4)
  assign('TP', tab[1, 1], pos = parent.frame())
  assign('FP', tab[1, 2], pos = parent.frame())
  assign('FN', tab[2, 1], pos = parent.frame())
  assign('TN', tab[2, 2], pos = parent.frame())
}

accuracy <- function(predicted, reference, print.tab = TRUE) {
  .cmatrix(predicted, reference, print.tab)
  (TP + TN) / (TP + FP + FN + TN)
}

precision <- function(predicted, reference, print.tab = TRUE) {
  .cmatrix(predicted, reference, print.tab)
  TP / (TP + FP)
}

recall <- function(predicted, reference, print.tab = TRUE) {
  .cmatrix(predicted, reference, print.tab)
  TP / (TP + FN)
}

specificity <- function(predicted, reference, print.tab = TRUE) {
  .cmatrix(predicted, reference, print.tab)
  TN / (TN + FP)
}

accuracy(data$y, data$y)
precision(data$y, data$y)
accuracy(data$y, data$y, print.tab = F)
recall(data$y, data$y)

evaluate_model <- function(predicted, reference, print.tab = TRUE) {
  .cmatrix(predicted, reference, print.tab)
  return(
    c(
      Accuracy    = (TP + TN) / (TP + FP + FN + TN),
      Precision   = TP / (TP + FP),
      Recall      = TP / (TP + FN),
      Specificity = TN / (TN + FP),
      'F1 Score'  = 2*TP / (2*TP + FP + FN)
    )
  )
}

evaluate_model(data$y, data$y)
evaluate_model(
  data$y,
  sample(data$y, length(data$y))
)

tree_model <- tree(y ~ ., data_train)
plot(tree_model)
text(tree_model, pretty = 6)

tree_pred = predict(tree_model, data_test, type = 'class')

evaluate_model(tree_pred, data_test$y)

(tab <- table(data_test$y, tree_pred))

(F1 <- 2 * tab[1,1] / (2 * tab[1,1] + tab[2,1] + tab[1,2]))

cv_tree = cv.tree(tree_model, FUN=prune.misclass)
plot(cv_tree$size, cv_tree$dev, type="b")

prune_model = prune.misclass(tree_model, best=2)
plot(prune_model)
text(prune_model)
print(prune_model)

rp <- rpart(y ~ ., data_train)
prp(rp)
fancyRpartPlot(rp)

f1(rp, data_test, 'y')

tree_model <- ctree(y ~ ., data_train)

model <- rpart(y ~ ., data = data_train, xval = 30)
fancyRpartPlot(model)
pred <- predict(model, data_test, type = 'class')
evaluate_model(predicted = pred, reference = data_test$y)

model <- rpart(
  y ~ .,
  data = data_train,
  xval = 30,
  parms = list(prior = c(.5, .5))
)
fancyRpartPlot(model)
pred <- predict(model, data_test, type = 'class')
evaluate_model(predicted = pred, reference = data_test$y)

loss_matrix <- matrix(c(0, 1, 20, 0), ncol = 2)
model <- rpart(
  y ~ .,
  data = data_train,
  xval = 30,
  parms = list(loss = loss_matrix)
)
fancyRpartPlot(model)
pred <- predict(model, data_test, type = 'class')
evaluate_model(predicted = pred, reference = data_test$y)

loss_matrix <- matrix(c(0, 1, 2, 0), ncol = 2)
model <- rpart(
  y ~ .,
  data = data_train,
  xval = 30,
  parms = list(
    loss = loss_matrix,
    prior = c(.5, .5)
  )
)
fancyRpartPlot(model)
pred <- predict(model, data_test, type = 'class')
evaluate_model(predicted = pred, reference = data_test$y)

drop_cols <- c('month', 'day_of_week')

data_train_d <- select(data_train, -drop_cols)
data_test_d <- select(data_test, -drop_cols)

model <- rpart(
  y ~ .,
  data = data_train_d,
  xval = 30,
  parms = list(prior = c(.5, .5))
)
fancyRpartPlot(model)
pred <- predict(model, data_test_d, type = 'class')
evaluate_model(predicted = pred, reference = data_test$y)

loss_matrix <- matrix(c(0, 1, 5, 0), ncol = 2)
model <- rpart(
  y ~ .,
  data = data_train_d,
  xval = 30,
  parms = list(
    loss = loss_matrix
  )
)
fancyRpartPlot(model)
pred <- predict(model, data_test, type = 'class')
evaluate_model(predicted = pred, reference = data_test$y)

drop_cols <- c('month', 'day_of_week', 'euribor3m', 'nr.employed')
data_train_d <- select(data_train, -drop_cols)
data_test_d <- select(data_test, -drop_cols)

loss_matrix <- matrix(c(0, 1, 5, 0), ncol = 2)
model <- rpart(
  y ~ .,
  data = data_train_d,
  xval = 30,
  parms = list(
    loss = loss_matrix
  )
)
fancyRpartPlot(model)
pred <- predict(model, data_test, type = 'class')
evaluate_model(predicted = pred, reference = data_test$y)

# best so far
loss_matrix <- matrix(c(0, 3, 4, 0), ncol = 2)
model <- rpart(
  y ~ .,
  data = data_train_d,
  xval = 60,
  parms = list(
    loss = loss_matrix,
    split = 'gini',
    prior = c(.25, .75)
  ),
  control = list(
    minbucket = 100,
    minsplit  = 300,
    cp = 0.001
  )
)
fancyRpartPlot(model)
pred <- predict(model, data_test_d, type = 'class')
evaluate_model(predicted = pred, reference = data_test_d$y)
###

loss_matrix <- matrix(c(0, 3, 7, 0), ncol = 2)
model <- rpart(
  y ~ .,
  data = data_train_d,
  xval = 60,
  parms = list(
    loss = loss_matrix,
    split = 'gini',
    prior = c(.25, .75)
  ),
  control = list(
    minbucket = 100,
    minsplit  = 300,
    cp = 0.001
  )
)
fancyRpartPlot(model)
pred <- predict(model, data_test_d, type = 'class')
evaluate_model(predicted = pred, reference = data_test_d$y)
recall(pred, data_test_d$y)

data_target <- dlookr::target_by(data, y)

eda_report(data_target, target = 'y')

dlookr::relate(data_target, emp.var.rate)
ggplot(data, aes(x = emp.var.rate, fill = y)) +
  geom_density(alpha = .4)
# it works
# https://www.quora.com/What-is-meant-by-employment-variation-rate-Does-it-affect-in-any-way-the-financial-decisions-that-an-individual-takes
# https://www.euribor-rates.eu/en/what-is-euribor/
dlookr::relate(data_target, cons.price.idx)
ggplot(data, aes(x = cons.price.idx, fill = y)) +
  geom_density(alpha = .4)
dlookr::relate(data_target, previous)
dlookr::relate(data_target, cons.conf.idx)
rm(data_target)

bank_tree <- function(
  pars = c(
    fp_loss = 3,
    fn_loss = 4,
    prior_y = .25,
    minbckt = 100,
    minsplt = 300,
    complexity = .001 
  )
) {
  loss_matrix <- matrix(
    c(0, pars[1], pars[2], 0),
    ncol = 2,
    byrow = TRUE
  )
  model <- rpart(
    y ~ .,
    data = data_train_d,
    xval = 60,
    parms = list(
      loss = loss_matrix,
      split = 'gini',
      prior = c(pars[3], 1 - pars[3])
    ),
    control = list(
      minbucket = pars[4],
      minsplit  = pars[5],
      cp = pars[6]
    )
  )
  pred <- predict(model, data_test_d, type = 'class')
  scores <- evaluate_model(predicted = pred, reference = data_test_d$y)
  return(scores['F1 Score'])
}

bank_par <- optim(
  c(
    3,
    4,
    .25,
    100,
    300,
    .001
  ),
  fn = bank_tree,
  lower = c(0, 0, 0, 10, 20, -0.001),
  upper = c(6, 10, 1, 500, 1000, .4),
  method = 'L-BFGS-B',
  control = list(fnscale = -1)
)

bank_par

bank_tree(
  c(
    fp_loss = 3,
    fn_loss = 4,
    prior_y = .25,
    minbckt = 100,
    minsplt = 300,
    complexity = .001 
  )
)
