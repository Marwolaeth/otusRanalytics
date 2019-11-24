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
  mutate(
    education_info = if_else(
      education == 'unknown',
      'unknown',
      'known'
    )
  ) %>%
  mutate(
    education = if_else(
      education != 'unknown',
      education,
      NA_character_
      # Иначе ошибка: несовпадение классов
    )
  ) %>%
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
  mutate(
    education = factor(
      education,
      ordered = TRUE,
      levels = c(
        'illiterate',
        'basic.4y',
        'basic.6y',
        'basic.9y',
        'high.school',
        'professional.course',
        'university.degree'
      )
    )
  ) %>%
  # this input should only be included for benchmark purposes
  # and should be discarded if
  # the intention is to have a realistic predictive model
  select(-duration)
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
rm(data_target)

n <- nrow(data)
set.seed(1111)
test  <- sample(1:n, n / 3)
train <- -test

data_train <- data[train, ]
data_test  <- data[test,  ]

tree_model <- tree(y ~ ., data_train)
plot(tree_model)
text(tree_model, pretty = 6)

tree_pred = predict(tree_model, data_test, type = 'class')

(tab <- table(data_test$y, tree_pred))

(F1 <- 2 * tab[2,2] / (2 * tab[2,2] + tab[1,2] + tab[2,1]))

cv_tree = cv.tree(tree_model, FUN=prune.misclass)
plot(cv_tree$size, cv_tree$dev, type="b")

prune_model = prune.misclass(tree_model, best=3)
plot(prune_model)
text(prune_model)
print(prune_model)

tree_pred1 <- predict(prune_model, data_test, type = 'class')
(tab <- table(data_test$y, tree_pred1))
(F1 <- 2 * tab[2,2] / (2 * tab[2,2] + tab[1,2] + tab[2,1]))

rp <- rpart(y ~ ., data_train)
prp(rp)
fancyRpartPlot(rp)

tree_model <- ctree(y ~ ., data_train)