### Заметка
# Распространенная ошибка: функции либо совсем не используют,
# либо делают слишком объемными↓
# что увеличивает вероятность ошибок, которые сложно отследить

###
# Код, копируемый из раза в раз — тело функции
# Изменяемые переменные — аргументы функции

hypotenuse <- function(a, b) {
  return(sqrt(a^2 + b^2))
}

hypotenuse(3, 4)
formals(hypotenuse)
args(hypotenuse)
formalArgs(hypotenuse)

calc_cube_root <- function(x) {
  return(x^(1/3))
}
calc_cube_root(c(1, 2, 3, -4))

calc_cube_root <- function(x) {
  sign(x) * abs(x)^(1/3)
}
calc_cube_root(c(1, 2, 3, -4))

## Lazy Evaluation
f <- function(a, b) {
  a %/% 2
}
f(2)

# ...
normalise <- function(x, m = mean(x), s = sd(x)) {
  (x - m) / s
}
normalise(c(1:6, NA))

normalise <- function(x, m = mean(x, ...), s = sd(x, ...), ...) {
  (x - m) / s
}
normalise(c(1:6, NA), na.rm = TRUE)

MyMerge <- function(x, y) {
  df <- merge(x, y, by= "name of the common column", all.x= TRUE, all.y= TRUE)
  return(df)
}
new.df <- Reduce(MyMerge, list(df1, df2, df3, df4))

# cars
mycars <- cars
mycars[40, 2] <- NA
mycars[49, 2] <- NA

percent_na <- function(df) {
  if(!is.data.frame(df)) {
    stop('Input should be data.frame!')
  }
  sum(is.na(df)) / prod(dim(df))
}
percent_na(mycars)
percent_na(1:10)

my_confint <- function(x) {
  if(!is.numeric(x))  stop('x should be a numeric vector')
  if(length(x) <= 2)  stop('x should be of length more than two')
  return(
    c(
      mean(x, na.rm = TRUE) - 2 * (sd(x, na.rm = TRUE) / sqrt(length(x))),
      mean(x, na.rm = TRUE),
      mean(x, na.rm = TRUE) + 2 * (sd(x, na.rm = TRUE) / sqrt(length(x)))
    )
  )
}
set.seed(123)
rad <- rnorm(20, mean = 5, sd= 2)
my_confint(rad)
my_confint('xyz')

confint <- function(x, level = .95) {
  if(!is.numeric(x))  stop('x should be a numeric vector')
  if(length(x) <= 2)  stop('x should be of length more than two')
  est = mean(x)
  q = qnorm(level + (1 - level) / 2)
  err = 2 * (sd(x) / sqrt(length(x)))
  lwr = est - err
  upr = est + err
  return(c(lower = lwr, estimate = est, upper = upr))
}
set.seed(123)
rad <- rnorm(20, mean = 5, sd = 2)
confint(rad)
