## Scientific calculator
1:5
1:5 + 6:11

sum(1:5)
median(1:5)
sum(6:10)

-2:2 * -2:2

10 / 3   # Floating point division
10 %/% 3 # Integer division
10 %% 3  # Remainder after division

1:5 * (6:10)^2

pi
cos(pi)
cos(1i)
exp(pi)
log(pi)
log(pi + 1)
log1p(1) == log(1 + 1)

c(3, 4 - 1, 1 + 1 + 1) == 3
1:3 != 3:1
exp(1:5) < 100
(1:5)^2 >= 16

(3:8)^2 > 200

## Не сравнивать дробные числа
sqrt(2)^2 == 2 # False
sqrt(2)^2 - 2  # Вот
all.equal(sqrt(2)^2, 2) # all.equal() нам в помощь
all.equal(sqrt(4)^2, 4)

## Сравнение строк
c('A', 'B', 'C', 'D') < 'C'

## Creating variables <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
x <- 1:5
y <- 6:10
(x + 2) * (y - 3)
assign('local_variable', x^3 + y^3)
w <- 3:8
w^2 - c(x, 1)
w <- 3:7
w^2 - x
?make.names

z <- rnorm(5)
(z <- rnorm(5))

## Inf, NA
-Inf + 1
Inf -1
NaN
NA
# NA == NaN
Inf - Inf
-Inf + Inf
is.finite(x)
is.finite(Inf)
is.infinite(-Inf)
is.na(x)
is.na(Inf)
is.nan(NaN)
is.na(NaN)
is.nan(NA)

(x <- 1:10 >= 5)
!x
(y <- 1:10 %% 2 == 0)
!y
x & y
x | y

none_true <- c(FALSE, FALSE, FALSE)
some_true <- c(FALSE, TRUE, FALSE)
all_true  <- c(TRUE, TRUE, TRUE)
any(none_true) # F
any(some_true) # T
any(all_true)  # T
all(none_true) # F
all(some_true) # F
all(all_true)  # T

class(1:10)
class(10)
class(10L)
class(10i)

(gender <- factor(c('female', 'male', 'female', 'female', 'male', 'female')))
# levels(gender) <- c('male', 'female') # Так плохо!
gender
is.integer(gender)

gender_char <- sample(c('male', 'female'), size = 10000, replace = TRUE)
gender_fact <- factor(gender_char)
object.size(gender_char)
object.size(gender_fact)

ls(pattern = "^is", baseenv())

## Vectors
numeric(5)
character(5)
integer(5)
logical(5)
factor(5) # Не то :)
complex(5)

seq(0, 20, by = 4)

n <- 0
n:0
1:n
seq_len(13)
seq.int(1, 8, 2)
class(seq(2, 24, 2))
class(seq.int(2, 24, 2))
is.integer(seq(2, 24, 2))
is.integer(seq.int(2, 24, 2))

seq_along(gender)

## Indexing
x <- (1:5)^2
x[c(1, 3, 5)]
x[-c(2, 4)]
x[c(1, -1)] # Doesn't make sense!
x[-1.9]
which(x > 10)
which.max(x)
which.min(x)
x[which.max(x)]
# Еще можно по именам

rep(1:5, 3)
rep(1:5, times = 1:5)
rep(1:5, length.out = 7)
rep(1:5, each = 3)

order(x) # Индексы
sort(x)  # Значения
sort(x) == x[order(x)]

## Matrices

(m <- matrix(NA, nrow = 5, ncol = 4)) # Для memory-efficient
dim(m)


## Lists

## NULL
length(NULL)
length(NA)
z <- NULL # Используется для удаления переменной
