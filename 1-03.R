12^2 != 145

set.seed(123)
x <- rnorm(10, 5, 2)
(y <- x[x >= 5])

set.seed(1234)
x <- sample(1:100, 1)
if (x > 50) print('x is large')
if (x > 20) {
  print(paste(x, ', x is large'))
  x <- floor(x / 2)
}
x

x <- sample(1:500, 1)
if (x >= 300) {
  print(x - x * 0.15)
} else if (x >= 100) {
  print(x - x * 0.075)
} else {
  print(x)
}

sprintf('x is %d', x)

set.seed(1234)
x <- rnorm(100)
y <- numeric(100)
for (i in seq_along(x)) {
  y[i] <- x[i]^2
}
sum(y)

set.seed(123)
X <- matrix(rnorm(30), nrow = 5, ncol = 6)
x <- numeric(6)
for (i in 1:ncol(X)) {
  x[i] <- mean(X[, i])
}
x

?mapply
mapply(rep, 1:4, 4:1)
