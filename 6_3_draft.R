N <- 200 # Для обеих групп
p <- .3  # Для обеих групп
set.seed(191222)
# A <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))
# B <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))

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
(tt <- tidy(the_test))
cbind(A = table(A), B = table(B))
(the_test <- prop.test(rbind(A = rev(table(A)), B = rev(table(B)))))
