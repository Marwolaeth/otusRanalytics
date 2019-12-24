N <- 200 # Для обеих групп
p <- .3  # Для обеих групп
set.seed(191222)
A <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))
B <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))

set.seed(191222)
A <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))
B <- sample(c(FALSE, TRUE), size = N, replace = TRUE, prob = c(1-p, p))

c(mean(A), mean(B))

(the_test <- fisher.test(A, B))
(tt <- tidy(the_test))
cbind(A = table(A), B = table(B))
(the_test <- prop.test(rbind(A = rev(table(A)), B = rev(table(B)))))
