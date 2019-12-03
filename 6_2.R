51*52
(4/52) * (16/51)

b <- replicate(
  10000,
  any(
    duplicated(
      sample(
        1:365,
        50,
        replace = T
      )
    )
  )
)
table(b)
prop.table(table(b))

if (!require(pacman)) install.packages('pacmnan')
pacman::p_load(tidyverse, dslabs)

data("heights")
summary(heights)

x <- heights %>% filter(sex == 'Male') %>% mutate(h = height * 2.54) %>% pull(h)

h <- rnorm(800, mean(x), sd(x))

mean(x <= 175)
