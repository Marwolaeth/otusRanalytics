if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, e1071, randomForest)

# data <- read_csv('Wholesale_customers_data-52504-5e00a2.csv')

data <- read_csv2('divorce.csv')
str(data)
head(data)
summary(data)

# Пришлось бы отдельно скачивать названия
# Стоит ли привести значения к ordinal?
f <- factor(data$Atr1, ordered = TRUE)
head(f)
# Или можно без этого обойтись?
# забыл

data$Class <- factor(data$Class)

set.seed(1111)
split <- runif(n = nrow(data)) > .3
train <- data[split, ]
test <- data[!split, ]

fit <- randomForest(Class ~ ., data = train)
fit

str(fit, 1)

pred <- predict(fit, test)

table(test$Class, pred)

importance(fit) %>% as.data.frame() %>% rownames_to_column(var = 'attr') %>% arrange(desc(MeanDecreaseGini))

##############################
fit_glm <- glm(Class ~ ., data = train, family = binomial)
fit_glm
summary(fit_glm)

data_f <- data %>%
  mutate_all(function(x) factor(x, ordered = TRUE)) %>%
  mutate(Class = as.integer(Class) - 1)
glimpse(data_f)
summary(data_f)

train <- data_f[split, ]
test <- data_f[!split, ]

cd <- cor(data[1:(length(data) - 1)])
diag(cd) <- 0
any(cd == 1)

fa <- factanal(data[1:(length(data) - 1)], 3, scores = 'Bartlett')
fa

pairs(data[, 1:20])
