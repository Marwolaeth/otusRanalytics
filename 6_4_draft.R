(txt <- darlana[1:3, text])
text_one_hot(txt[2], n = 10000)

text_hashing_trick(txt[2], n = 10000)
text_to_word_sequence(txt[2])

darlana_enc <- copy(darlana)[
  ,
  c('token_hash', 'news') := 
    .(sapply(text, keras::text_hashing_trick, n = 10000),
      as.numeric(grepl('^\\d{2}\\.\\d{2}\\.', text)))
  ]

library(dplyr)

# x <- as.tbl_cube(as_tibble(darlana_enc[, c(2, 4)]), met_name = 'token_hash')
y <- darlana_enc$news

x <- darlana_enc[, c(2, 4)]

set.seed(191222)
is_test <- sample(
  c(TRUE, FALSE),
  nrow(darlana_enc),
  replace = TRUE,
  prob = c(.34, .66)
)

imdb <- dataset_imdb(num_words = 1000, maxlen = 600)
str(imdb, 1)
str(imdb$test, 1)
str(imdb$test$x, 1)
class(imdb$test$x)

x_train <- x[!is_test] %>%
  select(token_hash) %>%
  array() %>%
  tensorflow::tf$convert_to_tensor()
x_test  <- x[ is_test] %>%
  as.tbl_cube(met_name = 'token_hash') %>%
  getElement('mets') %>%
  .[[1]] %>%
  as.array()
y_train <- y[!is_test]
y_test  <- y[ is_test]

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(1)) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

val_indices <- 1:100
x_val <- x_train[val_indices]
y_val <- y_train[val_indices]
partial_x_train <- x_train[-val_indices]
partial_y_train <- y_train[-val_indices]

options(verbose = T)
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  validation_data = list(x_val, y_val)
)

rm(list = grep('^[^(darlan)]', ls(), value = T))

########################################
library(dplyr)
library(dtplyr)
library(tidytext)

darlana_enc <- lazy_dt(darlana_encoded)
darlana_matrix <- darlana_enc %>%
  select(postid, token_hash) %>%
  count(postid, token_hash) %>%
  as_tibble() %>%
  bind_rows(
    as_tibble(
      expand.grid(
        postid = unique(.$postid),
        token_hash = setdiff(1:10000, unique(.$token_hash)),
        n = 0L
      )
    )
  ) %>%
  cast_sparse(postid, token_hash, n)
dim(darlana_matrix)

set.seed(191222)
is_test <- sample(
  c(TRUE, FALSE),
  nrow(darlana_matrix),
  replace = TRUE,
  prob = c(.34, .66)
)
y <- as.numeric(grepl('([Кк]иев)|([Уу]кра)', darlana[, text]))

x_train <- darlana_matrix[!is_test, ] %>% as.matrix()
x_test  <- darlana_matrix[ is_test, ] %>% as.matrix()
y_train <- y[!is_test]
y_test  <- y[ is_test]

dim(x_test)
dim(x_train)
str(dimnames(x_train), 1)

dimnames(x_train) <- NULL
dimnames(x_test)  <- NULL

(nwords <- dim(x_train)[2])

model <- keras_model_sequential() %>%
  layer_dense(units = 12, activation = "relu", input_shape = c(nwords)) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

val_indices <- 1:100
x_val <- x_train[val_indices,]
y_val <- y_train[val_indices]
partial_x_train <- x_train[-val_indices,]
partial_y_train <- y_train[-val_indices]

options(verbose = T)
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  validation_data = list(x_val, y_val)
)

#################################
tensor_d <- lapply(darlana$text, text_hashing_trick, n = 10000)
head(tensor_d)

set.seed(191222)
is_test <- sample(
  c(TRUE, FALSE),
  nrow(darlana),
  replace = TRUE,
  prob = c(.34, .66)
)
y <- as.numeric(grepl('([Кк]иев)|([Уу]кра)', darlana[, text]))

x_train <- tensor_d[!is_test]
x_test  <- tensor_d[ is_test]
y_train <- y[!is_test]
y_test  <- y[ is_test]

model <- keras_model_sequential() %>%
  layer_input(units = 12, activation = "relu", input_shape = c(1000)) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

val_indices <- 1:100
x_val <- x_train[val_indices]
y_val <- y_train[val_indices]
partial_x_train <- x_train[-val_indices]
partial_y_train <- y_train[-val_indices]

options(verbose = T)
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  validation_data = list(x_val, y_val)
)
