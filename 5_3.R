if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  purrr
)
pacman::p_load_gh(
  'rstudio/reticulate',
  'rstudio/tensorflow',
  'rstudio/keras'
)
options(verbose = TRUE)

tensorflow::install_tensorflow()
install_keras()

mnist <- dataset_mnist()
str(mnist, 2)
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

str(train_images)
str(train_labels)
str(test_images)
str(test_labels)

train_images <- train_images[1:500,,]
train_labels <- train_labels[1:500]
test_images <- test_images[1:200,,]
test_labels <- test_labels[1:200]

network <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = 'relu', input_shape = c(28 * 28)) %>% 
  layer_dense(units = 10, activation = 'softmax')

network %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
# функция compile() изменяет сеть на месте подобно := in data.table (не создает новый объект сети)

str(train_images)
train_images <- array_reshape(train_images, c(500, 28 * 28))
str(train_images)
train_images <- train_images / 255

test_images <- array_reshape(test_images, c(200, 28 * 28))
test_images <- test_images / 255


# Подготовка меток:
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)


# теперь можно начинать обучение сети, для этого нужно вызвать метод fit сети — он пытается адаптировать (fit)
# модель под обучающие данные:

network %>% fit(train_images, train_labels, epochs = 5, batch_size = 100)

metrics <- network %>% evaluate(test_images, test_labels, verbose = 0)
metrics
