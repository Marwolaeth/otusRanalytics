

#########################################################
############### KERAS ################
#########################################################
library(devtools)
install_github("rstudio/keras")
library(keras)
library(reticulate)
options(verbose = T)

### Должна быть установлена Anaconda3
browseURL("https://www.anaconda.com/distribution/")

# Вариант с установкой из R  не всегда работает безглючно. поэтому можно 
# установить tensorflow вручную и прописать use_python("path") 
### conda create -n tensorflow_env tensorflow
### conda install -c anaconda cudnn


browseURL("https://stackoverflow.com/questions/49684605/how-to-install-package-keras-in-r")

# Вариант установки из R:
tensorflow::install_tensorflow()
install_keras()
# Keras и TensorFlow. В результате вы получите версии Keras и TensorFlow,
# выполняющиеся на CPU.


# Если в вашей системе имеется видеокарта NVIDIA и правильно настроенные
# библиотеки CUDA и cuDNN, вы можете установить версию TensorFlow с поддержкой GPU:
#install_keras(tensorflow = "gpu")
# 

tensorflow::install_tensorflow()

#use_python("/home/df/anaconda3/envs/r-reticulate/bin/python3.6")

use_python("/home/df/anaconda3/envs/tensorflow_env/bin/python3.7")

library(magick)
p2 <- cowplot::ggdraw() + cowplot::draw_image("https://expertise.kiev.ua/wp-content/uploads/2017/02/876587587567576.png", scale = 1)
cowplot::plot_grid(p2, labels = "AUTO")

mnist <- dataset_mnist()
str(mnist, 2)
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

# Здесь train_images и train_labels — это обучающий набор, то есть данные, необходимые
# для обучения модели. После обучения модель будет проверяться тестовым
# (или контрольным) набором: test_images и test_labels. Изображения хранятся
# в трехмерных массивах, а метки — в одномерном массиве цифр от 0 до 9. Изображения
# и метки находятся в прямом соответствии, один к одному.


# Рассмотрим обучающие данные:

str(train_images)
str(train_labels)
str(test_images)
str(test_labels)

train_images <- train_images[1:500,,]
train_labels <- train_labels[1:500]
test_images <- test_images[1:200,,]
test_labels <- test_labels[1:200]

# сначала передадим нейронной сети обучающие данные, train_images и train_labels. 
# В результате этого сеть обучится сопоставлять изображения с метками. 
# Затем мы предложим сети классифицировать изображения в test_images 
# и проверим точность классификации по метка из test_labels.
# keras_model_sequential() - добавление слоёв в модель

network <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>% 
  layer_dense(units = 10, activation = "softmax")

options(browser = "google-chrome-stable")
browseURL("https://www.rdocumentation.org/packages/keras/versions/2.2.5.0/topics/layer_dense")

# "units = " число нейронов в слое (пример на картинках в пр.)
# "activation = "     функции активации
# input_shape  - Размерность ввода (целое число) без учета оси выборок. 
# Этот аргумент необходим при использовании этого слоя в качестве первого слоя в модели.

# Второй (и последний) уровень — это 10-переменный слой потерь (softmax layer),
# возвращающий массив с 10 оценками вероятностей (в сумме дающих 1). Каждая
# оценка определяет вероятность принадлежности текущего изображения к одному
# из 10 классов цифр.

#

# Чтобы подготовить сеть к обучению, нужно настроить еще три параметра для этапа
# компиляции:

#  Оптимизатор — механизм, с помощью которого сеть будет обновлять себя,
# опираясь на наблюдаемые данные и функцию потерь.
#  Функцию потерь — определяет, как сеть должна оценивать качество своей
# работы на обучающих данных и, соответственно, как корректировать ее в пра-
# вильном направлении.
#  Метрики для мониторинга на этапах обучения и тестирования — здесь нас
# будет интересовать только точность (доля правильно классифицированных изображений).

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
# функция compile() изменяет сеть на месте подобно := in data.table (не создает новый объект сети)



# Перед обучением мы выполним предварительную обработку данных, преобразовав
# их в форму, которую ожидает получить нейронная сеть, и масштабируем их так,
# чтобы все значения оказались в интервале [0, 1]. Исходные данные — обучающие
# изображения — хранятся в трехмерном массиве (60000, 28, 28) типа integer,
# начениями в котором являются числа в интервале [0, 255]. Мы преобразуем его
# в массив (60000, 28 * 28) типа double со значениями в интервале [0, 1].

# Подготовка исходных данных:

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

browseURL("https://www.rdocumentation.org/packages/keras/versions/2.2.5.0/topics/fit.keras.engine.training.Model")

# "batch_size = 128" - Количество образцов на обновление градиента. 
# В процессе обучения отображаются две величины: потери сети на обучающих
# данных (loss) и точность сети (accuracy) на обучающих данных.
# В данном случае мы достигли точности 0.9885 (98,9 %) на обучающих данных.



# Теперь проверим, как модель распознает контрольный набор:

metrics <- network %>% evaluate(test_images, test_labels, verbose = 0)
metrics
#$loss
#[1] 0.06551541
#$accuracy
#[1] 0.9811

# Точность на контрольном наборе составила 97,8 (98.1) %  — немного меньше, чем на
# обучающем наборе. Эта разница между точностью на обучающем и контрольном наборах 
# демонстрирует пример переобучения (overfitting), когда модели машинного
# обучения показывают худшую точность на новом наборе данных по сравнению с обучающим.

# Давайте классифицируем первые 10 образцов в контрольном наборе:

network %>% predict_classes(test_images[1:15,])
#[1] 7 2 1 0 4 1 4 9 5 9
# (проверка на первых 15 экз.)
browseURL("https://www.rdocumentation.org/packages/keras/versions/2.2.5.0/topics/predict_proba")



##############################################################
##############################################################
##############################################################

# Представление данных для нейронных сетей (презентация .. tensors)
# вернемся к данным из MNIST:

mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

# Узнаем количество осей тензора train_images, обратившись к его атрибуту ndim:
length(dim(train_images))
# [1] 3

# его форму:
dim(train_images)
# [1] 60000     28     28

# и тип данных:
typeof(train_images)
# [1] "integer"

# теперь мы знаем, что это трехмерный тензор с целыми числами. Точнее, это
# массив с 60 000 матриц целых чисел размером 28 × 28 . Каждая матрица 
# представляет собой черно-белое изображение, где каждый элемент представляет 
# пиксел с плотностью серого цвета в диапазоне от 0 до 255.

# Попробуем отобразить пятую цифру из этого трехмерного тензора:
digit <- train_images[5,,]
plot(as.raster(digit, max = 255))

# Операция выбора конкретного элемента в тензоре называется получением среза тензора.

# Следующий пример извлекает цифры с 10-й до 99-й и помещает их в массив 
# c формой (90, 28, 28):
my_slice <- train_images[10:99,,]
dim(my_slice)
# [1] 90 28 28

# можно получить срез между любыми двумя индексами по каждой оси тензора. 
# Например, вот как можно выбрать пикселы из области 14 × 14 в правом
# нижнем углу каждого изображения (лейбл Adidasius находится в данном участке):
my_slice <- train_images[, 15:28, 15:28]

# модели глубокого обучения не обрабатывают весь набор данных целиком;
# они разбивают его на небольшие пакеты (пачки).
batch1 <- train_images[1:128,,]

# следующий пакет:
batch2 <- train_images[129:256,,]

# При рассмотрении таких пакетных тензоров первую ось  называют осью пакетов,
# или измерением пакетов. Эта терминология часто будет встречаться 
# при работе с Keras и другими библиотеками глубокого обучения.


