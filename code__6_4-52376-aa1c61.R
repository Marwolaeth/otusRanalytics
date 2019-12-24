
#########################################################
############### randomizeR ##############################
#########################################################
library(randomizeR)
options(browser = "google-chrome-stable")
browseURL("https://www.rdocumentation.org/packages/randomizeR/versions/2.0.0")
vignette("comparison-example")

cg <- corGuess("CS") 
# настр. ожидаемого количества правильных предположений последовательностей рандомизации
# "CS" относится к «стратегии конвергенции», то есть исследователь прогнозирует лечение, 
# которое до сих пор происходило реже.
# "DS" относится к «стратегии дивергенции», то есть исследователь предсказывает лечение, 
# которое до сих пор происходило чаще.

rar <- genSeq(rarPar(6),10)
rar2 <- genSeq(rarPar(6, groups = c("M", "F")), 10)
# Генерирует последовательности рандомизации из заданной процедуры рандомизации.
browseURL("https://www.rdocumentation.org/packages/randomizeR/versions/2.0.0/topics/generateRandomSequences")
# смотрим начинку настроек рандомизации:
rarPar(6)

genSeq(rarPar(6),10)
genSeq(rarPar(6, groups = c("M", "F")), 10)
# rarPar - случайное распределение (элементов в выборке)
# crPar  - полная рандомизация
# mpPar  - Последовательности максимальной длины (m-последовательности) 

# красивая математика примитивных  полиномов (для m-последовательностей):
browseURL("https://www.gaussianwaves.com/2018/09/maximum-length-sequences-m-sequences/")

getRandList(rar)
getRandList(rar2)

assess(rar, cg)

###
# оценить полный набор правил случайного распределения для N = 4 пациентов
sequences <- getAllSeq(rarPar(4))
issue1 <- corGuess("CS")
issue2 <- corGuess("DS")
issue3 <- imbal("imb")
issue4 <- imbal("maxImb")
assess(sequences, issue1, issue2, issue3, issue4)

#  оценить одну последовательность дизайна Big Stick относительно правильных догадок
sequence <- genSeq(bsdPar(10, 2), seed = 1909)
assess(sequence, issue1)

# оценить ту же последовательность в отношении смещения выбора и мощности для нормальной конечной точки
endp <- normEndp(c(2, 2), c(1, 1))
issue5 <- selBias("CS", 4, "exact")
issue6 <- setPower(2, "exact")
assess(sequence, issue1, issue5, issue6, endp = endp)

# оценить ту же последовательность относительно смещения выбора для экспоненциальной конечной точки
endp <- expEndp(lambda = c(0.5, 0.5), cenRate=0.1, accrualTime=1, cenTime=5)
issue7 <- selBias("CS", 0.1, "exact")
assess(sequence, issue1, issue7, endp = endp)

# рекомендуемый plot для оценки вероятности отказа
RP <- getAllSeq(crPar(6))
cB <- chronBias(type = "linT", theta = 1/6, method = "exact")
sB <- selBias(type=  "CS", eta = 1/4, method = "exact")
normEndp <- normEndp(c(0, 0), c(1, 1))
A <- assess(RP, cB, sB, endp = normEndp)
D <- A$D
desiredSeq <- round(sum(D[,2][D[,3] <= 0.05 & D[,4] <= 0.05]), digits = 4)
colnames(D) <- c("Seq", "Prob", "SB", "linT")
g <- ggplot(D, aes(x = SB, y = linT))
g <- g + annotate("rect", xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05,
                  alpha=0.2, fill="green") 
g <- g + geom_point(alpha = 1/10, size = 3, col = "orange")
g <- g <- g + geom_vline(xintercept = 0.05, col = "red")
g <- g + geom_hline(yintercept = 0.05, col = "red")
g  <- g + geom_text(data = NULL, x = 0, y = 0,
                    label = paste("Proportion:", desiredSeq), hjust=0, vjust=0, size = 7)
g


#####################
# https://rdrr.io/cran/randomizeR/src/tests/testthat/examples.r
# ----------------------------------------------------------------------
# EXAMPLES FOR TESTING THE FUNCTIONALITY OF RANDOM SEQUENCE GENERATION
# ----------------------------------------------------------------------


#########################################################
###############  blockrand ##############################
#########################################################

library(blockrand)
browseURL("https://www.rdocumentation.org/packages/blockrand/versions/1.3/topics/blockrand")
male <- blockrand(n=100, id.prefix='M', block.prefix='M',stratum='Male')
female <- blockrand(n=100, id.prefix='F', block.prefix='F',stratum='Female')
male
female
my.study <- rbind(male,female)
my.study

#########################################################
###############  TrialSize ##############################
#########################################################

library(TrialSize)


#########################################################
############### KERAS ###################################
#########################################################
library(keras)
library(reticulate)
options(verbose = T)

use_python("/home/df/anaconda3/envs/tensorflow_gpuenv/bin/python3.7")
#use_python("/home/df/anaconda3/envs/tensorflow_env/bin/python3.7")

########################################################################
########################################################################
########################################################################
  
# Классификация отзывов к фильмам -  пример бинарной классификации.
# Классификация по двум классам, или бинарная классификация, является едва ли
# не самой распространенной задачей машинного обучения.  
    
# набором данных IMDB: множеством 50 000 самых разных отзывов о фильмах
# в интернет-базе кинофильмов (Internet Movie Database). Набор разбит 
# на 25 000 обучающих и 25 000 контрольных отзывов, каждый набор на 50 %
# состоит из отрицательных и на 50 % — из положительных отзывов.    
# Нужно учитывать, что никогда не  следует тестировать модель машинного обучения
# на тех же данных, что использовались для ее обучения.
# Например, возможно, что ваша модель просто запомнит соответствия между
# обучающими образцами и их целями, что совершенно бесполезно для задачи 
# предсказания по данным, которые модель никогда не видела прежде.  
  
## Набор данных IMDB поставляется в составе Keras. Он уже готов к использованию: 
#  отзывы (последовательности слов) преобразованы в последовательности целых чисел, 
#  каждое из которых определяет позицию слова в словаре.  

# Загрузка набора данных IMDB:

  imdb <- dataset_imdb(num_words = 10000)
  c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb  

#То же самое можно записать иначе:
#  imdb <- dataset_imdb(num_words = 10000)
#  train_data <- imdb$train$x
#  train_labels <- imdb$train$y
#  test_data <- imdb$test$x
#  test_labels <- imdb$test$y
  
# Все наборы данных, что входят в состав Keras, имеют форму вложенных списков
# обучающих и контрольных данных.   
  
#  Аргумент num_words=10000 означает, что в обучающих данных будет сохранено
#  только 10 000 слов, наиболее часто встречающихся в обучающем наборе отзывов.
#  Редкие слова будут отброшены. Это позволит вам работать с вектором управляемого размера.  
  
# Переменные train_data и test_data — это списки отзывов; каждый отзыв — это
#  список индексов слов (кодированное представление последовательности слов).
#  Переменные train_labels и test_labels — это списки нулей и единиц, где нули
#  соответствуют отрицательным отзывам, а единицы — положительным:

str(train_data[[1]])  
train_labels[[1]]

# Поскольку мы ограничили себя 10 000 наиболее употребимых слов, в наборе 
# отсутствуют индексы больше 10 000:

max(sapply(train_data, max))
  

# показано декодирование одного из отзывов в последовательность слов на английском языке:  

# word_index — это список именованных элементов, отображающий слова в целочисленные индексы  
word_index <- dataset_imdb_word_index()                       
word_index$amerterish

# Получить обратное представление словаря, отображающее индексы в слова 
reverse_word_index <- names(word_index)                             
reverse_word_index
names(reverse_word_index) <- word_index

# Декодирование отзыва. Индексы смещены на 3, потому что индексы 0, 1 и 2 
# зарезервированы для слов «padding»  (отступ), «start of sequence» (начало последовательности)
# и «unknown» (неизвестно)

decoded_review <- sapply(train_data[[1]], function(index) {
  word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
  if (!is.null(word)) word else "?"
}
)



###### Pres: "Подготовка данных"

###

# Вариант с векторизованными данными,  созданными вручную:

vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension) # matrix с формой (length(sequences), dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1         #Запись единицы в элемент с данным индексом.
  results                                       
}      
x_train <- vectorize_sequences(train_data)                             
x_test <- vectorize_sequences(test_data)

str(x_train[1,])

# векторизация меток:
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)


### Конструирование сети
# Входные данные представлены векторами, а метки — скалярами (единицами и нулями):
# это самый простой набор данных, какой можно встретить. С задачами
# этого вида прекрасно справляются сети, организованные как простой стек полносвязных
# уровней с операцией активации relu: layer_dense(units = 16, activation == "relu").
# Аргумент (16 ), передаваемый каждому полносвязному уровню, — это число
# скрытых единиц уровня. Скрытая единица (hidden unit) — это измерение в пространстве 
# представлений уровня. Каждый полносвязный слой с операцией активации relu реализует
# следующую цепочку операций с тензорами:
#   output = relu(dot(W, input) + b)
# Наличие 16 скрытых единиц означает, что весовая матрица W будет иметь форму
# (input_dimension, 16): скалярное произведение на W спроецирует входные дан-
# ные в 16-мерное пространство представлений (затем будет произведено сложение
# с вектором смещений b и выполнена операция relu). Размерность пространства
# представлений можно интерпретировать как «степень свободы нейронной сети
# при изучении внутренних представлений». Большее количество скрытых единиц
# (большая размерность пространства представлений) позволяет сети обучаться
# на более сложных представлениях, но при этом увеличивается вычислительная
# стоимость сети, что может привести к выявлению нежелательных шаблонов (ша-
# блонов, которые могут повысить качество классификации обучающих данных, но
# не контрольных).
# В отношении такого стека полносвязных слоев требуется принять два важных
# архитектурных решения:
#  сколько слоев использовать;
#  сколько скрытых единиц выбрать для каждого уровня.

# В данном варианте настроек использавано 
# два промежуточных уровня с 16 скрытыми единицами в каждом;
# третий слой будет выводить скалярное значение — оценку направленности текущего отзыва.
# Промежуточные уровни будут использовать операцию relu в качестве функции активации, 
# а последний слой — использовать сигмоидную функцию активации и выводить вероятность
# (оценку вероятности, между 0 и 1, что образец относится к классу «1», то есть насколько 
# он близок к положительному отзыву).

# Функция relu используется для преобразования отрицательных значений в ноль,
#  а сигмоидная функция «размазывает» произвольные значения по интервалу [0, 1].
#  возвращая значения, которые можно интерпретировать как вероятность. (Презентация)

# Определение модели:
library(keras)
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

# Компиляция модели

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# Оптимизатор, функция потерь и метрики передаются в виде строковых значений,
# что возможно, потому что rmsprop, binary_crossentropy и accuracy являются частью
# Keras. Иногда бывает желательно настроить параметры оптимизатора или
# передать свою функцию потерь или метрик. Первую задачу можно решить, передав 
# в аргументе optimizer экземпляр оптимизатора (код ниже), 
# а последнюю — передав в аргументе loss и/или metrics объект функции (код ниже).

# Настройка оптимизатора:
model %>% compile(
  optimizer = optimizer_rmsprop(lr=0.001),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# Использование нестандартных функций потерь и метрик:
model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = loss_binary_crossentropy,
  metrics = metric_binary_accuracy
)

# Для контроля точности модели во время обучения на данных, которые она прежде
# не видела, создадим проверочный набор, выбрав 10 000 образцов из оригинального
# набора обучающих данных.
val_indices <- 1:10000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]



# обучение модели в течение 20 эпох (выполнив 20 итераций по всем образцам 
# в тензорах x_train и y_train ) пакетами по 512 образцов.
# В то же время будем следить за потерями и точностью на 10 000 отложенных образцов.
# Для этого достаточно передать проверочные данные в аргументе validation_data.

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

str(history)

# Объект history включает параметры, используемые для обучения модели (history$params), 
# а также данные для каждой метрики, за которыми осуществлялся мониторинг (history$metrics).
# Объект history имеет также метод plot(), с помощью которого можно создать
# графики изменения метрик по эпохам:
plot(history)


#
history_df <- as.data.frame(history)
str(history_df)

#######################
# Обучение новой модели с нуля:
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)

# Конечные результаты:
results
#$loss
#[1] 0.2900235
#$acc
#[1] 0.88512
# Это простейшее решение позволило достичь точности 88 %. Используя самые со-
# временные подходы, можно добиться точности 95 %.



# После обучения сети ее можно использовать для практического применения.
# Например, попробуем оценить несколько отзывов с помощью метода predict :
model %>% predict(x_test[1:10,])
#            [,1]
#[1,] 0.18956268
#[2,] 0.99990833
#[3,] 0.88671929
#[4,] 0.83468699
#[5,] 0.94504118
#[6,] 0.79102862
#[7,] 0.99975187
#[8,] 0.01413777
#[9,] 0.96875459
#[10,] 0.98719984

# сеть уверена в одних образцах (0,99 или выше либо 0,01 или ниже), но
# не так уверена в других (0,8; 0,7).

###############################################################################
###################  Подготовка данных  #######################################
###############################################################################

s = "To be or not to be"

text_to_word_sequence(s)
#[1] "to"  "be"  "or"  "not" "to"  "be" 

text_one_hot(s, n = 10000)
#[1] 6551 1002 2223 6983 6551 1002

browseURL("https://www.rdocumentation.org/packages/keras/versions/2.2.5.0/topics/text_one_hot")
browseURL("https://keras.io/preprocessing/text/")
browseURL("https://keras.rstudio.com/reference/text_one_hot.html")

getwd()
setwd("Desktop/OTUS/P_B_Z_6.1/")
dir()
text <- readLines("text_one_hot.txt")
text_one_hot(text, n = 10000)
#Ошибка в py_call_impl(callable, dots$args, dots$keywords) :
#  AttributeError: 'list' object has no attribute 'lower' 

str(text)
text_one_hot(text[3], n = 10000)
text_to_word_sequence(text[3])

lapply(text, text_one_hot, n = 10000)

# затем нужно вручную разметить обучающую лабель в виде вектора, где каждый элемент,
# соответствующий элементу вектора, будет промаркирован категорией 
# (например при классификации постов на 4 категории  - 0 1 2 3)

rm(text)
### Кодирование данных с помощью хэширования признаков:
# недостатки способа:
browseURL("https://www.rdocumentation.org/packages/keras/versions/2.2.5.0/topics/text_hashing_trick")

# принцип работы и отличие от предыдущего (когда используется):
browseURL("https://habr.com/ru/company/ods/blog/326418/#one-hot-encoding")
# разделы "One-Hot Encoding" и "Хэширование признаков (Hashing trick)"

text_hashing_trick(text[3], n = 10000)