### Занятие ООП ###

#### S3 ####


#### ИНТРО ####

# создание объекта класса vehicle. Содержит одно поле со сторокой "car"
# structure()
car <- structure('car', class = 'vehicle')
car <- structure("car", class = "vehicle")

car
class(car)

# то же самое, только в 2 строки
car_2 <- "car"
class(car_2) <- "vehicle"
class(car_2)


#### S3: Создание ####

# Задание 1:
# - Придумать свой собственный класс. 
# Идеи: компьютер и его компоненты, геометрические фигуры
# - Написать конструктор для придуманного класса
# - Создать 2 объекта

# Пример
# create_car <- function(brand, weight, type, passengers = 0, max_passengers) {
#   return(structure(list(brand = brand,
#                         weight = weight,
#                         type = type,
#                         passengers = passengers,
#                         max_passengers = max_passengers),
#                    class = "vehicle"))
# }

# Конструктор
# Just kidding
new_dog <- function(sex, breed, breed_purity, height,
                    weight, aggressiveness, smartness) {
  return(structure(list(sex = sex,
                        breed = breed,
                        breed_purity = breed_purity,
                        height = height,
                        weight = weight,
                        aggressiveness = aggressiveness,
                        smartness = smartness), 
                   class = "dog"))
}

# Создать 2+ объекта из нового класса
Smalkin <- new_dog(
  sex = 'm',
  breed = 'puddle',
  breed_purity = 0.92,
  height = 40,
  weight = 13,
  aggressiveness = .2,
  smartness = .4
)
___ <- ___(___)


#### S3: Методы ####

# Задание 2:
# - Придумать два метод для своих класса
# - Реализовать эти методы


# Пример:
# is_heavy <- function(x) UseMethod("is_heavy")
# 
# is_heavy.vehicle <- function(x) {
#   return(x$weight > 2000)
# }


is_purebred <- function(x) UseMethod("is_purebred")

is_purebred.dog <- function(x) {
  x$breed_purity >= 0.9
}
is_purebred(Smalkin)

train <- function(x, what, amount) UseMethod("train")

train.dog <- function(x, what = 'smartness', amount = .1) {
  if(!(what %in% c('smartness', 'aggressiveness'))) {
    stop('Invalid characteristic name')
  }
  x[[what]] <- x[[what]] + amount
  return(x)
}

train(Smalkin)
train(Smalkin, 'aggressiveness')

___(___)
___ <- ___(___)


#### S3: Print/Summary ####

# метод print для своего класса
print.dog <- function(x) {
  name <- deparse(substitute(x))
  dog_description <- sprintf(
    '%s is a %s %s dog. It is %f cm high and %f kg heavy.\nView summary for details',
    name, x$sex, x$breed, x$height, x$weight
  )
  cat(paste(dog_description, '\n'))
  return(invisible(x))
}

print(Smalkin)
# конструктор объекта summary
summary.___ <- function(x) {
  
  ___
  class(___) <- "___"
  return(___)
}

# метод print для объекта summary
print.___ <- function(x) {
  
  ___
  return(invisible(x))
}

class(mycar1)

### Рефлексия
# 1. Что вообще существуют системы классов о_О
# 2. Что для сотрудничества с разработчиками S3 скорее всего будет недостаточно
# 3. Для понимания: системы классов похожи на категории в когнитивистики, вплоть до «прототипов»