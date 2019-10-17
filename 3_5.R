if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse)

str(HairEyeColor)
HairEyeColor
(HairEyeColor <- data.frame(HairEyeColor))

## filter()

filter(HairEyeColor, Eye == 'Blue')
(blue_eye <- filter(HairEyeColor, Eye == 'Blue'))

### Операторы
filter(HairEyeColor, Eye %in% c('Blue', 'Green'))

str(mpg)
filter(
  mpg,
  (manufacturer %in% c('toyota', 'audi')) &
    (year == 2008)
)

dplyr::glimpse(mpg)

filter(
  mpg,
  year %in% (1999:2002),
  cyl == 6,
  !(drv %in% c('4', 'f'))
)

## select()
select(mpg, manufacturer:model)
select(mpg, manufacturer, model)
select(mpg, 1:2)
select(mpg, starts_with('m'))
select(mpg, -(displ:class))

mpg %>%
  filter(hwy > 40, class != 'subcompact') %>%
  select(manufacturer, model, displ)

mpg %>%
  arrange(displ)

mpg %>%
  arrange(desc(displ), cty)

mpg %>%
  arrange(desc(hwy-cty))

## mutate()
str(airquality)

airquality <- mutate(airquality, TempC = (Temp - 32) * (5/9))

?mutate_each()
?mutate_all()

## group_by() + summarise()
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty))

mpg %>%
  group_by(class, year) %>%
  summarise(max_displ = max(displ)) %>%
  arrange(desc(max_displ))

?summarise_each()
?summarise_all()

## joins()

# Задачи

## 1
glimpse(airquality)
airquality %>%
  filter(Month %in% c(5, 9), Day > 15) %>%
  mutate(Wind2 = 0.44704 * Wind) %>%
  group_by(Month) %>%
  summarise(Wind.Max = max(Wind2))

## 2
pacman::p_load(nycflights13)

glimpse(flights)
glimpse(planes)
intersect(names(flights), names(planes))

flights %>%
  left_join(planes, by = 'tailnum') %>%
  filter(!is.na(type)) %>%
  select(model, engine, arr_delay)
