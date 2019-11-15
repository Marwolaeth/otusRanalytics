if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  tidyverse,
  data.table,
  dbplyr,
  reshape2,
  GGally,
  cowplot,
  patchwork,
  gifski,
  png,
  gganimate
)

# распределение категориальной переменной
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# То же самое с помощью dplyr::count()
diamonds %>% 
  count(cut)

# проверить распределение непрерывной переменной
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# вручную, комбинируя dplyr::count()и ggplot2::cut_width():
diamonds %>% 
  count(cut_width(carat, 0.5))

# увеличиваем только алмазы размером менее трех каратов и выбираем меньшую ширину бина
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# Если вы хотите наложить несколько гистограмм на одном графике, рекомендуется использовать geom_freqpoly()
# вместо geom_histogram(). geom_freqpoly()выполняет те же вычисления, что и geom_histogram(),
# но вместо отображения счетчиков в столбцах вместо них используются строки. 

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

# Типичные значения
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

# Нестандартные
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

#  увеличить малые значения по оси Y с помощью coord_cartesian():
# аналог увеличительного стекла 
# xlim, ylim    Пределы для осей X и Y.
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
# рассматриваем данные на промежутке 0 : 50 по Y, благодаря чему возможно рассмотрение мелких выбросов


# Это позволяет нам увидеть, что есть три необычных значения: 0, ~ 30 и ~ 60. 
# Мы достаем их с помощью dplyr:
 
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual

# y Переменная измеряет один из трех размеров этих алмазов, в мм. 
# Мы знаем, что бриллианты не могут иметь ширину 0 мм, поэтому эти значения должны быть неверными. 
# Мы также можем подозревать, что размеры 32 мм и 59 мм неправдоподобны: эти алмазы имеют длину 
# более дюйма, но не стоят сотен тысяч долларов!

# Рекомендуется повторять анализ с выбросами и без них. Если они оказывают минимальное влияние 
# на результаты, и вы не можете понять, почему они есть, разумно заменить их пропущенными значениями 
# и двигаться дальше. Однако, если они оказывают существенное влияние на ваши результаты, 
# вы не должны отбрасывать их без объяснения причин. Вам нужно выяснить, что их вызвало 
# (например, ошибка ввода данных), и раскрыть, что вы удалили их в своей записи.


# ************************************************************
# Отсутствующие значения

# Удалить всю строку со странными значениями: ( не рекомендуется)
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# заменить необычные значения пропущенными значениями (ifelse() )
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

# Как и R, ggplot2 поддерживает философию о том,
# что пропущенные значения никогда не должны молча пропадать. 
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()
#> Warning: Removed 9 rows containing missing values (geom_point).

# Чтобы подавить это предупреждение, установите na.rm = TRUE:
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)


# Сравнить наблюдения с отсутствующими значениями и  с записанными значениями.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)


############################################################

### Ковариация

# если одна из групп намного меньше других, трудно увидеть различия в форме:
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
# на первый взгляд графики почти одинаковы, тк общее количество сильно отличается:
ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

# Чтобы упростить сравнение, нужно поменять местами то, что отображается на оси Y. 
# Вместо отображения количества будем отображать плотность , которая является 
# стандартизированным количеством, так что область под каждым многоугольником частоты равна единице.
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# Более детальный расчет плотности возможен за счет функций пакеджа "plotly"
# https://plot.ly/ggplot2/geom_density/
# Это материал следующего занятия

# Boxplot 
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# Если у вас длинные имена переменных,можно повернуть график на 90 °
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot() +
  coord_flip()
######################################################################
#############   fasets   #############################################
######################################################################

library(reshape2)

str(tips)
head(tips)
sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp

# This is done by giving a formula to facet_grid(), of the form vertical ~ horizontal.

# Divide by levels of "sex", in the vertical direction
sp + facet_grid(sex ~ .)


# Divide by levels of "sex", in the horizontal direction
sp + facet_grid(. ~ sex)

# Divide with "sex" vertical, "day" horizontal
sp + facet_grid(sex ~ day)

# Divide by day, going horizontally and wrapping with 2 columns
sp + facet_wrap( ~ day, ncol=2)
tips$day

# одинаковая фасовка:
sp + facet_grid(. ~ day)

sp + facet_wrap( ~ day, ncol=4)

###
# Modifying facet label appearance
sp + facet_grid(sex ~ day) +
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=22, face="bold"),
        strip.background = element_rect(colour="red", fill="#CCCCFF"))

### Интересный момент - "выпуск шкал на свободу"
### Free scales
# Normally, the axis scales on each graph are fixed, which means that they have the same size and range. They can be made independent, by setting scales to free, free_x, or free_y.

# A histogram of bill sizes
hp <- ggplot(tips, aes(x=total_bill)) + geom_histogram(binwidth=2,colour="white")

# Histogram of total_bill, divided by sex and smoker
hp + facet_grid(sex ~ smoker)

# Same as above, with scales="free_y"
hp + facet_grid(sex ~ smoker, scales="free_y")

# With panels that have the same scaling, but different range (and therefore different physical sizes)
hp + facet_grid(sex ~ smoker, scales="free", space="free")

######################################################################
#############   GGally :: ggpairs   ##################################
######################################################################

# По умолчанию ggpairs обеспечивает два разных сравнения каждой пары столбцов и отображает либо плотность, 
# либо количество соответствующих переменных по диагонали. При различных настройках параметров диагональ 
# можно заменить значениями осей и метками переменных.


## Quick example, with and without colour
p_ <- GGally::print_if_interactive
data(flea)
ggpairs(flea, columns = 2:4)
pm <- ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
p_(pm)


## Вернемся к курильщикам:

# columnsОтображается по умолчанию для всех столбцов прилагаемых data. 
# Для поднабора только нескольких столбцов используйте columnsпараметр.

data(tips, package = "reshape")
pm <- ggpairs(tips)
pm

# Снизим количество используемых столбцов:

pm <- ggpairs(tips, columns = c(1, 6, 2))
pm <- ggpairs(tips, columns = c("total_bill", "time", "tip"), columnLabels = c("Total Bill", "Time of Day", "Tip"))
pm

# Эстетика может быть применена к каждому фрагменту с mapping параметром.

pm <- ggpairs(tips, mapping = aes(color = sex), columns = c("total_bill", "time", "tip"))
pm


# Поскольку графики являются графиками по умолчанию (или являются вспомогательными функциями от GGally),
# эстетический цвет изменяется в соответствии с требованиями. Глядя на приведенный выше пример,
# «tip» против «total_bill» (pm [3,1]) нуждается в colorэстетике,
# в то время как «time» против «total_bill» нуждается в fillэстетике. 
# Если предоставлены пользовательские функции, эстетические изменения не будут сделаны.

#######################################################################

### Разделы матрицы
# Есть три основных разделов попарной матрицы: lower, upperи diag. lower и upper может содержать 
# три типа: сюжетные continuous, combo и discrete. «Diag» содержит только или continuous или discrete.

# * continuous: оба X и Y являются непрерывными переменными
# * combo: одна переменная X и Y дискретна, а другая непрерывна
# * discrete: оба X и Y являются дискретными переменными

# Чтобы внести коррективы в каждый раздел, может быть предоставлен список информации. 
#Список может состоять из следующих элементов:

#continuous:
  # символьная строка, представляющая конец ggally_NAMEфункции или пользовательскую функцию
  # текущая действительная upper$continuousи lower$continuous строка символов: 'points', 'smooth', 'density', 'cor','blank'
  # текущие действительные diag$continuousстроки символов: 'densityDiag', 'barDiag','blankDiag'
# combo:
  # символьная строка, представляющая конец ggally_NAME функции или пользовательскую функцию. (не относится к diagсписку)
  # текущая действительная upper$comboи lower$comboстрока символов: 'box', 'dot', 'facethist', 'facetdensity', 'denstrip','blank'
# discrete:
  # символьная строка, представляющая конец ggally_NAME функции или пользовательскую функцию
  # текущая действительная upper$discreteи lower$discreteстрока символов: 'ratio', 'facetbar','blank'
  # текущие действительные diag$discreteстроки символов: 'barDiag','blankDiag'
# mapping: если сопоставление предусмотрено, только сопоставление раздела будет перезаписано

pm <- ggpairs(
  tips, columns = c("total_bill", "time", "tip"),
  lower = list(
    continuous = "smooth",
    combo = "facetdensity",
    mapping = aes(color = time)
  )
)
pm

# Список разделов может быть установлен на строку символов "blank"или NULL 
# если раздел должен быть пропущен при печати.

pm <- ggpairs(
  tips, columns = c("total_bill", "time", "tip"),
  upper = "blank",
  diag = NULL
)
pm


# пример ggpairs:
pm <- ggpairs(tips, columns = c("total_bill", "time", "tip"))
# retrieve the third row, first column plot
p <- pm[3,1]
p <- p + aes(color = time)
p

pm[3,1] <- p
pm

#################################################################
#################   Cowplot   ###################################
#################################################################

# этот пакет позволяет легко объединить несколько графиков «ggplot2» в один 
# и пометить их буквами, например, A, B, C и т. Д., Что часто требуется для научных публикаций. 
# В отличии от фасетов, можно использовать разные типы графиков.
library(ggplot2)
library(cowplot)

# Пакет cowplot предоставляет функцию plot_grid()упорядочения участков в сетке и их маркировки.

p1 <- ggplot(mtcars, aes(disp, mpg)) + 
  geom_point()
p2 <- ggplot(mtcars, aes(qsec, mpg)) +
  geom_point()

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)

# plot_grid() построен поверх общего графического слоя, который позволяет нам захватывать графики 
# в виде изображений, а затем рисовать ими или поверх них.

p <- ggplot(mtcars, aes(disp, mpg)) + 
  geom_point(size = 1.5, color = "blue") +
  theme_cowplot(12)

logo_file <- system.file("extdata", "logo.png", package = "cowplot")

ggdraw(p) + 
  draw_image(logo_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)

#################################################################
#################   patchwork  #################################
#################################################################

# Так же, как и коровий плот, соединяет и маркирует несколько графиков.
# синтаксис проще предыдущего, использует действие + 

library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 + p2



ggplot(mtcars) +
  geom_point(aes(mpg, disp)) +
  ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, group = gear))

# Макеты можно указать, добавив plot_layout()вызов для сборки. 
# Это позволяет вам определять размеры сетки и сколько места выделять различным строкам и столбцам.

p1 + p2 + plot_layout(ncol = 1, heights = c(3, 1))


# Если вам нужно добавить немного пространства между вашими графиками, вы можете использовать, plot_spacer() чтобы сделать пропуск.
p1 + plot_spacer() + p2


# Вы можете сделать макет вложенных графиков, заключив часть графиков в {}. 
# В этом случае макет ограничен различными уровнями вложенности.

p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

p4 + {
  p1 + {
    p2 +
      p3 +
      plot_layout(ncol = 1)
  }
} +
  plot_layout(ncol = 1)

# Добавить аннотацию к первому графику
p1 + p2 + plot_annotation(title = "A great plot!", tag_levels = "A")


#  -  Уровень вложенности (как {} )
p1 + p2 + p3 + plot_layout(ncol = 1)

p1 + p2 - p3 + plot_layout(ncol = 1)


#  | и / для горизонтальной и вертикальной компоновки соответственно
(p1 | p2 | p3) /
  p4

# & или * для добавления элементов во все графики. 
# Второй отличаются тем, что * будут влиять только на графики на текущем уровне вложенности:
(p1 + (p2 + p3) + p4 + plot_layout(ncol = 1))

(p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) * theme_bw()


#  тогда как & вернется только во вложенные уровни:
p1 + (p2 + p3) + p4 + plot_layout(ncol = 1) & theme_bw()


################################################################
############   GGanymate   #####################################
################################################################

# 1.
#  ERROR: configuration failed for package ‘gifski
# ""Please install cargo / rustc: ""
# For Windows:
# URL: https://forge.rust-lang.org/infra/other-installation-methods.html
# https://static.rust-lang.org/rustup/dist/i686-pc-windows-gnu/rustup-init.exe
# На Unix запустите "curl https://sh.rustup.rs -sSf | sh"

# 2.  
install.packages("gifski")
install.packages("png")
library(gifski)
library(ggplot2)
library(gganimate)
library(png)
library(dplyr)

ggplot(airquality) + 
  geom_line(aes(x = Day, y = Temp, group = Month))

last_plot() + 
  transition_reveal(Day)

########################################################################
########################################################################
########################################################################



# Firework colours

colours <- c(
  'lawngreen',
  'gold',
  'white',
  'orchid',
  'royalblue',
  'yellow',
  'orange'
)
# Produce data for a single blast
blast <- function(n, radius, x0, y0, time) {
  u <- runif(n, -1, 1)
  rho <- runif(n, 0, 2*pi)
  x <- radius * sqrt(1 - u^2) * cos(rho) + x0
  y <- radius * sqrt(1 - u^2) * sin(rho) + y0
  id <- sample(.Machine$integer.max, n + 1)
  data.frame(
    x = c(x0, rep(x0, n), x0, x),
    y = c(0, rep(y0, n), y0, y),
    id = rep(id, 2),
    time = c((time - y0) * runif(1), rep(time, n), time, time + radius + rnorm(n)),
    colour = c('white', rep(sample(colours, 1), n), 'white', rep(sample(colours, 1), n)),
    stringsAsFactors = FALSE
  )
}
# Make 20 blasts
n <- round(rnorm(20, 30, 4))
radius <- round(n + sqrt(n))
x0 <- runif(20, -30, 30)
y0 <- runif(20, 40, 80)
time <- runif(20, max = 100)
fireworks <- Map(blast, n = n, radius = radius, x0 = x0, y0 = y0, time = time)
fireworks <- dplyr::bind_rows(fireworks)


##################
##################

ggplot(fireworks) + 
  geom_path(aes(x = x, y = y, group = id, colour = colour)) + 
  scale_colour_identity()

### Ill

ggplot(fireworks) + 
  geom_point(aes(x, y, colour = colour, group = id), size = 0.5, shape = 20) + 
  scale_colour_identity() + 
  coord_fixed(xlim = c(-65, 65), expand = FALSE, clip = 'off') +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'black', colour = NA), 
        panel.border = element_blank()) + 
  # Here comes the gganimate code
  transition_components(time, exit_length = 20) + 
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_wake(0.05, size = 3, alpha = TRUE, wrap = FALSE, 
              falloff = 'sine-in', exclude_phase = 'enter') + 
  exit_recolour(colour = 'black')

# transition_component() позволяет  всем точкам следовать своей траектории и временной шкале независимо друг от друга,
# ease_aes() гарантирует, что скорость точек сужается,
# shadow_wake() отвечает за след после каждой точки 
# exit_recolour()обеспечивает постепенное исчезновение точек на черном фоне. однажды они «сгорят».