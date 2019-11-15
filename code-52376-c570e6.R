# 
# getwd()
# setwd("/home/df/OTUS/ggplot_4_4/")

if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  tidyverse,
  data.table,
  dbplyr,
  cowplot,
  listviewer,
  histogram,
  seqinr,
  patchwork,
  gifski,
  png,
  ggplot2,
  gganimate,
  ggforce,
  mvtnorm,
  magick,
  rsvg,
  plotly
)

cowplot :: draw_image("https://plotly-r.com/images/workflow.svg")


library(ggplot2)
library(plotly)
library(magick)
library(rsvg)
library(png)
library(jpeg)
library(svglite)
library(plotly)
library(RColorBrewer)
library(ggforce)
data(diamonds, package = "ggplot2")
magick::image_read_svg("https://plotly-r.com/images/workflow.svg")

p2 <- cowplot::ggdraw() + cowplot::draw_image("https://plotly-r.com/images/workflow.svg", scale = 0.9)
p2 <- cowplot::ggdraw() + cowplot::draw_image("http://jeroen.github.io/images/tiger.svg", scale = 0.9)
p2 <- cowplot::ggdraw() + cowplot::draw_image("https://plotly-r.com/images/color-mapping.svg", scale = 1)
cowplot::plot_grid(p2, labels = "AUTO")

####################################################################################
####################################################################################
####################################################################################

# Основное различие между интерактивной и пассивной графикой заключается в том, 
# что активная реагирует на действия пользователя. В пассивной графике, графический
# интерфейс не делает ничего особенного, когда пользователь пытается взаимодействовать
# с ним. Это может быть изменение цвета при наведении мышкой или воспроизведение 
# графической анимации во время нажатия на нее.
# Основным недостатком использования интерактивной графики является дополнение 
#к размеру веб-страницы. 
library(plotly)
library(RColorBrewer)
data(diamonds, package = "ggplot2")
diamonds

# Если мыы используем VAR для x, y, color и т.д., 
# plot_ly() самостоятельно пытается найти  геометрическое представление этих данных

plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

# отбражение использованной палитры
p2 <- cowplot::ggdraw() + cowplot::draw_image("https://plotly-r.com/images/color-mapping.svg", scale = 1)
cowplot::plot_grid(p2, labels = "AUTO")

# Выбор палитры colors = "Accent" из примера выше 
# (Используется палитра из RColorBrewer)
display.brewer.pal(7,"BrBG")
display.brewer.pal(7,"Accent")
# название и отображение всех палитр:
display.brewer.all(n=10, exact.n=FALSE)
# text
RColorBrewer::brewer.pal.info[1]


# colors = "Paired"
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Paired")
# выглядит так себе; подбираем другой вариант:
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Spectral")
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Dark2")
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Purples")
# "фиолетовый в крапинку" (С) смотрится лучше

# неожиданные результаты: doesn't produce black bars
plot_ly(diamonds, x = ~cut, color = "black")

# Использование I()для непосредственного предоставления визуальных свойств вместо 
# сопоставления значений с визуальным диапазоном. В верхней части этого рисунка 
# значение 'black'отображается в визуальный диапазон, охватываемый colors 
# (который для дискретных данных по умолчанию равен 'Set2').
plot_ly(
  diamonds, 
  x = ~cut, 
  color = I("red"), 
  stroke = I("black"), 
  span = I(2)
)
# добавление доп. элементов через layout
layout(
  plot_ly(diamonds, x = ~cut),
  title = "My beatiful histogram"
)

#  через %>%
diamonds %>%
  plot_ly(x = ~cut) %>%
  layout(title = "My beatiful histogram")

# семейство add_*()функций 
diamonds %>%
  plot_ly() %>% 
  add_histogram(x = ~cut)

# Аргументы, указанные в plot_ly() являются глобальными , что означает,  что любые 
# последующие add_*() функции наследуют эти аргументы (за исключением arg. "inherit = FALSE")
diamonds %>%
  plot_ly(x = ~cut) %>% 
  add_histogram() %>%
  group_by(cut) %>%
  summarise(n = n()) %>%
  add_text(
    text = ~scales::comma(n), y = ~n, 
    textposition = "top middle", 
    cliponaxis = FALSE
  )

# Для отладки добавляют plotly_data()
diamonds %>%
  plot_ly(x = ~cut) %>% 
  add_histogram() %>%
  group_by(cut) %>%
  summarise(n = n()) %>% 
  plotly_data()

##############################################################################################

# SVG format not supported text label
p2 <- cowplot::ggdraw() + cowplot::draw_image("https://plotly-r.com/images/printing.svg", scale = 0.9)
cowplot::plot_grid(p2, labels = "AUTO")
# bug: https://desk.draw.io/support/solutions/articles/16000042487-why-does-the-text-of-svg-export-sometimes-not-display-correctly-in-ie-and-some-svg-editors-
# "However, the subject is very complex and we don't have a fix so far."

# поэтому иллюстрация залита на локальный комп в png
p2 <- cowplot::ggdraw() + cowplot::draw_image("Img/RData__JSON__JScript.png", scale = 0.9)
cowplot::plot_grid(p2, labels = "AUTO")

#  plotly передает данные в   plotly.js  используя JSON format

# для отладки можно использовать plotly_build() и plotly_json()
p <- plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")
plotly_json(p)

#No trace type specified:
#Based on info supplied, a 'histogram' trace seems appropriate.
#Read more about this trace type -> https://plot.ly/r/reference/#histogram
#  Ошибка: Package `listviewer` required for `plotly_json`.
#Please install and try again.

# install.packages("listviewer"), double 2
p <- plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")
plotly_json(p)

# plotly.js фигура имеет два ключевых компонента: data (иначе traces) и a layout.
# traces определяет отображение данных и визуальные эффекты . Каждый traces 
# имеет тип (например, гистограмма, круговая, разброс и т.д.) и тип трассировки 
# определяет , через  какие другие атрибуты (например, визуальные и / или 
# интерактивные свойства, как x, hoverinfo, name) можно управлять отображением трассировки. 


# иногда проще использовать plotly_build()
b <- plotly_build(p)
###No trace type specified:
###Based on info supplied, a 'histogram' trace seems appropriate.
###Read more about this trace type -> https://plot.ly/r/reference/#histogram


# Confirm there 8 traces
length(b$x$data)
#> [1] 8

# Extract the `name` of each trace. plotly.js uses `name` to 
# populate legend entries and tooltips
purrr::map_chr(b$x$data, "name")
#> [1] "IF" "VVS1" "VVS2" "VS1" "VS2" "SI1" "SI2" "I1" 

# Every trace has a type of histogram
unique(purrr::map_chr(b$x$data, "type"))
#> [1] "histogram"

#############

# встраивание одного графика в другой:
p1 <- plot_ly(x = c(1, 2, 3), y = c(4, 3, 2))
p2 <- plot_ly(x = c(20, 30, 40), y = c(30, 40, 50)) %>%
  layout(xaxis = list(domain = c(0.6, 0.95)),
         yaxis = list(domain = c(0.6, 0.95)))
subplot(p1, p2)




####################################################################################
###################    ggplotly()    ###############################################
####################################################################################

# возможность перевести ggplot2 в plotly
# В сложных графиках (ggplot >> plotly) возможны error 
# http://biostat-r.blogspot.com/2015/12/plotly-r.html


dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p <- ggplot(data = dsamp, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity))) +
  geom_smooth(aes(colour = cut, fill = cut), size = 0.3) +
  facet_wrap(~cut)
gg <- ggplotly(p)
gg


# хексограмный  график зависимости алмазов в каратах от цены:
p <- ggplot(diamonds, aes(x = log(carat), y = log(price))) + 
  geom_hex(bins = 100)
ggplotly(p)


# Частотные полигоны цены на алмаз по чистоте алмаза:
p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
  geom_freqpoly()
ggplotly(p)

# facet_wrap по cut | Цена бриллианта по четкости и огранке
p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
  geom_freqpoly(stat = "density") + 
  facet_wrap(~cut)
ggplotly(p)


# еще один способ визуализации ту же информацию, используя geom_sina()
library(ggforce)
p <- ggplot(diamonds, aes(x=clarity, y=log(price), color=clarity)) +
  ggforce::geom_sina(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", color = "black") +
  facet_wrap(~cut)

# WebGL is a lot more efficient at rendering lots of points
toWebGL(ggplotly(p))

# предобработка данных через регрессию  карат-цена
m <- lm(log(price) ~ log(carat), data = diamonds)
diamonds <- modelr::add_residuals(diamonds, m)
p <- ggplot(diamonds, aes(x = clarity, y = resid, color = clarity)) +
  ggforce::geom_sina(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", color = "black") +
  facet_wrap(~cut)
toWebGL(ggplotly(p))

# введение интерактивности в график c ggcoef() (можно двигать) 
library(GGally)
m <- lm(log(price) ~ log(carat) + cut, data = diamonds)
gg <- ggcoef(m)
# dynamicTicks means generate new axis ticks on zoom
ggplotly(gg, dynamicTicks = TRUE)


#  Пакет naniar предоставляет набор вычислительных 
# и визуальных ресурсов для работы и выявления структуры в пропущенных значениях.

library(naniar)
# in dataset  вводятся поддельные недостающие значения в цену алмаза.
diamonds$price_miss <- ifelse(diamonds$depth>60, diamonds$price, NA)
p <- ggplot(diamonds, aes(x = clarity, y = log(price_miss))) +
  geom_miss_point(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  facet_wrap(~cut)
toWebGL(ggplotly(p))
# NA значения показаны красным.

# С помощью group_by() можно эффективно использовать временные ряды
library(plotly)
data(economics, package = "ggplot2")

# sort economics by psavert, just to 
# show difference between paths and lines
p <- economics %>%
  arrange(psavert) %>%
  plot_ly(x = ~date, y = ~psavert)
# Разница между add_paths()и add_lines(): верхняя панель связывает наблюдения 
# в соответствии с порядком psavert(норма личных сбережений), тогда как нижняя 
# панель связывает наблюдения в соответствии с порядком x(дата).
add_paths(p)
add_lines(p)

# один из способов борьбы с переполнением с помощью альфа-смешивания
subplot(
  plot_ly(mpg, x = ~cty, y = ~hwy, name = "default"),
  plot_ly(mpg, x = ~cty, y = ~hwy) %>% 
    add_markers(alpha = 0.2, name = "alpha")
)


# Цветовая шкала по умолчанию - это viridis
p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5)
subplot(
  add_markers(p, color = ~cyl, showlegend = FALSE) %>% 
    colorbar(title = "Viridis"),
  add_markers(p, color = ~factor(cyl))
)

# следует помнить об использовании последовательной цветовой шкалы
# для числовых переменных (и упорядоченных факторов)
col1 <- c("#132B43", "#56B1F7")
col2 <- viridisLite::inferno(10)
col3 <- colorRamp(c("red", "white", "blue"))
subplot(
  add_markers(p, color = ~cyl, colors = col1) %>%
    colorbar(title = "ggplot2 default"),
  add_markers(p, color = ~cyl, colors = col2) %>% 
    colorbar(title = "Inferno"),
  add_markers(p, color = ~cyl, colors = col3) %>% 
    colorbar(title = "colorRamp")
) %>% hide_legend()


# и качественной цветовой шкалы для дискретных переменных
col1 <- "Accent"
col2 <- colorRamp(c("red", "blue"))
col3 <- c(`4` = "red", `5` = "black", `6` = "blue", `8` = "green")
subplot(
  add_markers(p, color = ~factor(cyl), colors = col1),
  add_markers(p, color = ~factor(cyl), colors = col2),
  add_markers(p, color = ~factor(cyl), colors = col3)
) %>% hide_legend()



####################################################################################
###################    2D freq граафики   ##########################################
####################################################################################

# add_heatmap()и add_histogram2d()
# (ячейки должны быть предварительно вычислены)

# сравнение трёх различных использований add_histogram2d(): 
# (1) алгоритм разбивки по умолчанию plotly.js, 
# (2) сглаживание по умолчанию и сглаживание, 
# (3) установка количества бинов в направлениях x и y. 
p <- plot_ly(diamonds, x = ~log(carat), y = ~log(price))
subplot(
  add_histogram2d(p) %>%
    colorbar(title = "default") %>%
    layout(xaxis = list(title = "default")),
  add_histogram2d(p, zsmooth = "best") %>%
    colorbar(title = "zsmooth") %>%
    layout(xaxis = list(title = "zsmooth")),
  add_histogram2d(p, nbinsx = 60, nbinsy = 60) %>%
    colorbar(title = "nbins") %>%
    layout(xaxis = list(title = "nbins")),
  shareY = TRUE, titleX = TRUE
)

# заполненные контуры вместо бункеров можно использовать
# в любом из этих случаев, используя add_histogram2dcontour() вместо add_histogram2d()
# (аналог картографических высот)
subplot(
  add_histogram2d(p) %>%
    colorbar(title = "default") %>%
    layout(xaxis = list(title = "default")),
  add_histogram2dcontour(p, zsmooth = "best") %>%
    colorbar(title = "zsmooth") %>%
    layout(xaxis = list(title = "zsmooth")),
  add_histogram2dcontour(p, nbinsx = 60, nbinsy = 60) %>%
    colorbar(title = "nbins") %>%
    layout(xaxis = list(title = "nbins")),
  shareY = TRUE, titleX = TRUE
)
# (видно при вытягивании в ширину)


# график использует kde2d()для оценки 2D-плотности, масштабирует относительную 
# частоту до абсолютной частоты, а затем использует add_heatmap() функцию
# для отображения результатов в виде тепловой карты.

library(MASS)
kde_count <- function(x, y, ...) {
  kde <- MASS::kde2d(x, y, ...)
  df <- with(kde, setNames(expand.grid(x, y), c("x", "y")))
  # The 'z' returned by kde2d() is a proportion, 
  # but we can scale it to a count
  df$count <- with(kde, c(z) * length(x) * diff(x)[1] * diff(y)[1])
  data.frame(df)
}

kd <- with(diamonds, kde_count(log(carat), log(price), n = 30))
plot_ly(kd, x = ~x, y = ~y, z = ~count) %>% 
  add_heatmap() %>%
  colorbar(title = "Number of diamonds")


### Категориальные оси

# add_histogram2d()  можно использовать для простого отображения двусторонних таблиц
corr <- cor(dplyr::select_if(diamonds, is.numeric))
plot_ly(colors = "RdBu") %>%
  add_heatmap(x = rownames(corr), y = colnames(corr), z = corr) %>%
  colorbar(limits = c(-1, 1))
###  сгруппированные гистограммы
# для сравнения количества алмазов по наглядности, учитывая тип огранки
plot_ly(diamonds, x = ~cut, color = ~clarity) %>%
  add_histogram()




###################################################################################
###################    3D граафики   ##############################################
###################################################################################

# фактически анимация позволяет добавить ещё одну мерность в визуализацию, представив
# изменения в аналогичном человеческому восприятию виде. (изменение в зависимости от временного отрезка)


# Сетка проекций появляется при наведении мышки

# добавление zатрибута
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_markers(color = ~cyl)


# Чтобы создать траекторию в 3D, используйте так add_paths()
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_paths(color = ~displ)

# add_lines()вместо того, add_paths() 
# чтобы убедиться, что точки соединены осью X вместо порядка строк
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_lines(color = ~displ)

# вы можете создать несколько линий, указав переменную группировки
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  group_by(cyl) %>%
  add_lines(color = ~displ)


# заголовки осей (например, рисунок 8.5 ) или что-то еще, 
# специфичное для определения оси, делают через scence
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_lines(color = ~displ) %>%
  layout(
    scene = list(
      xaxis = list(title = "MPG city"),
      yaxis = list(title = "MPG highway"),
      zaxis = list(title = "Number of cylinders")
    )
  )

####################################################################################
###################    3D Поверхности   ############################################
####################################################################################

# Создание трехмерных поверхностей add_surface()
# во многом похоже на создание тепловых карт add_heatmap()
# При этом должно быть разумное упорядочение по осям x / y на графике поверхности,
# поскольку plotly.js интерполирует значения z

x <- seq_len(nrow(volcano)) + 100
y <- seq_len(ncol(volcano)) + 500
plot_ly() %>% add_surface(x = ~x, y = ~y, z = ~volcano)





###################################################################################
###################    Организация просмотров   ###################################
###################################################################################

###  subplot()
library(plotly)
p1 <- plot_ly(economics, x = ~date, y = ~unemploy) %>% 
  add_lines(name = "unemploy")
p2 <- plot_ly(economics, x = ~date, y = ~uempmed) %>% 
  add_lines(name = "uempmed")
subplot(p1, p2)

# один временной ряд для каждой переменной в economics наборе данных и общая ось X
vars <- setdiff(names(economics), "date")
plots <- lapply(vars, function(var) {
  plot_ly(economics, x = ~date, y = as.formula(paste0("~", var))) %>%
    add_lines(name = var)
})
subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

# Количество строк (и, как следствие, количество столбцов) указывается через nrows

# как показано на рисунке, зн. по умолчанию могут быть изменено с помощью heights и widths аргументов
p2 <- cowplot::ggdraw() + cowplot::draw_image("Img/subplot.png", scale = 0.9)
cowplot::plot_grid(p2, labels = "AUTO")


# график плотности суставов в действительности представляет собой участок плотностей суставов и пределов
library(mvtnorm)
s <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
m <- mvtnorm::rmvnorm(1e5, sigma = s)
x <- m[, 1]
y <- m[, 2]
s <- subplot(
  plot_ly(x = x, color = I("black")), 
  plotly_empty(), 
  plot_ly(x = x, y = y, color = I("black")) %>%
    add_histogram2dcontour(colorscale = "Viridis"), 
  plot_ly(y = y, color = I("black")),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
  shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
)
layout(s, showlegend = FALSE)

###################################################################################
###################    Анимация   #################################################
###################################################################################

# пример для ДЗ
data(gapminder, package = "gapminder")
gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10()
ggplotly(gg)

# показаны те же данные, но удваивается количество времени между кадрами,
# используется замедление линейного перехода, размещаются кнопки анимации 
# ближе к ползунку и изменяются currentvalue.prefix настройки ползунка по умолчанию

base <- gapminder %>%
  plot_ly(x = ~gdpPercap, y = ~lifeExp, size = ~pop, 
          text = ~country, hoverinfo = "text") %>%
  layout(xaxis = list(type = "log"))

base %>%
  add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red"))
  )
# дёрганная анимация


# континенты (т. Е. Кадры) упорядочены в соответствии с их средней ожидаемой
# продолжительностью жизни в разных странах континента. Кроме того, поскольку 
# нет значимой связи между объектами в разных кадрах, длительность плавного 
# перехода устанавливается равной 0.

meanLife <- with(gapminder, tapply(lifeExp, INDEX = continent, mean))
gapminder$continent <- factor(
  gapminder$continent, levels = names(sort(meanLife))
)

base %>%
  add_markers(data = gapminder, frame = ~continent) %>%
  hide_legend() %>%
  animation_opts(frame = 1000, transition = 0, redraw = FALSE)

# frameи idsатрибуты работают на уровне трассировки - 
# это означает, что мы можем нацеливать определенные слои графика для анимации

# слои анимированных кадров располагаются поверх фона всех кадров. В результате 
# легче поместить конкретный год в глобальный контекст.

base %>%
  add_markers(
    color = ~continent, showlegend = F,
    alpha = 0.2, alpha_stroke = 0.2
  ) %>%
  add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
  animation_opts(1000, redraw = FALSE)


