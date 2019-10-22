if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, ggthemes, gapminder, txhousing, reshape2)

str(mpg)

plot(
  cty ~ displ,
  data = mpg
)

ggplot(data = mpg, aes(displ, cty, colour = factor(cyl), shape = factor(cyl))) +
  geom_point()

ggplot(data = mpg, aes(x = displ, y = cty)) +
  geom_point()

ggplot(data = mpg, aes(x = displ, y = cty)) +
  geom_point(size = .7)

?ggsave
?png
?pdf
?print.ggplot
?dev.off

png(
  filename = 'crossplot.png'
)

ggplot(
  data = mpg,
  aes(x = displ, y = cty, colour = class)
) +
  geom_point()

ggplot(
  data = mpg,
  aes(x = displ, y = cty, colour = hwy, shape = drv)
) +
  geom_point()

if (!require(gapminder)) install.packages('gapminder')
library(gapminder)

ggplot(
  data = gapminder,
  aes(
    x = log(gdpPercap),
    y = lifeExp,
    colour = continent
  )
) +
  geom_point()

ggplot(
  data = gapminder,
  aes(
    x = gdpPercap,
    y = lifeExp,
    colour = continent
  )
) +
  geom_point() +
  stat_smooth()

if (!require(txhousing)) install.packages('txhousing')
library(txhousing)

str(txhousing)

txhousing %>%
  filter(city %in% c('Houston', 'Austin')) %>%
  ggplot(
    aes(x = date, y = median, colour = city)
  ) +
  geom_line()

###### reshape ######
plt <- mpg %>%
  select(class, cty, hwy) %>%
  melt(id = 'class', variable.name = 'terrain') %>%
  ggplot(
    aes(x = class, y = value, colour = terrain)
  ) +
  geom_boxplot()
plt + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

str(txhousing)
txhousing %>%
  group_by(city) %>%
  summarise(total_sales = sum(sales)) %>%
  ggplot(
    aes(x = city, y = total_sales)
  ) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
