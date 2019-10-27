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

display.brewer.all(3, type = 'qual', colorblindFriendly = TRUE)

ggplot2movies::movies %>%
  group_by(year) %>%
  summarise(avg_budget = mean(budget, na.rm = TRUE)) %>%
  filter(year >= 1970) %>% # Иначе слишком много значений
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = avg_budget)) +
  geom_col(width = .7, fill = 'darkgrey') +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

gapminder %>%
  filter(year == last(unique(year))) %>% # Сравниваем ВВП для одного периода
  group_by(continent) %>%
  summarise(
    min = min(gdpPercap, na.rm = TRUE),
    max = max(gdpPercap, na.rm = TRUE)
  ) %>%
  gather('rank', 'gdp', -continent, factor_key = TRUE) %>%
  ggplot(aes(x = continent, y = gdp, fill = rank)) +
  geom_col(
    position = position_dodge2(
      width = .9, padding = .1
    )
  ) +
  scale_y_continuous('GDP per capita') +
  scale_fill_brewer('Rank', palette = pals[2]) +
  theme_minimal()

ggplot(salaries, aes(x = rank, y = salary, colour = sex)) +
  geom_jitter(alpha = .5, position = position_jitterdodge(jitter.width = .2)) +
  scale_colour_brewer(palette = pals[3]) +
  stat_summary(
    mapping = aes(group = sex),
    fun.y = median,
    geom = 'point',
    shape = 18,
    size = 3,
    position = position_dodge(width = .2)
  ) +
  stat_summary(
    mapping = aes(group = sex),
    fun.data = median_hilow,
    geom = 'errorbar',
    width = .2,
    lwd = 1,
    position = 'dodge'
  ) +
  theme_minimal()

ggplot(
  salaries,
  aes(x = yrs.since.phd, y = salary, colour = discipline)
) +
  geom_jitter() +
  stat_smooth(span = .8) +
  scale_x_continuous('Years since PhD') +
  scale_colour_brewer(palette = pals[3]) +
  theme_minimal()
