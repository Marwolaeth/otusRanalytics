if(!require('pacman')) install.packages('pacman')
pacman::p_load(data.table, lubridate, ggplot2, stringr, colorspace)

ggplot(so_sum, aes(x = hour, y = p_words, colour = factor(user))) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 22, 2))

ggplot(
  so_sum,
  aes(
    x = hour,
    y = p_words,
    colour = factor(user),
    fill = factor(user)
  )) +
  geom_line(lwd = 2.5, show.legend = FALSE) +
  scale_x_continuous(
    'Время суток',
    breaks = seq(0, 23, 1),
    limits = c(0, 23),
    expand = c(.01, .01)
  ) +
  scale_y_continuous('Доля от всех слов') +
  scale_colour_discrete_qualitative(
    name = 'Пользователь',
    palette = 'Dynamic'
  ) +
  facet_wrap(~ user, ncol = 1) +
  theme_light()

  ################
  
ggplot(
  so_sum,
  aes(
    x = hour,
    y = md_words,
    colour = factor(user),
    fill = factor(user)
  )) +
  geom_line(lwd = 2, show.legend = FALSE) +
  geom_linerange(
    aes(
      ymin = q1_words,
      ymax = q3_words,
      colour = factor(user),
      group = user
    ),
    alpha = .3,
    size = 12,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    'Время суток',
    breaks = seq(0, 23, 1),
    limits = c(0, 23),
    expand = c(.01, .01)
  ) +
  scale_y_continuous('Доля от всех слов') +
  scale_colour_discrete_qualitative(
    name = 'Пользователь',
    palette = 'Dynamic'
  ) +
  facet_wrap(~ user, ncol = 1) +
  theme_light()
