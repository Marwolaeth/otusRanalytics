if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  data.table,
  lubridate,
  ggplot2,
  stringr,
  colorspace,
  plotly
)

if (!exists('so')) {
  so <- fread('so.csv', encoding = 'UTF-8')
  so_ind <- copy(so) 
}

setnames(
  so,
  c('user', 'usid', 'postid', 'date', 'quoter', 'text', 'url', 'id', 'smth')
)

so[, c('smth') := NULL]
str(so)

so[, created := as.POSIXct(
  date,
  tz = 'UTC',
  format = ' %a %b %d, %Y %I:%M %p '
)
]

so <- so[!is.na(created)]
so_cl <- copy(so)
# so <- copy(so_cl)

# Ограничение периода
so <- so[year(created) > 2016]

so[, hour := {
  # Округлим даты при помощи lubridate
  h = floor_date(created, unit = '2 hours')
  # Другой вариант: as.numeric(format(created, '%H'))
  h = hour(h)
}
]

so[, epoch := floor_date(created, unit = '3 months')][
  , epoch_ch := format(epoch, format = '%B %Y')
  # Человеческое название «эпохи»
]

setkey(so, user)
so <- so[user %chin% so[, .(.N, first = min(created)), by = user][
  first <= ymd('2017-01-13')][
    order(-N)][
      1:5, user]]

so[, .(first_post = min(created),
       last_post  = max(created)),
   by = user]

so[, nwords := stringr::str_count(text, "\\w+")]

so[, .(
  sum    = sum(nwords),
  mean   = mean(nwords),
  median = median(nwords),
  nposts = .N
),
by = user
]

so_sum <- so[,
             .(n_words  = median(nwords, na.rm = TRUE),
               .N
              ),
             keyby = .(user, epoch, epoch_ch, hour)
             ]

so_total <- so_sum[,
                   .(total_words = sum(n_words)),
                   by = .(user, epoch)
                   ]

so_sum <- so_total[so_sum]
rm(so_total)
so_sum[, p_words := n_words / total_words]

so_sum[, epoch_n := as.numeric(factor(epoch))]
setkey(so_sum, NULL)

p <- ggplot(
  so_sum,
  aes(
    x = hour,
    y = p_words,
    colour = user
  )
) +
  geom_line(aes(frame = epoch, ids = hour))

ggplotly(p) %>%
  animation_opts(frame = 1500, easing = 'elastic', redraw = FALSE) %>%
  animation_slider(
    currentvalue = list(prefix = '', suffix = '')
  )

#################################################
p <- ggplot(
  so_sum,
  aes(
    x = hour,
    y = p_words,
    colour = factor(user)
  )) +
  geom_line(
    aes(
      frame = epoch,
      ids = hour
    ),
    lwd = 2,
    show.legend = FALSE) +
  scale_x_continuous(
    'Время суток',
    breaks = seq(0, 22, 2),
    minor_breaks = NULL,
    limits = c(0, 22),
    expand = c(.01, .01),
    labels = function(x) sprintf('%d–%d', x, x + 2)
  ) +
  scale_y_continuous(
    '% от всех слов',
    labels = function(x) scales::percent(x, accuracy = 1)
  ) +
  scale_colour_discrete_qualitative(
    name = 'Пользователь',
    palette = 'Dynamic'
  ) +
  facet_wrap(~ user, ncol = 1) +
  theme_light() +
  ggtitle('ТОП-5 пользователей: активность по времени суток')

ggplotly(p) %>%
  animation_slider(
    currentvalue = list(prefix = 'Период: ')
  ) %>%
  animation_opts(frame = 1500, easing = 'elastic', redraw = TRUE)
