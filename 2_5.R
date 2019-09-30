if(!require('pacman')) install.packages('pacman')
pacman::p_load(data.table)

flights <- fread("https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv")

X <- data.table(x = rnorm(100), y = rpois(100, 10), z = 1:100)
X
head(X, 10)

str(flights)
summary(flights)
flights

# Фильтрация
flights[1:9]
flights[5:10]
flights[c(1, 11, 13)]
flights[month == 2 | month == 5]
flights[month %in% c(2, 5)]

flights[month %between% c(2, 5)]
unique(flights[month %between% c(2, 5), month])

unique(flights[month %between% c(2, 5), carrier])
flights[carrier %chin% c('AA', 'WN')]

system.time(flights[order(arr_time)])
system.time(flights[base::order(arr_time)])

system.time(flights[carrier %chin% c('AA', 'WN')])
system.time(flights[carrier %in% c('AA', 'WN')])

flights[month == 2 & day == 5]

flights[tailnum %like% '^N3']

# Переменные
flights[, .(dep_delay)]
flights[, 'dep_delay']
flights[, c('dep_delay', 'dep_time')] # Только для функций
flights[, -c('dep_delay')]
flights[, -'dep_delay']

flights[, .(DEPARTURE = dep_time, ARRIVAL = arr_time)]

flights[, .(MEAN_DELAY = mean(dep_delay), MEAN_ARR_DELAY = mean(arr_delay))]
flights[, mean(arr_delay)]
flights[, list(mean(arr_delay))]

flights[month == 2, .(MEAN_DEP_DELAY = mean(dep_delay))]

flights[, -'carrier']
flights[, -(1:5)]

# Агрегация
flights[, .(MEAN_DEP_DELAY = mean(dep_delay)), by = month]

flights[, .(MEAN_DEP_DELAY = mean(dep_delay)), by = .(month, day)]

flights[, .(MEAN_DEP_DELAY = mean(dep_delay)), by = .(carrier, month)]

# Спецсимволы
flights[, .N]
flights[.N]  # Последнее наблюдение
flights[, .N, by = carrier]

flights[, .N, by = carrier][order(N)]
flights[, .N, by = carrier][order(N)][1:3]

flights[, .SD[which.min(dep_delay)],
        by = month]

flights[, lapply(.SD, mean), by = month, .SDcols = c('dep_delay', 'arr_delay')]

################

flights[carrier %chin% c('AA', 'US'), .(mean_arr_time = mean(arr_time)), by = month]
flights[carrier %chin% c('AA', 'US'),
        .(mean_arr_time = mean(arr_time)), by = .(carrier, month)]
flights[, uniqueN(tailnum), by = .(month, origin)]

flights[, lapply(.SD, uniqueN), by = .(month, origin), .SDcols = c('tailnum', 'dest')]

flights[, .N, by = .(origin, dest)]

flights[, .N, by = .(origin, dest)][order(N)]
flights[, .N, by = .(origin, dest)][order(-N)]
flights[, .N, by = .(origin, dest)][order(N, decreasing = TRUE)]