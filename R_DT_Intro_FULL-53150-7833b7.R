#### Data.table Intro ####

library(data.table)

# Создание DT
X <- data.table(id = c("a", "b", "c"), value = c(0.5, 1.0, 1.5))


# Загрузка из удаленного источника
flights <- fread("https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv")

head(flights)

str(flights)

# фильтрация
(row_5 <- flights[5])
flights[4:10]
flights[c(1, 4, 7)]
flights[c(1, .N)]
flights[(.N-10):.N]

flights[carrier == "UA"]

flights[carrier == "UA" & origin == "EWR"]

flights[carrier == "UA" & origin == "EWR" & dest != "TPA"]


unique(flights$carrier)
(like_L <- flights[carrier %like% "^A"])

flights[month %between% c(3, 7)]

flights[carrier %like% "^A"]

AA_DL <- flights[carrier %chin% c("AA", "DL")]
unique(AA_DL$carrier)



# выбор колонок

flights[, dep_delay]

flights[, "dep_delay"]

flights[, .(dep_delay, arr_delay)]

flights[, -c("dep_delay", "arr_delay")]

flights[, .(dep_delay, arr_delay)][, mean(dep_delay)]

flights[, .(NEWNAME = dep_delay, NAME2 = arr_delay)]
flights[, .(MEAN = mean(dep_delay), STD = sd(dep_delay))]


# группирования 
flights[, .(mean_delay = mean(dep_delay)), by = month]

flights[, .(mean_delay = mean(dep_delay), N_flights = .N), by = month]

flights[, .(mean_delay = mean(dep_delay), N_flights = .N), by = month
      ][order(mean_delay, decreasing = TRUE)
      ][1:5]


# subset of data

flights[, print(.SD), by = carrier]

flights[, .SD[which.min(dep_delay)],
        by = month, 
        .SDcols = c("arr_time")]

# не сработает
flights[, .SD[which.min(dep_delay)],
        by = month, 
        .SDcols = .(arr_time)]


####

flights[, lapply(.SD, uniqueN), 
        by = .(month, origin), 
        .SDcols = c("tailnum", "dest")]

flights[, .N, 
        by = .(origin, dest)]
