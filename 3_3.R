## Рабочий код занятия

require(data.table) 
setDTthreads(3)
options(width = 170L)

DT = fread("https://raw.githubusercontent.com/wiki/Rdatatable/data.table/data/melt_default.csv")
DT
############## melt ############################################################################
# Переформатировать DT в “длинную” форму, в которой каждый dob является отдельным наблюдением.
## Args:
## measure.vars = "наблюдения из этих колонок группируются в одну, с добавлением каждой colName"
## id.vars = "наблюдения из этих колонок дублируются с привязкой к каждому наблюдению"
## variable.name  и value.name - "переименовать вновь созданные соответствующие столбцы"
## variable(value).factor = TRUE  "преобразовать вновь созданные соответствующие столбцы в factor"



DT.m1 = melt(DT, id.vars = c("family_id", "age_mother"), 
             measure.vars = c("dob_child1", "dob_child2", "dob_child3"))
DT.m1

DT.m2 = melt(DT, id.vars = c("family_id", "age_mother"), 
             measure.vars = c("dob_child1", "dob_child2",
                              "dob_child3"),
             variable.name = 'child',
             value.name = 'dob')
DT.m2

DT.m2 = melt(DT, id.vars = c("family_id", "age_mother"), 
             measure.vars = grep('child', names(DT)),
             variable.name = 'child',
             value.name = 'dob')
DT.m2

############## dcast ############################################################################
# переформатирование из “длинного” формата в “широкий"
## 

ChickWeight = as.data.table(ChickWeight)
setnames(ChickWeight, tolower(names(ChickWeight))) ## setnames tolower
DT <- melt(as.data.table(ChickWeight), id=2:4)
DT

# dcast is an S3 method in data.table from v1.9.6
dcast(DT, time ~ variable, fun=mean) # using partial matching of argument
dcast(DT, diet ~ variable, fun=mean)
dcast(DT, diet+chick ~ time, drop=FALSE)
dcast(DT, diet+chick ~ time, drop=FALSE, fill=0)

# using subset
dcast(DT, chick ~ time, fun=mean, subset=.(time < 10 & chick < 20))

set.seed(1111)
DT <- data.table(
  measure_id = 111:125,
  group = rep(LETTERS[1:5], each = 3),
  measure = rep(
    sort(
      paste0(
        replicate(
          3,
          paste(
            sample(letters[1:20], sample(3:5)),
            collapse = ''
          )
        ),
        replicate(3, sample(2:5, size= 1))
      )
    ),
    5
  ),
  value = rnorm(15, mean = 10, sd = 4)
)
DT

dcast(DT, group ~ measure)
#########################################################################################
#########################################################################################

# drop argument, #1512
DT <- data.table(v1 = c(1.1, 1.1, 1.1, 2.2, 2.2, 2.2),
                 v2 = factor(c(1L, 1L, 1L, 3L, 3L, 3L), levels=1:3),
                 v3 = factor(c(2L, 3L, 5L, 1L, 2L, 6L), levels=1:6),
                 v4 = c(3L, 2L, 2L, 5L, 4L, 3L))
DT
# drop=TRUE
dcast(DT, v1 + v2 ~ v3, drop=FALSE)          # ++demo1 all missing combinations of both LHS and RHS
dcast(DT, v1 + v2 ~ v3)                      # ++demo2 default is drop=TRUE

#########################################################################################
#########################################################################################

dcast(DT, v1 + v2 ~ v3, drop=c(FALSE, TRUE)) # all missing combinations of only LHS
dcast(DT, v1 + v2 ~ v3, drop=c(TRUE, FALSE)) # all missing combinations of only RHS

# using . and ...
DT <- data.table(v1 = rep(1:2, each = 6),
                 v2 = rep(rep(1:3, 2), each = 2),
                 v3 = rep(1:2, 6),
                 v4 = rnorm(6))
DT
dcast(DT, v1 + v2 ~ v3, 
      value.var = "v4") #same as v1 + v2 ~ v3, value.var = "v4"
dcast(DT, v1 + v2 + v3 ~ ., value.var = "v4")

## for each combination of (v1, v2), add up all values of v4
dcast(DT, v1 + v2 ~ ., value.var = "v4", fun.aggregate = sum)

# fill and types
dcast(DT, v2 ~ v3, value.var = 'v1', fill = 0L)  #  0L --> 0
dcast(DT, v2 ~ v3, value.var = 'v4', fill = 1.1) # 1.1 --> 1L

#############
so <- fread('so.csv', encoding = 'UTF-8', )
# so
str(so)

so1 <- so[c(10:100), .(V1, V6, V8)]
head(so1)

so1 <- so1[, .(user = V1, text = V6, id = V8)]
colnames(so1) <- c('user', 'text', 'id')
str(so1)
head(so1)

so1.d1 <- dcast(so1, id ~ user, value.var = 'text')
names(so1.d1)
str(so1.d1)

so1.d2 <- dcast(so1, id ~ user, value.var = 'text', drop = TRUE)
dim(so1.d1)
dim(so1.d2)

# colnames(so1) <- c('user', 'text', 'id')
t <- so[V1 == 'Darlana', V6]
str(t)

t1 <- unlist(strsplit(t, split = '[\\.\\!\\?]\\s?'))
str(t1)
t1[1:10]

# t2 <- gsub('[\\.\\!\\?]\\s?', '\\.\n', t)
# str(t2)
# head(t2, 20)
# cat(t2[20])

# t3 <- split(t[20], t[20], sep = "[\\.\\!\\?]\\s?")
# str(t3)

# gsub("[[:digit:]]", " ", x, perl = T)
# gsub("[a-zA-Z]", "", x, perl = T)
# gsub("\\n", "", x) 
# gsub('\\"', "", x, perl = T)

# t2 <- gsub("[[:digit:]]", " ", t1, perl = TRUE)
# str(t2)

t2 <- as.data.table(t1)
dim(t2)
str(t2)
setindex(t2, t1)
t2 <- t2[
  t1 != "^[[:digit:]].*"
]
t1[1:10]
t2[1:10]

t3 <- t2[1:10]
setindex(t3, t1)
t4 <- t3[
  !grepl("^[[:digit:]].*", t1)
]
t1[1:10]
t4

t5 <- t2[
  !grepl("^[[:digit:]].*", t1, perl = TRUE) &
  t1 != ' ' &
  !grepl("^[a-zA-Z]+$", "", x, perl = T)
]

require(udpipe)
tx <- function(z) {
  str_flatten(
    as_phrasemachine(
      as.data.frame(
        udpipe_annotate(udmodel, z, parser =  "none",   trace = F)
      )$upos
    )
  )
}
# У меня время вышло (
#  Я на работе, пора домой
#############

# multiple value.var and multiple fun.aggregate
DT = data.table(x=sample(5,20,TRUE), y=sample(2,20,TRUE),
                z=sample(letters[1:2], 20,TRUE), d1 = runif(20), d2=1L)
# multiple value.var
dcast(DT, x + y ~ z, fun=sum, value.var=c("d1","d2"))
# multiple fun.aggregate
dcast(DT, x + y ~ z, fun=list(sum, mean), value.var="d1")
# multiple fun.agg and value.var (all combinations)
dcast(DT, x + y ~ z, fun=list(sum, mean), value.var=c("d1", "d2"))
# multiple fun.agg and value.var (one-to-one)
dcast(DT, x + y ~ z, fun=list(sum, mean), value.var=list("d1", "d2"))

################################################################################################
################################################################################################
################################################################################################


dataurl <- "https://jozef.io/post/data/"
flights <- readRDS(url(paste0(dataurl, "r006/flights.rds")))
flights <- as.data.table(flights)[month < 3]
flights

# создать сводку расстояний, пройденных за месяц, и аэропорта, отправляющего данные
flights[, sum(distance), by = c("month", "origin")]

#назвать новый столбец "distance"
flights[, .(distance = sum(distance)), by = c("month", "origin")]

# создать  сводную таблицу
cubed <- data.table::cube(
  flights,
  .(distance = sum(distance)),
  by = c("month", "origin")
)
cubed

#преобразовать данные в широкий формат
# - Origins in columns, months in rows
data.table::dcast(cubed, month ~ origin,  value.var = "distance")

# - Origins in rows, months in columns
data.table::dcast(cubed, origin ~ month,  value.var = "distance")


#для создания сводок с более чем двумя измерениями
# With 3 dimensions:
cubed2 <- cube(
  flights, 
  .(distance = sum(distance)),
  by = c("month", "origin", "carrier")
)
cubed2

# через dcast()в широком формате
dcast(cubed2, month + carrier ~ origin,  value.var = "distance")