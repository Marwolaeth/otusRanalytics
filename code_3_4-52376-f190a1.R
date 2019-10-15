require(data.table) 
setDTthreads(2)
options(width = 170L)

# setwd("/home/df/Desktop/OTUS/Arh_old_courses/SO_ex/")
# getwd()
so <- fread("so.csv", encoding = 'UTF-8')
so_ind <- copy(so)
setnames(so, c("user", "usid", "postid", "date", "quoter", "text", "url", "id", ""))

###сводная таблица (длинная и узкая, аналог melt() )

dt <- data.table(mtcars)[, .(cyl, gear)]
dt
dt[,unique(gear), by=cyl]

ls()
so[, unique(quoter), by = user][100:155]


###сводная таблица (короткая и узкая)
dt <- data.table(mtcars)[,.(gear, cyl)]
dt[, gearsL := list(list(unique(gear))), by=cyl]
dt[, gearsL := .(list(unique(gear))), by=cyl] # вспоминаем сокращение "list() ==  .()"
head(dt)
dt[, .(list(unique(gear))), by=cyl]

so1 <- so[, .(user, quoter)]
so1[, uni_q := .(list(unique(quoter))), by = user]
so1[1, uni_q][]
so1[53301, .(user, uni_q)][] ### значение колонки uni_q начинается со значения "", поэтому отображается как:
#    user                                          uni_q
# 1:  blk ,Istanaro,Мэйпл,Lisabet,Полина Ф.,mr.Midas,...



######################################################
########## REGEXP ####################################
######################################################


# Creating a large data.table with 100k rows, 32 columns
n <- 100000L
foo_cols <- paste0("foo", 1:30)
big_dt <- data.table(bar = rnorm(n), baz = rnorm(n))
big_dt[, (foo_cols) := rnorm(n)]

# Methods
subsetting <- function(dt) {
  subset(dt, select = grep("bar|baz", names(dt)))
}

usingSD <- function(dt) {
  dt[, .SD, .SDcols = names(dt) %like% "bar|baz"]
}

usingWith <- function(dt) {
  cols <- grep("bar|baz", names(dt), value = TRUE)
  dt[, cols, with = FALSE]
}

usingDotDot <- function(dt) {
  cols <- grep("bar|baz", names(dt), value = TRUE)
  dt[, ..cols]
}

# Benchmark
microbenchmark(
  subsetting(big_dt), usingSD(big_dt), usingWith(big_dt), usingDotDot(big_dt),
  times = 50
)

######################################################
######################################################
######################################################


### Доступ к элементам из вложенного списка (столбца списков)
# Извлеките второй элемент каждого списка в gearsL и запишите его в созданную строку gearL1. 
dt[,gearL1:=lapply(gearsL, function(x) x[2])]
head(dt)
str(head(dt[,gearL1]))
dt[,gearS1:=sapply(gearsL, function(x) x[2])] 
head(dt)
str(head(dt[,gearS1]))


## то же на табл. so1,   дергаем 3-й элемент списка
head(so1)
so1[, uni_q3 := sapply(uni_q, function(x) x[3])]
head(so1)
str(head(so1[,uni_q3]))

so1[, uni_qL3 := lapply(uni_q, function(x) x[3])]
str(head(so1[,uni_qL3])) ### без измерений видно, то lapply раза в 2 - в 3  подтормаживает

###  Упрощение синтаксиса согласно комментариям Мэтта Доула
dt[,gearS1 := sapply(gearsL, `[`, 2)]
dt[,gearM1 := purrr::map_dbl(gearsL, 2)]
dt[,gearM2 := purrr::map(gearsL, 2)]
str(dt)
so1[, uni_q3 := sapply(uni_q, "[", 3)]
head(so1)

# mcapply

so1[, uni_q3]
so1[, uni_q3 := NULL] ## удаляем
so1[, uni_qL3 := NULL]

DT <- data.table(id = c(1,2,2,3,3,3))[, v := LETTERS[1:.N]]
DT[, .SD[1L], by = id]

######################################################
# Подавление промежуточного вывода с помощью {} 

dt <- data.table(mtcars)
dt
# По умолчанию (и принудительно, в том числе после invisible() ), возвращается 
# только последний объект, указанный в фигурных скобках без имени. 

dt[,{tmp1 = mean(mpg); tmp2 = mean(abs(mpg-tmp1)); tmp3 = round(tmp2, 2)}, by=cyl]
dt[,{tmp1 = mean(mpg); tmp2 = mean(abs(mpg-tmp1))}, by=cyl]
dt[, .(sd = {tmp1 = mean(mpg); tmp2 = mean(abs(mpg-tmp1)^2); tmp3 = round(tmp2, 2)}), by=cyl]
# В первую var загружается среднее зн. столбца, выполняются вычисления и результат округляется (до 2-го знака посде запятой).
# Последний результат - "отклонение от среднего каждого значения" выводится на экран.

## В нечисленных форматах данных необходимость подобного поэтапного преобразования часто встречается с датами и с очисткой данных. 

##  именованный список результата к выводу

dt[,{tmp1=mean(mpg); tmp2=mean(abs(mpg-tmp1)); tmp3=round(tmp2, 2); list(mean=tmp2, dev=tmp3)}, by=cyl]

# то же самое в формате записи без точек с запятой

dt[,{tmp1=mean(mpg)
     tmp2=mean(abs(mpg-tmp1))
     tmp3=round(tmp2, 2)
     list(mean=tmp2, dev=tmp3)},
     by=cyl]

######################################################################
######################################################################

### Arg format and FUN format

dates <- format(as.Date(mydata$timestamp,format="%Y-%m-%d %H:%M:%S"),format="%d-%m-%y")

# format аргумент in as.Date()информирует функцию о том, что представляет собой формат входящих данных,
# а formatаргумент in format()сообщает ему, каким должен быть исходящий формат.

######################################################################
######### Dates ######################################################
######################################################################

str(so$date)
as.Date(so$date) ### error
so$date[222]
# [1] " Пн янв 11, 2016 5:11 pm "
as.Date.character(so$date[222], "%a %b %d, %Y %I:%m %P") ### обратите внимание, что на отсутствие " " впереди рег.ки  опечатку %P  идет error
# [1] NA
as.Date.character(so$date[222], " %a %b %d, %Y %I:%m %p") ### а на опечатку %m (вместо %M) не ругается, но в strptime() обнуляет минуты 
# [1] "2016-11-11"
as.Date(so$date[222], " %a %b %d, %Y %R ") ### так же не влияет формат времени - "24". Время не используется
# [1] "2016-01-11"

strptime(so$date[222], " %a %b %d, %Y %I:%m %p ") ### %m обнуляет минуты 
# [1] "2016-11-11 17:00:00 -00"
strptime(so$date[222], " %a %b %d, %Y %I:%M %p ")  ### допиливаем минуты
# [1] "2016-01-11 17:11:00 -00"


so[, date2 := as.Date(date, " %a %b %d, %Y %I:%m %p ")]
so[1:13, date2]
#   [1] NA           "2016-11-28" NA           NA           NA           NA           NA           NA           NA           NA           NA          
#  [12] NA           NA           NA           "2016-11-29" NA           NA           NA           NA           NA           NA           NA          
#  [23] NA           NA           "2016-08-30" "2016-12-30" NA           NA         NA           NA           NA           NA           NA          
so[, date2 := as.Date(date, " %a %b %d, %Y %I:%M %p ")]
sum(is.na(so[['date2']]))
so[is.na(date2), .(date, date2)]

rm(so)
so <- copy(so_ind)
setnames(so, c("user", "usid", "postid", "date", "quoter", "text", "url", "id", ""))
so1 <- so[2000:2300]
so1
as.Date.character(so$date[3300:3600], " %a %b %d, %Y %I:%M %p ") ## error
as.Date.character(so$date[3350:3450], " %a %b %d, %Y %I:%M %p ")
so$date[3350:3450] ### ошибку выдают показатели дат, где проставлен месяц "май" 
#пробуем на отдельном значении:
as.Date.character(so$date[3360], " %a %b %d, %Y %I:%M %p ")

so$date[3360]
# [1] " Пн май 02, 2016 1:17 pm "  

as.Date.character(" Пн май 02, 2016 1:17 pm ", " %a %b %d, %Y %I:%M %p ")

### меняем "май" на "мая" - работает.
as.Date.character(" Пн мая 02, 2016 1:17 pm ", " %a %b %d, %Y %I:%M %p ")
# [1] "2016-05-02"

lubridate::dmy_hm(" Пн мая 02, 2016 1:17 pm ")


### возможные варианты приведение данных в читаемый вид просьба сбросить в чат

####################################################################
#################   POSIXct и POSIXlt  #############################
####################################################################

# POSIXct - это количество секунд с начала эпохи . В данном случае эпоха 1 января 1970 года.

# POSIXlt - это смешанный текстовый и символьный формат


# POSIXct проще обрабатывать, так как структура списка даты в POSIXlt может быть проблематичной
# и POSIXct занимает немного меньше памяти


datetime <- Sys.time()
datetime

## юлианские преобразования:
weekdays(datetime)

months(datetime)

quarters(datetime)

julian(datetime)

class(datetime)
## [1] "POSIXct" "POSIXt"



date1 <- as.POSIXct(1268736919, origin="1970-01-01", tz="GMT")
date2 <- as.POSIXlt(1268736913, origin="1970-01-01", tz="GMT")
date1; date2

# [1] "2010-03-16 10:55:19 GMT"
# [1] "2010-03-16 10:55:19 GMT"

# внутренняя разница между POSIXct и POSIXlt:
unclass(date1)
unclass(date2)

# вывод отдельных объектов POSIXlt:

date2$min

date2$year

# Для обоих объектов класса мы можем видеть часовой пояс, используя функцию attr .

attr(date1, "tzone")

attr(date2, "tzone")

# возможность преобразования в даты Char & Int данных. В числ. данных использование Arg format необязательно

date3 <- as.POSIXct("20100316105519", format="%Y%m%d%H%M%S", origin="1970-01-01", tz="GMT")
date3
# [1] "2010-03-16 10:55:19 GMT"

z <- 20100316105519
as.POSIXct(z, format="%Y%m%d%H%M%S", origin="1970-01-01", tz="GMT")
## [1] NA


##### Действия с  датами

several_dates <- seq.POSIXt(date2, by=1, length.out = 100)
# Arg: исходная дата, инкремент, длина
head(several_dates)

## Аргумент by может быть указан несколькими способами:
# Число, принятое за секунды.
# Объект класса difftime
# Строка символов, содержащая одно из следующих значений: «sec», «min», «hour», «day», «DSTday», «week», «month», «квартал» или «year». 
#При желании этому может предшествовать (положительное или отрицательное) целое число и пробел, или после него следует «s».


several_dates <- seq.POSIXt(date2, by="day", length.out = 100)
head(several_dates)
several_dates <- seq.POSIXt(date2, by="3 days", length.out = 100)
head(several_dates)

##  разрезать этот сезон на интервалы:
day_weeks<- cut.POSIXt(several_dates, "weeks")
class(day_weeks)
## [1] "factor"
levels(day_weeks)

## получить разницу между двумя датами
datetime - date1

## добавить время к объектам POSIXct и POSIXlt
date1 + 5000
date2 + 5000





####################################################################
#################   SET в итерациях ################################
####################################################################

### set на несколько порядков быстрее, чем присвоения из R-base

# set() !!!!!!!!!

M = matrix(1,nrow=100000,ncol=100)
DF = as.data.frame(M)
DT = as.data.table(M)
system.time(for (i in 1:1000) DF[i,1L] <- i)   # 591.000s ## видимо данные устарели, у меня работает быстрее. Допилили R-base
system.time(for (i in 1:1000) DT[i,V1:=i])     #   1.158s
system.time(for (i in 1:1000) M[i,1L] <- i)    #   0.016s
system.time(for (i in 1:1000) set(DT,i,1L,i))  #   0.027s

# Создатели data.table позиционируют применение set как "идеологически правильное действие правоверного человека" (юмор)
# вспоминаем, что set пишет непосредственно в участок памяти, без создания копии

dt <- data.table(mtcars)[,1:5, with=F]
for (j in c(1L,2L,4L)) set(dt, j=j, value=-dt[[j]]) # integers using 'L' passed for efficiency
for (j in c(3L,5L)) set(dt, j=j, value=paste0(dt[[j]],'!!'))
head(dt)

# dt <- data.table(mtcars)[,1:5, with=F]
# setkey(dt, cyl)
# dt
# 
# set(dt[8], j = 'disp', value = NA)
# dt
# 
# dt[8, disp := NA]
# dt


###Использование shift для вывода элементов и декремента
## (Img)
###  "Base R не имеет хороших инструментов 
# для работы с отведениями / лагами векторов, которые большинство стат. программ  (Stata, SAS) комплектуются из коробки."

dt <- data.table(mtcars)[,.(mpg, cyl)]
dt[,mpg_lag1:=shift(mpg, 1)]
dt[,mpg_forward1:=shift(mpg, 1, type='lead')]
head(dt)

dt[,lst:=list(shift(mpg, 1:3, type='lead'))]
dt

dt[,lst:=list(shift(mpg, 1:3, type='lead'), shift(mpg, 1:3))]
dt
str(dt)

dt <- data.table(id = 1:10, gr = LETTERS[1:10], key = 'id')

dt[, .SD[, lapply(shift)]]

### shift с участием by

n <- 30
dt <- data.table(
  date=rep(seq(as.Date('2010-01-01'), as.Date('2015-01-01'), by='year'), n/6), 
  ind=rpois(n, 5),
  entity=sort(rep(letters[1:5], n/5))
)
dt
setkey(dt, entity, date) # important for ordering
dt[,indpct_fast:=(ind/shift(ind, 1))-1, by=entity]
dt
lagpad <- function(x, k) c(rep(NA, k), x)[1:length(x)] 
dt[,indpct_slow:=(ind/lagpad(ind, 2))-1, by=entity]
head(dt, 10)


### Создание нескольких столбцов с :=одним оператором

dt <- data.table(mtcars)[,.(mpg, cyl)]
dt[,`:=`(avg=mean(mpg), med=median(mpg), min=min(mpg)), by=cyl]
head(dt)


### Назначить столбец с := именованным символом объекта
# Это рекомендуемый способ назначить новый столбец, имя которого вы уже определили и сохранили в переменную как символ.
# для этого  заключите объект в круглые скобки.

dt <- data.table(mtcars)[, .(cyl, mpg)]

thing2 <- 'mpgx2'
dt[,(thing2) := mpg * 2]


### старый, устаревший способ, который все еще работает на данный момент. Не рекомендуется:

thing3 <- 'mpgx3'
dt[,thing3:=mpg*3, with=F]

head(dt)

############
# для data.frame:

df <- data.frame(col1 = 1:3)
colname <- "col1"
df[colname] <- 4:6
df


##Два способа программного выбора переменных:

#with = FALSE:
DT = data.table(col1 = 1:3)
colname = "col1"
DT[, col1, with = FALSE] # Error
DT[, 'col1', with = FALSE]
DT[, (colname)] # Херня
DT[, colname] # Error
DT[, colname, with = FALSE]
#    col1
# 1:    1
# 2:    2
# 3:    3


#префикс «точка-точка»  ..( ):
  
  DT[, ..colname]    
#    col1
# 1:    1
# 2:    2
# 3:    3

  
  
####################################################################
#################   BY  ############################################
####################################################################

# Вычислить функцию по группе (используя by), исключая каждую сущность во второй категории
# То есть сравнить mpg каждый автомобиль со средним mpg количеством автомобилей в одном классе (одинаковое количество цилиндров). 
# Однако мы не хотим смещать среднее значение группы, включая автомобиль, который мы хотим сравнить со средним в этом среднем


### 0.a Смещенное среднее: простое среднее cyl
# Вычислить для каждого ряда, каково среднее значение для всех других автомобилей 
# с таким же количеством cylединиц, за исключением этой машины.

dt <- data.table(mtcars)[,.(cyl, gear, mpg)]
dt[, mpg_biased_mean := mean(mpg), by=cyl] 
dt

### 1.а .GRP без установочного ключа
# .GRP	в j , текущий индекс подмножества данных
# .BY	в j , список по значениям для текущего подмножества данных

dt[, dt[!gear %in% unique(dt$gear)[.GRP], mean(mpg), by=cyl], by=gear]

# Обновление 24.09.2015: согласно комментариям Мэтта Доула

dt[, dt[!gear %in% .BY[[1]], mean(mpg), by=cyl], by=gear]



### 1.b То же, что и 1.a, но немного быстрее

(uid <- unique(dt$gear))
dt[, dt[!gear %in% (uid[.GRP]), mean(mpg), by=cyl] , by=gear][order(cyl, gear)] 

dt[!gear %in% (uid[.GRP]), mean(mpg), by=cyl]

# принцип работы .GRP
dt[, .GRP, by=cyl]
dt[, .GRP, by=.(cyl, gear)]
dt[, .GRP, keyby=.(cyl, gear)]

dt[, .BY, keyby=.(cyl, gear)]

dt[, .(.GRP, unique(dt$gear)[.GRP]), by=cyl]

dt[,dt[, .(.GRP, unique(dt$gear)[.GRP]), by=cyl], by=gear]


### 1.b Установочный ключ

setkey(dt, gear)
uid <- unique(dt$gear)
dt[, dt[!.(uid[.GRP]), mean(mpg), by=cyl] , by=gear]

mean(dt[cyl==4 & gear!=3,mpg]) # testing

mean(dt[cyl==6 & gear!=3,mpg]) # testing

####################################################################
#################   [1], [.N], setkey и by  ########################
####################################################################
### .N	в i общее число строк

## Используя [1], [.N], setkey и by для группы в подменю

# Макс qsec для каждой категории cyl

dt <- data.table(mtcars)[, .(cyl, mpg, qsec)]
dt
dt[, max(qsec), by=cyl]


setkey(dt, mpg) ###  col "mpg" перетасовывается по key.  Это дает возможность:

# значение qsec когда mpg является самым высоким в категории cyl
dt[,qsec[.N],  by=cyl]

# значение qsec когда mpg является самым низким в категории cyl
dt[,qsec[1],  by=cyl]

# значение, qsec когда mpg медиана на категорию cyl
dt[,qsec[round(.N/2)],  by=cyl]


####################################################################
#################   ФУНКЦИИ  #######################################
####################################################################

### 1. Передача в data.table имен столбцов в качестве аргументов функции
# То есть когда надо передать произвольный столбец in FUN

dt <- data.table(mtcars)[,.(cyl, mpg)]
dt
myfunc <- function(dt, v) {
  v2=deparse(substitute(v))
  dt[,v2, with=F][[1]] # [[1]] returns a vector instead of a data.table
}

myfunc(dt, mpg)

### 2. цитаты и get
dt <- data.table(mtcars)
myfunc <- function(dt, v) dt[,get(v)]

myfunc(dt, 'mpg')

### область видимости в data.table
# Когда вы добавляете что-то data.frameв функцию, которая существует в глобальной среде, 
# это не влияет на этот объект в глобальной среде,
# если вы не вернете и не переназначите его как таковое или не воспользуетесь <<- оператором.

df <- mtcars[,c('cyl', 'mpg')]
add_column_df <- function(df) {
  df$addcol1<- 'here in func!'
  df$addcol2 <<- 'in glob env!'
  return(df)
}

# Когда мы вызываем функцию, мы видим addcol1в выводе. Но нет addcol2. Это потому, 
# что он был добавлен df в глобальную среду на один уровень выше.

head(add_column_df(df))

# Здесь addcol2, но нет addcol.

head(df)

#########
### В отличие от data.frame, :=оператор добавляет столбец как к объекту, 
# живущему в глобальной среде, так и используемому в функции.

dt <- data.table(mtcars)
add_column_dt <- function(dat) {
  dat[,addcol:='sticking_to_dt!'] # hits dt in glob env
  return(dat)
}
head(add_column_dt(dt)) # addcol here

head(dt)# addcol also here

### другой способ: сохранить некоторые переменные локальными для функции, сохраняя и возвращая другие столбцы.

dt <- data.table(mtcars)
add_column_dt <- function(dat) {
  datloc <- copy(dat)
  datloc[,addcol:='not sticking_to_dt!'] # hits dt in glob env
  return(datloc)
}
head(add_column_dt(dt)) # addcol here


############################################################

# []  в конце выражения - выводит его результат на консоль
df <- head(mtcars) # doesn't print
(df <- head(mtcars)) # does print

dt <- data.table(head(mtcars)) # doesn't print
dt[,hp2wt:=hp/wt][] # does print
