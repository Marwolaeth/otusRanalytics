if(!require('pacman')) install.packages('pacman')
pacman::p_load(data.table, microbenchmark)

set.seed(45L)
DT = data.table(x=sample(1e4, 5e6, TRUE), y=sample(letters, 5e6, TRUE))
DT3 = data.table(x=sample(1e4, 5e7, TRUE), y=sample(letters, 5e7, TRUE))

options(datatable.auto.index = FALSE)
microbenchmark(
  DT[DT$x %in% 4:10, ],
  DT[x %in% 4:10, ] ,
  DT[x %between% c(4L, 10L), ] ,
  1
)

DT2 <- copy(DT)
system.time(setkey(DT3, x))

microbenchmark(
  DT3[x %between% c(4L, 10L), ] ,
  DT3[.(4:10)] ,
  3,
  unit = 's'
)
gc()

oc0 <- fread(
  'grep -v ^[0-9] dict.opcorpora.txt',
  encoding = 'UTF-8',
  header = FALSE,
  sep = '\t',
  sep2 = ',',
  blank.lines.skip = TRUE
)
# head(oc, 10)

oc[1, 'V2']
oc[1, V2]

oc <- copy(oc0)
oc2 <- copy(oc0)
setkey(oc, V1)
setindex(oc2, V1)

microbenchmark(
  oc0[V1 == 'БОГДАНУ', V2],
  oc[V1  == 'БОГДАНУ', V2],
  oc2[V1 == 'БОГДАНУ', V2],
  times = 3L
)

options(datatable.auto.index = TRUE)

(pos <- oc[V1  == 'БОГДАНУ', V2])
(pos <- pos[1])
gsub('(^[A-Z]{4}).+', '\\1', pos)
sapply(strsplit(pos, split = ','), '[', 1)
strsplit(pos, split = ',')[[1]][1]

microbenchmark(
  gsub('(^[A-Z]{4}).+', '\\1', pos),
  sapply(strsplit(pos, split = ','), '[', 1),
  strsplit(pos, split = ',')[[1]][1],
  times = 500L
)

oc3 <- oc[, V2 := gsub('(^[A-Z]+?\\,).+', '\\1', V2)]
oc3
oc0[, V2 := gsub('(^[A-Z]+?)\\,.+', '\\1', V2)]
oc0
