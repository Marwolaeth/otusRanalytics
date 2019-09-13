if(!require('pacman')) install.packages('pacman')
pacman::p_load(microbenchmark, readxl, data.table, R.utils, feather, fst)


Sys.getenv("HOME")

f <- readLines('hw_2_1-52376-b62121.Rmd')
head(f, 10)
writeLines(f)
writeLines(f, 'hw_2_1.txt')

deps <- read_excel('rkn_deps.xlsx')
head(deps, 10)

# vim
# tmux
<<<<<<< HEAD

setDTthreads(12)
getDTthreads()

oc <- fread("http://opencorpora.org/files/export/dict/dict.opcorpora.txt.zip")

download.file(
  "http://opencorpora.org/files/export/dict/dict.opcorpora.txt.zip",
  'dict.corpora.txt.zip'
)
unzip("dict.corpora.txt.zip")
oc <- fread(
  'dict.opcorpora.txt'
)
dict <- readLines('dict.opcorpora.txt')
head(dict, 30)
=======
# sed
# awk

setDTthreads(3)
getDTthreads()

oc <- fread('http://opencorpora.org/files/export/dict/dict.opcorpora.txt.zip')
download.file(
  'http://opencorpora.org/files/export/dict/dict.opcorpora.txt.zip',
  'dict.opcorpora.txt.zip'
)
unzip('dict.opcorpora.txt.zip')
rl <- readLines('dict.opcorpora.txt')
head(rl, 30)

oc <- fread(
  'grep -v ^[0-9] dict.opcorpora.txt',
  encoding = 'UTF-8',
  header = FALSE,
  sep = '\t',
  sep2 = ',',
  nrows = 111,
  blank.lines.skip = TRUE
)
head(oc, 30)
str(oc)

# oc3 <- fread(
#   "dict.opcorpora.txt",
#   encoding = 'UTF-8',
#   sep = "\t",
#   sep2 = ",",
#   nrows = 70,
#   fill=TRUE,
#   blank.lines.skip = T
# )
# head(oc3, 30)

oc3 <- fread(
  'grep -v ^[0-9] dict.opcorpora.txt',
  encoding = 'UTF-8',
  header = FALSE,
  sep = '\t',
  sep2 = ',',
  blank.lines.skip = TRUE
)
head(oc3, 30)
str(oc3)


if (!dir.exists('corpora')) dir.create('corpora')
ids <- (nrow(oc3) - 29):nrow(oc3)
for (i in ids) {
  fname <- paste0(paste('corpora/dict.opcorpora', i, sep = '_'), '.fst')
  fst::write_fst(oc3[i, ], fname)
}
# >>>>>>> acc277c2252faf5f69c57fcdabac54d220eaccc6

if(!require(pacman)) install.packages('pacman')
pacman::p_load(microbenchmark, readxl, data.table, R.utils, feather, fst)

setDTthreads(3)
getDTthreads()

download.file(
  'http://opencorpora.org/files/export/dict/dict.opcorpora.txt.zip',
  destfile = 'dict.opcorpora.txt.zip'
)
unzip('dict.opcorpora.txt.zip')
oc3 <- fread(
  'grep -v ^[0-9] dict.opcorpora.txt',
  encoding = 'UTF-8',
  header = FALSE,
  sep = '\t',
  sep2 = ',',
  blank.lines.skip = TRUE
)
head(oc3, 30)
str(oc3)

writeLines.dict <- function() {
  # Если не преобразовать в текст
  # выдает ошибку: can only write character objects 
  writeLines(
    apply(oc3, 1, paste, collapse = ' '),
    'dict.opcorpora_wl.txt')
}

write.csv.dict <- function() {
  write.csv(oc3, 'dict.opcorpora.csv')
}

fwrite_2t.dict <- function() {
  # fwrite() using 2 threads
  data.table::fwrite(oc3, 'dict.opcorpora_fw.csv', nThread = 2)
}

fwrite_3t.dict <- function() {
  # fwrite() using 3 threads
  data.table::fwrite(oc3, 'dict.opcorpora_fw.csv', nThread = 3)
}

write_feather.dict <- function() {
  feather::write_feather(oc3, 'dict.opcorpora.feather')
}

write_fst.dict <- function() {
  fst::write_fst(oc3, 'dict.opcorpora.fst')
}

bm_write_dict <- microbenchmark(
  writeLines.dict(),
  write.csv.dict(),
  fwrite_2t.dict(),
  fwrite_3t.dict(),
  write_feather.dict(),
  write_fst.dict(),
  times = 3L
)
bm_write_dict
str(bm_write_dict)
?summary.microbenchmark
summary(bm_write_dict, unit = 's')

bm_write_dict_summary <- data.table(
  summary(bm_write_dict, unit = 's'),
  key = 'expr'
)
data.table::setorderv(
  bm_write_dict_summary,
  cols = 'median',
  order = 1L
)
bm_write_dict_summary
