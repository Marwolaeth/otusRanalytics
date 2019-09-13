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
>>>>>>> acc277c2252faf5f69c57fcdabac54d220eaccc6
