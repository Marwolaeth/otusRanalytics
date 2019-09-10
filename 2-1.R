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