if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse)

(x <- c('X1', 'X2', 'Y1', 'Y2'))
str_replace(x, 'Y', 'Z')
str_replace_all(x, 'Y', 'Z')

?p_load

# отлично!
pacman::p_load(tidytext, tm, udpipe, update = TRUE)

x <- 1:10
y <- 4:13
plot(x, y)
plot(x, y, type = 's')
plot(x, y, type = 'S')
