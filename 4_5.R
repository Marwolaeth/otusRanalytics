if(!require('pacman')) install.packages('pacman')
pacman::p_load(shiny, rsconnect, UScensus2010)

getwd()
runApp('apps/helloshiny')
runApp('apps/so')
runApp('apps/second')
