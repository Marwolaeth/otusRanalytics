if(!require('pacman')) install.packages('pacman')
pacman::p_load(RSQLite, data.table)

con <- dbConnect(RSQLite::SQLite())
if (!con) {
  print('Ошибка при подключении: ')
  dbGetException(con)
} 
dbDisconnect(con)
# cat(URLnext, file = "log.txt", fill = T, append = T)
# print(URLnext)

# print("###################   NEXT TOPIC   ######################")
# print(i)

so <- fread('data/so.csv', encoding = 'UTF-8')
str(so)

con <- dbConnect(RSQLite::SQLite())
dbWriteTable(con, 'so', so, row.names = FALSE)
dbListTables(con)
dbGetQuery(con, 'SELECT COUNT(*) FROM so;')
names(so)
dbListFields(con, 'so')

v1 <- dbSendQuery(con, 'SELECT V1, V5 FROM so;')
qtrs <- dbFetch(v1, n = 200)

qtrs <- qtrs[qtrs$V5 != '', ]
head(qtrs, 13)

if (!require(statnet)) install.packages('statnet')
library(statnet)

net_so <- network(qtrs, matrix.type = "edgelist")
class(net_so)
dev.new()
pdf(file = 'qr_net.pdf')
net_plot <- gplot(net_so, vertex.col = 2, displaylabels = T)
dev.off()

qr <- dbGetQuery(
  con,
  'SELECT V1, V5, COUNT(*) FROM so WHERE V5 IS NOT NULL GROUP BY V1, V5;'
)
head(qr)
