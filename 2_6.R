if(!require('pacman')) install.packages('pacman')
pacman::p_load(httr, magrittr, Quandl, fredr, quantmod, vkR, igraph, tidyverse, data.table)

#### Quandl ####
srch <- Quandl.search("Moody's AAA")
AAA_xts <- Quandl('FRBP/SPR_BAA_AAA_MN', type = 'xts')

#### quantmod ####
getSymbols('GBP/CAD', src = 'oanda')
GBPCAD

getSymbols(c('MSFT', 'AAPL'))
getSymbols(c('MSFT', 'AAPL'), verbose = TRUE)
AAPL

#### VK API ####
.VK_OAUTH <- 5573604
.VK_OAUTH_token <- '322cecbd322cecbd322cecbd093279e7593322c322cecbd6fbad7254647b9c7f9b5a6c2'

vkOAuth(.VK_OAUTH)
.VK_TOKEN <- '15d39b63b53116fef4302c38776c70e5e2ecb5e83e85ae08fbb993bd77cdc0a157ed4a6260121962f9605'
setAccessToken(.VK_TOKEN)

frnds <- getFriends(fields = 'sex,bdate')
str(frnds, 1)
head(frnds$items, 11)
frnds_df <- frnds$items

frnds_df <- frnds_df %>%
  filter(is.na(deactivated)) %>%
  mutate(name = paste(first_name, last_name)) %>%
  select(id, name)
friends_net <- map(frnds_df$id, ~data.table(id1 = .x, id2 = getMutual(target_uid = .x)))

friends_net_df <- do.call(rbind, friends_net)
head(friends_net_df, 13)
str(friends_net_df)
friends_net_df <- filter(friends_net_df, map_int(id2, length) > 0) %>% mutate(id2 = unlist(id2))
# friends_net_df <- friends_net_df[map_dbl(friends_net_df[, id2], length) == 1, .(id1 = as.numeric(id1), id2 = as.numeric(id2))]

friends_net_df <- friends_net_df %>%
  right_join(frnds_df, by = c('id1' = 'id')) %>%
  right_join(frnds_df, by = c('id2' = 'id')) %>%
  select(id1 = name.x, id2 = name.y) %>%
  filter(complete.cases(.))

g <- graph_from_data_frame(
  friends_net_df,
  directed = FALSE,
  vertices = friends_net_df %>%
    distinct(id1) %>%
    right_join(frnds_df, by = c('id1' = 'name')) %>%
    select(id1)
)

g <- igraph::simplify(g)
plot(g)
