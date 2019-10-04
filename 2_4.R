if(!require('pacman')) install.packages('pacman')
pacman::p_load(httr, rvest, jsonlite, xml2, tidyverse, devtools, getProxy)
devtools::install_github('dantonnoriega/xmltools')
denullify <- function(lst, fill = NA) {
  lapply(lst, function(l) lapply(l, function(x) ifelse(is.null(x), fill, x)))
}

cities <- c('Дербент', 'Великий Новгород', 'Псков', 'Полоцк', 'Козельск')
base_url <- 'https://htmlweb.ru/geo/api.php?json'
.htmlweb_API_KEY <- '211ecf285f865e3b842ddfdc25296c87'
# city <- character()
# id <- integer()

result <- GET(
  base_url,
  query = list(
    city_name = cities[5],
    api_key = .htmlweb_API_KEY,
    charset = 'utf-8'
  )
)
result
content(result)
res <- fromJSON(content(result))
res$limit <- NULL
do.call(rbind, lapply(denullify(res), as.data.frame, stringsAsFactors = FALSE))

# base_url <- 'https://htmlweb.ru/geo/api.php?xml'
# res <- content(result)
# res[length(res)] <- NULL
# res <- xmltools::xml_dig_df(res)

cities_df <- data.frame()
for (city in cities) {
  result <- GET(
    base_url,
    query = list(
      city_name = city,
      api_key = .htmlweb_API_KEY,
      charset = 'utf-8'
    )
  )
  city_df <- content(result)
  city_df$limit <- NULL
  city_df <- do.call(
    rbind,
    lapply(denullify(city_df), as.data.frame, stringsAsFactors = FALSE)
  )
  cities_df <- rbind(cities_df, city_df[1, c('id', 'name', 'country')])
}
cities_df

getProxy(country = 'DE', supportsHttps = TRUE, type = 'socks5', action = 'get')

get_cities_distance <- function(id1, id2, sleep = 1) {
  proxy_countries <- c('RU', 'CA', 'DE', 'FR', 'US', 'NL')
  proxy <- getProxy(
    country = sample(proxy_countries, 1),
    supportsHttps = TRUE,
    action = 'get'
  )
  proxy <- unlist(strsplit(proxy, split = ':'))
  Sys.sleep(sleep)
  result <- GET(
    base_url,
    query = list(
      city1 = id1,
      city2 = id2,
      api_key = .htmlweb_API_KEY,
      charset = 'utf-8'
    ),
    use_proxy(proxy[1], as.numeric(proxy[2]))
  )
  return(content(result)$distance)
}

distances_df <- expand.grid(id1 = cities_df$id, id2 = cities_df$id)
distances_df <- distances_df[!duplicated(t(apply(distances_df, 1, sort))), ]
distances_df$distance <- ifelse(
  distances_df$id1 == distances_df$id2,
  0,
  get_cities_distance(distances_df$id1, distances_df$id2)
)