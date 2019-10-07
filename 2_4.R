if(!require('pacman')) install.packages('pacman')
pacman::p_load(httr, rvest, jsonlite, tidyverse)

denullify <- function(lst, fill = NA) {
  # Обращает пустые вхождения внутри списка в NA
  # Для того, чтобы можно было объединить элементы списка в датафрейм
  lapply(lst, function(l) lapply(l, function(x) ifelse(is.null(x), fill, x)))
}

cities <- c('Дербент', 'Великий Новгород', 'Псков', 'Полоцк', 'Козельск')
.htmlweb_API_KEY <- '211ecf285f865e3b842ddfdc25296c87'
# city <- character()
# id <- integer()

# result <- GET(
#   base_url,
#   query = list(
#     city_name = cities[5],
#     api_key = .htmlweb_API_KEY,
#     charset = 'utf-8'
#   )
# )
# result
# content(result)
# res <- fromJSON(content(result))
# res$limit <- NULL
# do.call(rbind, lapply(denullify(res), as.data.frame, stringsAsFactors = FALSE))

# base_url <- 'https://htmlweb.ru/geo/api.php?xml'
# res <- content(result)
# res[length(res)] <- NULL
# res <- xmltools::xml_dig_df(res)

get_cities <- function(cities, api_key = NULL, sleep = 0) {
  cities_df <- data.frame()
  base_url <- 'https://htmlweb.ru/geo/api.php?json'
  for (city in cities) {
    # Sys.sleep(sleep)
    result <- GET(
      base_url,
      query = list(
        city_name = city,
        # api_key = api_key,
        charset = 'utf-8'
      )
    )
    city_df <- content(result)
    city_df <- city_df %>%
      .[[1]] %>%          # Забираем первый, самое релевантный результат поиска
      .[map_lgl(., ~ length(.) > 0)] %>%
      as_tibble() %>%
      select(id, name, country)
    cities_df <- rbind(cities_df, city_df)
  }
  return(cities_df)
}

cities_df <- get_cities(cities, api_key = .htmlweb_API_KEY)

# getProxy(country = 'DE', supportsHttps = TRUE, type = 'socks5', action = 'get')

get_cities_distance <- function(id1, id2, api_key = NULL, sleep = 0) {
  # proxy_countries <- c('RU', 'CA', 'DE', 'FR', 'US', 'NL')
  # while (!exists('proxy')) {
  #   try(
  #     proxy <- getProxy(
  #       country = sample(proxy_countries, 1),
  #       supportsHttps = TRUE,
  #       action = 'get'
  #     )
  #   )
  # }
  # proxy <- unlist(strsplit(proxy, split = ':'))
  if (length(id1) != 1 | length(id2) != 1) {
    stop('Argements id1 and id2 must be numeric (or coercible to one) vectors of length 0')
  }
  base_url <- 'https://htmlweb.ru/geo/api.php?json'
  Sys.sleep(sleep)
  result <- GET(
    base_url,
    query = list(
      city1 = id1,
      city2 = id2,
      api_key = api_key,
      charset = 'utf-8'
    )
    # use_proxy(proxy[1], as.numeric(proxy[2]))
  )
  return(content(result)$distance)
}
get_cities_distance(647, 647)

distances_df <- expand.grid(id1 = cities_df$id, id2 = cities_df$id)
# Удаляем повторяющиеся пары городов
# Сортируя каждую пару по алфавиту и убирая повторяющиеся результаты сортировки
# distances_df <- distances_df[!duplicated(t(apply(distances_df, 1, sort))), ]
# Добавляем расстояние
# Не знаю, как обойтись без dplyr (((
# distances_df <- distances_df %>%
#   mutate(
#     distance = if_else(
#       id1 == id2,
#       0,
#       get_cities_distance(
#         id1,
#         id2,
#         api_key = .htmlweb_API_KEY
#       )
#     )
#   )

# Добавляем расстояние
# Не знаю, как обойтись без dplyr ((( (зато можно без purrr, но зачем?)
distances_df <- distances_df %>%
  mutate(
    distance = map2_dbl( # На вход должен идти вектор длины 1
      distances_df$id1,
      distances_df$id2,
      ~ if (.x == .y) {
        0
      } else {
        get_cities_distance(.x, .y, api_key = .htmlweb_API_KEY)
      }
    )
  )
distances_df <- distances_df %>%
  left_join(cities_df, by = c('id1' = 'id')) %>%
  rename(city1 = name) %>%
  left_join(cities_df, by = c('id2' = 'id')) %>%
  rename(city2 = name) %>%
  select(-starts_with('country'))
# Вот в принципе таблица расстояний, но...
# Вот классическая матрица расстояний
distances_matrix <- distances_df %>%
  select(-starts_with('id')) %>%
  bind_rows(rename(., city2 = city1, city1 = city2)) %>%
  filter(city1 != city2) %>%
  spread(key = city2, value = distance, fill = 0) %>%
  column_to_rownames('city1') %>%
  as.matrix()

# Временный фикс из-за недоступности API kody.su (из примера на сайте):
JSON <- toJSON(
  str_remove_all(
    '{
    "success": true,
    "query": "79040000000",
    "quota": 91,
    "numbers": [
    {
    "number_current": ["79040000000"],
    "success": [true],
    "number_type": [1],
    "def": ["904"],
    "number": ["0000000"],
    "code_start": ["0000000"],
    "code_end": ["0299999"],
    "operator": ["Tele2"],
    "operator_full": ["ЗАО Смоленская Сотовая Связь Тверь"],
    "region": ["Тверская область"],
    "time": ["3.0"],
    "bdpn": [false],
    "bdpn_operator": [""]
    }
    ]
    }',
'\\n'
  ) %>% str_replace_all('\\s+', ' ')
)