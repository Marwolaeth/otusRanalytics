summary.vehicle <- function(x) {
  veh_summary <- list(
    brand = x$brand,
    weight = x$weight,
    type = x$type,
    passengers = x$passengers,
    max_passengers = x$max_passengers,
    fullness = x$passengers/x$max_passengers
  )
  class(veh_summary) <- "veh_summary"
  return(veh_summary)
}

print.veh_summary <- function(x) {
  
  out <- paste0("+==================================+\n",
                "> Car brand: ", x$brand, "\n",
                "> Car weight: ", x$weight, "\n",
                "> Car type: ", x$type, "\n",
                "> Passengers: ", x$passengers, "/", x$max_passengers, "\n",
                "> The car is ", round(x$fullness, 2), "% full\n",
                "+==================================+")
  cat(out)
  return(invisible(x))
}

is(1:11)
otype(1:11)


#### R6 ####

# пример - класс бухгалтерской проводки
# ! данная запись - по сути, конструктор
# новые объекты можно будет создать через account_entry$new()
# но можно написать конструктор и по-своему
if(!require('R6')) install.packages('R6')
library(R6)
account_entry <- R6Class("account_entry",
                         public = list(
                           name = NULL,
                           shop = NULL,
                           comment = NULL,
                           initialize = function(x) {}
                           # print = function(name, ...) {
                           #   ...
                           #   return(...)
                           # }
                         ),
                         private = list(
                           amount = NA,
                           currency = NA
                         )
)

aug22 <- account_entry$new(
  # name = 'Aug22',
  # shop = 'Biedronka',
  # comment = 'fajnie',
  # amount = 111,
  # currency = 'PLN'
)
aug22
is(account_entry)
is(aug22)

create_businesscard <- function(
  firstname, lastname, occupation, company = NULL,
  phone, cellphone = NULL, fax = NULL, email
) {
  return(
    structure(
      list(
        firstname  = firstname,
        lastname   = lastname,
        occupation = occupation,
        company    = company,
        phone      = phone,
        cellphone  = cellphone,
        fax        = fax,
        email      = email,
        fieldnames_ru  =  c(
          firstname  = "ИМЯ",
          lastname   = "ФАМИЛИЯ",
          occupation = "ДОЛЖНОСТЬ",
          company    = "ОРГАНИЗАЦИЯ",
          phone      = "ТЕЛ.",
          cellphone  = "МОБ.",
          fax        = "ФАКС",
          email      = "email"
        )
      ),
      class = 'businesscard'
    )
  )
}

bc_test <- create_businesscard(
  firstname  = 'Вадим',
  lastname   = 'Бестужев',
  occupation = 'Менеджер по работе с ключевыми клиентами',
  company    = 'Вектор',
  phone      = '8 (87240) 2-09-33',
  cellphone  = '+7 969 666-11-13',
  email      = 'VBestuzhev@vector.io'
)
str(bc_test)

print.businesscard <- function(bc, uppercase = NULL, fieldnames = NULL) {
  if (length(uppercase)) bc[uppercase] <- lapply(bc[uppercase], toupper)
  if (length(fieldnames)) {
    for (i in fieldnames) {
      bc[[i]] <- paste(bc$fieldnames_ru[[i]], bc[[i]], sep = ': ')
    }
  }
  padding <- sapply(bc[1:8], function(x) (48 - nchar(x)) %/% 2)
  output <- paste0("+==============================================+\n",
                if (length(bc$company)) {
                  paste0(
                    bc$company,                                 "\n",
                    paste(rep(' ', 48), collapse = ''),         "\n",
                    paste(rep(' ', 48), collapse = ''),         "\n"
                  )
                },
                paste(rep(' ', padding[['firstname']]), collapse = ''),
                bc$firstname,
                paste(rep(' ', padding[['firstname']]), collapse = ''),
                                                                "\n",
                paste(rep(' ', padding[['lastname']]), collapse = ''),
                bc$lastname,
                paste(rep(' ', padding[['lastname']]), collapse = ''),
                                                                "\n",
                paste(rep(' ', 48), collapse = ''),             "\n",
                paste(rep(' ', padding[['occupation']]), collapse = ''),
                bc$occupation,
                paste(rep(' ', padding[['occupation']]), collapse = ''),
                                                                "\n",
                "                                                \n",
                "                                                \n",
                paste(rep(' ', padding[['phone']] * 2), collapse = ''),
                bc$phone,                                       "\n",
                if (length(bc$cellphone)) {
                  paste0(
                    paste(rep(' ', padding[['cellphone']] * 2), collapse = ''),
                    bc$cellphone,                               "\n"
                  )
                },
                if (length(bc$fax)) {
                  paste0(
                    paste(rep(' ', padding[['fax']] * 2), collapse = ''),
                    bc$fax,                                     "\n"
                  )
                },
                paste(rep(' ', padding[['email']] * 2), collapse = ''),
                bc$email,                                       "\n",
                "+==============================================+\n")
  cat(output)
  return(invisible(bc))
}

print(bc_test)
print(
  bc_test,
  uppercase = c('firstname', 'lastname'),
  fieldnames = c('phone', 'cellphone', 'email')
)


translit <- function(text) {
  lat.up <- c("A","B","V","G","D","E","YO","ZH","Z","I","J","K",
              "L","M","N","O","P","R","S","T","U","F","KH","C","CH",
              "SH","SHH","''","Y","'","E'","YU","YA")
  rus.up <- c("А","Б","В","Г","Д","Е","Ё","Ж","З","И","Й","К","Л","М","Н",
              "О","П","Р","С","Т","У","Ф","Х","Ц","Ч","Ш","Щ","Ъ","Ы","Ь",
              "Э","Ю","Я")
  lat.low <- c("a","b","v","g","d","e","yo","zh","z","i","j","k","l","m","n",
               "o","p","r","s","t","u","f","kh","c","ch","sh","shh","''","y",
               "'","e'","yu","ya")
  rus.low <- c("а","б","в","г","д","е","ё","ж","з","и","й","к","л","м","н","о",
               "п","р","с","т","у","ф","х","ц","ч","ш","щ","ъ","ы","ь",
               "э","ю","я")
  # основной код
}

text <- c('Информация к размышлению','Секретное донесение','По щучьему велению')
