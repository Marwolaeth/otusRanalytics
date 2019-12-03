if(!require('R6')) install.packages('R6')
library(R6)

transliteration_dictionary <- R6Class(
  classname = 'transliteration_dictionary',
  public = list(
    initialize = function(
      latin_uppercase,
      latin_lowercase,
      russian_uppercase = NULL,
      russian_lowercase = NULL
    ) {
      self$latin_uppercase <- latin_uppercase
      self$latin_lowercase <- latin_lowercase
      if (!is.null(russian_uppercase)) {
        self$russian_uppercase <- russian_uppercase 
      }
      if (!is.null(russian_lowercase)) {
        self$russian_lowercase <- russian_lowercase
      }
    },
    latin_uppercase = NULL,
    latin_lowercase = NULL,
    russian_uppercase = c(
      'А','Б','В','Г','Д','Е','Ё','Ж','З','И','Й','К','Л','М','Н',
      'О','П','Р','С','Т','У','Ф','Х','Ц','Ч','Ш','Щ','Ъ','Ы','Ь',
      'Э','Ю','Я'
    ),
    russian_lowercase = c(
      'а','б','в','г','д','е','ё','ж','з','и','й','к','л','м','н','о',
      'п','р','с','т','у','ф','х','ц','ч','ш','щ','ъ','ы','ь',
      'э','ю','я')
  )
)

translit_casual <- transliteration_dictionary$new(
  latin_uppercase = c(
    "A","B","V","G","D","E","YO","ZH","Z","I","J","K",
    "L","M","N","O","P","R","S","T","U","F","KH","C","CH",
    "SH","SHH","''","Y","'","E'","YU","YA"
  ),
  latin_lowercase = c(
    "a","b","v","g","d","e","yo","zh","z","i","j","k","l","m","n",
    "o","p","r","s","t","u","f","kh","c","ch","sh","shh","''","y",
    "'","e'","yu","ya"
  )
)
translit_casual
class(translit_casual)

translit_academic <- transliteration_dictionary$new(
  # Урощенный вариант правил транслитерации Института языкознания АН СССР
  latin_lowercase = c(
    'ji','a','b','v','g','d','je','jo','ž','z','i','j','k','l','m','n',
    'o','p','r','s','t','u','f','ch','c','č','š','šč','’','’','e','ju','ja'
  ),
  latin_uppercase = c(
    'JI','A','B','V','G','D','JE','JO','Ž','Z','I','J','K','L','M','N',
    'O','P','R','S','T','U','F','CH','C','Č','Š','ŠČ','’','’','E','JU','JA'
  ),
  russian_uppercase = c(
    'ЬИ','А','Б','В','Г','Д','Е','Ё','Ж','З','И','Й','К','Л','М','Н',
    'О','П','Р','С','Т','У','Ф','Х','Ц','Ч','Ш','Щ','Ъ','Ы','Ь',
    'Э','Ю','Я'
  ),
  russian_lowercase = c(
    'ьи', 'а','б','в','г','д','е','ё','ж','з','и','й','к','л','м','н','о',
    'п','р','с','т','у','ф','х','ц','ч','ш','щ','ъ','ы','ь',
    'э','ю','я'
  )
)
translit_academic
class(translit_academic)

businesscard <- R6Class(
  classname = 'businesscard',
  public = list(
    initialize = function(
      firstname, lastname, occupation, company = NULL,
      phone, cellphone = NULL, fax = NULL, email
    ) {
      self$firstname  = firstname
      self$lastname   = lastname
      self$occupation = occupation
      self$company    = company
      self$phone      = phone
      self$cellphone  = cellphone
      self$fax        = fax
      self$email      = email
    }
  ),
  private = list(
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
  lock_objects = FALSE
)

bc_test2 <- businesscard$new(
  firstname  = 'Вадим',
  lastname   = 'Бестужев',
  occupation = 'Менеджер по работе с ключевыми клиентами',
  company    = 'Вектор',
  phone      = '8 (87240) 2-09-33',
  cellphone  = '+7 969 666-11-13',
  email      = 'VBestuzhev@vector.io'
)
bc_test2
str(bc_test2)
class(bc_test2)
bc_test2$cellphone
bc_test2[[2]]
print(bc_test2)
