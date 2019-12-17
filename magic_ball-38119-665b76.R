Sys.setlocale("LC_CTYPE", "russian")
encoding = "utf-8"
magic_ball <- function() {
  readline(prompt = "Ну-ка спроси: ")
  answers <- c("Да стопудово",
               "Даже и не пробуй",
               "Да",
               "Лучше попей чаю",
               "Один бог знает",
               "Красный цвет!")
  
  sample(answers, size = 1)
}
magic_ball()