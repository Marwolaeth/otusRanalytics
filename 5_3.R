if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  reticulate,
  purrr,
  magick,
  cowplot
)
pacman::p_load_gh('rstudio/keras')
options(verbose = TRUE)

tensorflow::install_tensorflow()
install_keras()

use_python("/home/df/anaconda3/envs/tensorflow_env/bin/python3.7")
