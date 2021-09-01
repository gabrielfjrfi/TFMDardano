library(reticulate)

source_python("TextoEnPython.py")

flights <- detector_idioma("Benvolgut company")


cld2::detect_language_mixed("compañero")

library(tidytext)
library(tidyverse)
library(textdata)
get_sentiments("nrc")

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

