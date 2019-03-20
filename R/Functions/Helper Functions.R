#### Common Functions ####


library(tidyverse)

#### function to benchmark time with message
benchmark <- function(start_time, message_str){
  writeLines(str_glue(message_str,'\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),0)} secs'))
}

# sample use
# benchmark(start_time = start_time, 'here is my message')
