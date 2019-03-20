# Compile Team History Data, and get distinct list of INC with 1st/2nd/3rd/4th/5th teams
# Using Incident Team History extracts

glue::glue('here is my string with the start time variable: {start_time}')
str_glue('here is string {start_time}')

# conditionally install packages if not already installed, then load
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

# benchmarking variables
start_time <- Sys.time()
writeLines(str_glue('Starting: {start_time}'))

# identify all appropriately named files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
team_history_files <- list.files(path, "(?i)inc_team_history_", full.names = TRUE)

# import and merge team [assignment] history
team_history <- data_frame()
writeLines(str_glue('Importing & merging team history.\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
for (i in 1:length(team_history_files)){
  data <- readxl::read_excel(team_history_files[i])
  team_history <- bind_rows(team_history, data)
}

# keep distinct rows, filter out duplicates (may be due to misaligned export filters)
writeLines(str_glue('Removing duplicate records.\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
team_history <- team_history %>% select(Number, Field, Value, Start, End) %>% distinct()  # keep distinct records
filter_out <- team_history %>% filter(Start == End)  # filter out duplicates
team_history <- team_history %>% anti_join(filter_out)  # safest way to filter out (avoids null/empty errors)

# get all distinct incidents
writeLines(str_glue('Compiling distinct incidents.\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
distinct_incidents <- team_history %>% select(Number) %>% distinct()

#TODO: get 1st/2nd/etc tickets



# output team history
writeLines(str_glue('Exporting full team history now at {Sys.time()}.\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
writexl::write_xlsx(x = team_history, path = paste0(path, "\\All Team History.xlsx"))  # switch to this
writeLines(str_glue('\nDONE. \nStart time: {start_time}.\nEnd time: {Sys.time()}.\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
