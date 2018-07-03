library(tidyverse); library(lubridate)
start_time <- Sys.time()
print(paste("Starting:", start_time))

# import all appropriately named files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
state_history_files <- list.files(path, "(?i)state_hist", full.names = TRUE)
team_history_files <- list.files(path, "(?i)team_hist", full.names = TRUE)

# merge state history
state_history <- data_frame()
for (i in 1:length(state_history_files)){
  data <- readxl::read_excel(state_history_files[i])
  state_history <- bind_rows(state_history, data)
}
state_history <- state_history %>% distinct()

# merge team assignment history
team_history <- data_frame()
for (i in 1:length(team_history_files)){
  data <- readxl::read_excel(team_history_files[i])
  team_history <- bind_rows(team_history, data)
}
team_history <- team_history %>% distinct()

# set TZ of imported times to CST
state_history[c('Start','End')] <- force_tz(state_history[c('Start','End')], tzone = 'US/Central')
team_history[c('Start','End')] <- force_tz(team_history[c('Start','End')], tzone = 'US/Central')

# calendar table; datetime @ 8am
calendar_start <- date("2018-02-22")
calendar <- data_frame(
  date = seq.Date(from = calendar_start, to = today(), by = "days"),
  datetime = seq.POSIXt(from = as.POSIXct(paste(calendar_start, "08"), format = "%Y-%m-%d %H"), 
                        to = as.POSIXct(today()+1), 
                        by = "DSTday")
)

# construct daily list of OPEN tickets
ovot_all_teams <- data_frame()
for (i in 1:nrow(calendar)){
  insert_day <- calendar %>% slice(i) %>% mutate(j=1) %>%  
    left_join(mutate(state_history, j=1), by="j") %>%  # j=dummy column. cross join effectively.
    filter(Start <= calendar$datetime[i] & (calendar$datetime[i] < End | is.na(End))) %>%
    left_join(team_history, by = "Number") %>%
    filter(Start.y <= calendar$datetime[i] & (calendar$datetime[i] < End.y | is.na(End.y))) %>% 
    distinct()
  ovot_all_teams <- bind_rows(ovot_all_teams, insert_day)
}
ovot_all_teams <- ovot_all_teams %>% distinct()

# prune & output file
out <- ovot_all_teams %>% select(Number, datetime, Status=Value.x, Team=Value.y)
writeLines(paste("Exporting file now at", Sys.time(),"\n Elapsed time:", Sys.time()-start_time))
write.csv(out, na = "", row.names = FALSE, paste0(path, "\\ovot_all_teams.csv"))

# timing
writeLines(paste0("Start time: ", start_time, "\nEnd time: ", Sys.time(), "\nElapsed time: ", Sys.time() - start_time))

