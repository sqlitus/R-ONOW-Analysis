# Open Volume Over Time - HISTORICAL BACKLOG REPORT #
# Purpose: dataset of all open tickets for each day
# Note: Using Incident Team History & State History to find open queue volumes
# Log: touch ups, filtering out redundant re-assignments from team hist. Doesn't change OVOT in this case but speeds up script.



# conditionally install packages if not already installed, then load
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

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
  # data$import_sheet <- str_extract(state_history_files[i], "(?<=/).*") # positive lookbehind
  state_history <- bind_rows(state_history, data)
}
state_history <- state_history %>% select(Number, Field, Value, Start, End) %>% distinct()
filter_out <- state_history %>% filter(Start == End)
state_history <- state_history %>% anti_join(filter_out)



# merge assignment history
team_history <- data_frame()
for (i in 1:length(team_history_files)){
  data <- readxl::read_excel(team_history_files[i])
  # data$import_sheet <- str_extract(team_history_files[i], "(?<=/).*") # positive lookbehind
  team_history <- bind_rows(team_history, data)
}
team_history <- team_history %>% group_by(Number) %>% arrange(Start)



### Filter out incorrect data ----

# filter out 'flash' assignments
team_history <- team_history %>% select(Number, Field, Value, Start, End) %>% distinct()
filter_out <- team_history %>% filter(Start == End)
team_history <- team_history %>% anti_join(filter_out)

# then filter out redundant re-assignments
writeLines(str_glue('Filtering out consecutive redundant assignments. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
assignment_group_reassignments <- team_history %>% 
  group_by(Number) %>%
  arrange(Start) %>%
  mutate(prev_team = lag(Value),
         prev_team_time = lag(Start)) %>%
  filter(Value == prev_team)
team_history <- team_history %>% anti_join(assignment_group_reassignments)



# set TZ of imported times to CST, since created calendar defaults to that timezone.
# state_history[c('Start','End')] <- force_tz(state_history[c('Start','End')], tzone = 'US/Central')
# team_history[c('Start','End')] <- force_tz(team_history[c('Start','End')], tzone = 'US/Central')



# calendar table; datetime @ 8am; Timezone defaults to CDT
calendar_start <- (Sys.Date() - (7*14)) + ( 1 - as.integer(format(Sys.Date(), format = "%u"))) # last 14 weeks
calendar <- data_frame(
  date = seq.Date(from = calendar_start, to = today(), by = "days"),
  datetime = seq.POSIXt(from = as.POSIXct(paste(calendar_start, "08"), format = "%Y-%m-%d %H"), 
                        to = as.POSIXct(today()+1), 
                        by = "DSTday")
)
calendar$datetime <- force_tz(calendar$datetime, tzone = 'UTC')


# get all distinct incidents from both datasets
distinct_incidents <- bind_rows(state_history %>% select(Number), team_history %>% select(Number)) %>% distinct()

# construct daily list of open tickets per day. use state history first since it's all tickets.
ovot <- data_frame()
for (i in 1:nrow(calendar)){
  insert_day <- distinct_incidents %>% mutate(datetime = calendar$datetime[i]) %>% 
    left_join(state_history, by = "Number") %>%
    filter(Start <= calendar$datetime[i] & (calendar$datetime[i] < End | is.na(End))) %>%
    left_join(team_history, by = "Number") %>%
    filter(Start.y <= calendar$datetime[i] & (calendar$datetime[i] < End.y | is.na(End.y))) %>% 
    distinct()
  ovot <- bind_rows(ovot, insert_day)
}
ovot <- ovot %>% distinct()



# prune & output file
out <- ovot %>% select(Number, datetime, Status=Value.x, Team=Value.y)

# ## ADD: data for getting historical aging matrix
# ## IMPORT & MERGE TICKET DATA, LEFT JOIN FOR CREATED / PRIORITY INFO
# ## somehow adding duplicates?
# inc_list_files <- list.files(path, "(?i)all inc list", full.names = TRUE)
# all_incidents <- data_frame()
# for (i in 1:length(inc_list_files)){
#   data <- read.csv(inc_list_files[i])
#   all_incidents <- bind_rows(all_incidents, data)
# }
# all_incidents <- all_incidents %>% distinct()
# all_incidents[c('Created','Resolved')] <- force_tz(all_incidents[c('Created','Resolved')], tzone = 'US/Central')
# # inc_list <- readxl::read_excel("\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\incident.xlsx")
# # inc_list[c('Created','Resolved')] <- force_tz(inc_list[c('Created','Resolved')], tzone = 'US/Central')
# 
# out <- out %>% left_join(select(all_incidents, Number, Priority, Created), by = "Number") %>% distinct()


# output
writeLines(paste("Exporting file now at", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
# write.csv(out, na = "", row.names = FALSE, paste0(path, "\\ovot.csv"))
writexl::write_xlsx(x = out, path = paste0(path, "\\ovot.xlsx"))  # switch to this
writeLines(paste0("DONE. \nStart time: ", start_time,
                  "\nEnd time: ", Sys.time(),
                  "\nElapsed time: ", round(difftime(Sys.time(),start_time, units='secs'),2), " seconds."))

