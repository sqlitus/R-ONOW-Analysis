# INC Full List - First 5 teams

# Purpose: Sort/clean Team History Data, and get distinct list of INC with 1st/2nd/3rd/4th/5th teams

# Note 
# 2019-04-08: Currently using Excel because CSV writexl problems.
# Using Incident Team History extracts - OLD EXCEL DATASETS W/ OLD HISTORICAL TEAM NAMES


# conditionally install packages if not already installed, then load
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)



# benchmarking variables
start_time <- Sys.time()
writeLines(str_glue('Starting: {start_time}'))



# identify all appropriately named files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
team_history_files <- list.files(path, "(?i)inc_team_history_", full.names = TRUE)



# DATAFRAME: import and merge team [assignment] history
team_history <- data_frame()
writeLines(str_glue('Importing & merging team history. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
for (i in 1:length(team_history_files)){
  data <- readxl::read_excel(team_history_files[i])
  team_history <- bind_rows(team_history, data)
}



# FILTER OUT: flash assignments
writeLines(str_glue('Removing duplicate records. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
team_history <- team_history %>% select(Number, Value, Start, End) %>% distinct()  # keep distinct records
filter_out <- team_history %>% filter(Start == End)  # filter out 'flash' assignments
team_history <- team_history %>% anti_join(filter_out)  # safest way to filter out records (avoids null/empty errors)



# FILTER OUT: later redundant re-assignments records to same team (team1, then same team1 later; doesn't always show in audit log)
# (this sometimes occurs during other ticket updates. hypothesis: due to an assn. group change unsaved???)
writeLines(str_glue('Filtering out consecutive redundant assignments. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
assignment_group_reassignments <- team_history %>% 
  group_by(Number) %>%
  arrange(Start)
  mutate(prev_team = lag(Value)) %>%
  filter(Value == prev_team)
team_history <- team_history %>% anti_join(assignment_group_reassignments)



# df: get distinct incidents
writeLines(str_glue('Compiling distinct incidents. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
distinct_incidents <- team_history %>% select(Number) %>% distinct()



# dfs: get history team order, get 1st/2nd/etc teams, join to distinct_incidents
inc_history_team_order <- team_history %>% group_by(Number) %>% arrange(Start) %>%   # sort data once?
  mutate(team_order = row_number(Start),
         prev_team = lag(Value),
         prev_team_start = lag(Start),
         next_team = lead(Value),
         next_team_start = lead(Start))

writeLines(str_glue('Getting first 5 team assignments. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
inc_team_1 <- inc_history_team_order %>% filter(team_order == 1) %>% mutate(team_1 = Value, team_1_start = Start) %>% select(Number, team_1, team_1_start)
inc_team_2 <- inc_history_team_order %>% filter(team_order == 2) %>% mutate(team_2 = Value, team_2_start = Start) %>% select(Number, team_2, team_2_start)
inc_team_3 <- inc_history_team_order %>% filter(team_order == 3) %>% mutate(team_3 = Value, team_3_start = Start) %>% select(Number, team_3, team_3_start)
inc_team_4 <- inc_history_team_order %>% filter(team_order == 4) %>% mutate(team_4 = Value, team_4_start = Start) %>% select(Number, team_4, team_4_start)
inc_team_5 <- inc_history_team_order %>% filter(team_order == 5) %>% mutate(team_5 = Value, team_5_start = Start) %>% select(Number, team_5, team_5_start)

# df: distinct inc list w/ first 5 teams
writeLines(str_glue('Join onto distinct incidents. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
inc_list_first_5_teams <- distinct_incidents %>% 
  left_join(inc_team_1) %>%
  left_join(inc_team_2) %>%
  left_join(inc_team_3) %>%
  left_join(inc_team_4) %>%
  left_join(inc_team_5)



# OUTPUT DATASETS: full team history, inc list w/ first 5 teams.
fcr_path <- "\\\\cewp1650\\Chris Jabr Reports\\FCR Analysis\\Datasets"

writeLines(str_glue('Exporting full team history & first x teams list now at {Sys.time()}.\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
writexl::write_xlsx(x = inc_history_team_order, path = paste0(fcr_path, "\\INC All Team History.xlsx"))
writexl::write_xlsx(x = inc_list_first_5_teams, path = paste0(fcr_path, "\\INC Full List - first 5 teams.xlsx"))
writeLines(str_glue('\nDONE. \nStart time: {start_time}.\nEnd time: {Sys.time()}.\nElapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))

# TODO: check current writexl version, load old if needed
# packageVersion("writexl")



