# Mean Time to Response (MTTR) & Mean Time Restore Service (MTRS) Analysis
# Relies on both team pass history & assignment pass history

library(tidyverse); library(lubridate); library(tools)
start_time <- Sys.time()
print(paste("Starting:", start_time))

# find all historical files for import ----
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
team_history_files <- list.files(path, "(?i)team_hist", full.names = TRUE)
assign_history_files <- list.files(path, "(?i)assign_hist", full.names = TRUE)

import_files <- function(files){
  out <- data_frame()
  for (i in 1:length(files)){
    if (file_ext(files[i]) == "xlsx") { data <- readxl::read_excel(files[i])
    } else if (file_ext(files[i]) == "csv") { data <- read.csv(files[i])  # make change: convert columns on import
    } else { break }
    data$import_sheet <- str_extract(files[i], "(?<=/).*") # positive lookbehind
    out <- bind_rows(out, data)
  }
  return(out)
}

# import team history data & filter out flash assignments ----
team_history <- import_files(team_history_files) %>% distinct(Number, Field, Value, Start, End, Resolved, State) %>% arrange(Start)
filter_out_teams <- team_history %>% filter(Start == End)  # anti join to filter out flash assignments, while keeping NA times
team_history <- team_history %>% anti_join(filter_out_teams)

# import assign history data (from csv) & filter out blank & flash assignments ----
assign_history <- import_files(assign_history_files) %>% 
  rename(Number=inc_number, Field=mi_field, Value=mi_value, Start=mi_start, End=mi_end, Resolved=inc_resolved_at, State=inc_state) %>%
  distinct(Number, Field, Value, Start, End, Resolved, State) %>%
  arrange(Start) %>%
  filter(Value != '')  # filtering out 'removed assignments' (shows up as 'assigned to [Empty])
assign_history$Number <- as.character(assign_history$Number)
assign_history$Field <- as.character(assign_history$Field)
assign_history$Value <- as.character(assign_history$Value)
assign_history$Start <- as.POSIXct(assign_history$Start, format = "%m-%d-%Y %H:%M:%S", tz = "UTC") # other dt funcs default UTC
assign_history$End <- as.POSIXct(assign_history$End, format = "%m-%d-%Y %H:%M:%S", tz = "UTC")
assign_history$Resolved <- as.POSIXct(assign_history$Resolved, format = "%m-%d-%Y %H:%M:%S", tz = "UTC")
assign_history$State <- as.character(assign_history$State)

filter_out_assigns <- assign_history %>% filter(Start == End)  # anti join to filter out flash assignments, while keeping NA times
assign_history <- assign_history %>% anti_join(filter_out_assigns)

# get first team per ticket ----
first_team <- team_history %>% group_by(Number) %>% filter(Start == min(Start)) %>% select(Number, first_team = Value)

# get count of times each ticket was assigned to L1 ----
L1_assigns <- team_history %>% filter(Value == 'Retail Support') %>% count(Number) %>% rename(L1_assigns = n)
  
# get first L1/L2/L3/aloha/payments team assignment per ticket ----
first_L1 <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Support') %>% filter(Start == min(Start)) %>%
  mutate(first_L1 = 1) %>% select(Number, Start, first_L1)
first_L2 <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Support L2') %>% filter(Start == min(Start)) %>%
  mutate(first_L2 = 1) %>% select(Number, Start, first_L2)
first_L3 <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Support L3') %>% filter(Start == min(Start)) %>%
  mutate(first_L3 = 1) %>% select(Number, Start, first_L3)
first_aloha <- team_history %>% group_by(Number) %>% filter(Value == 'Aloha Support Team') %>% filter(Start == min(Start)) %>%
  mutate(first_aloha = 1) %>% select(Number, Start, first_aloha)
first_payments <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Payments') %>% filter(Start == min(Start)) %>%
  mutate(first_payments = 1) %>% select(Number, Start, first_payments)

# compile all team & analyst passing history to compare team assignments w/ initial analyst assignments ----
all_history <- bind_rows(team_history, assign_history) %>% arrange(Start)
all_history <- all_history %>% arrange(Number, Start) %>% group_by(Number) %>% 
  mutate(event_num = row_number(),
         prev_time = lag(Start, order_by = Start),
         prev_action = lag(Field, order_by = Start),
         response_time = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' ~ 
                                     difftime(Start, prev_time, units='mins')))

# get first analyst responses for teams L1/L2/L3/aloha/payments ----
all_history <- all_history %>% left_join(first_L1) %>% left_join(first_L2) %>% left_join(first_L3) %>%
  left_join(first_aloha) %>% left_join(first_payments)

all_history <- all_history %>% arrange(Number, Start) %>% group_by(Number) %>%
  mutate(L1_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_L1 == 1) ~ 1),
         L2_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_L2 == 1) ~ 1),
         L3_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_L3 == 1) ~ 1),
         aloha_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_aloha == 1) ~ 1),
         payments_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_payments == 1) ~ 1))

first_L1_response <- all_history %>% filter(L1_response == 1) %>% 
  select(Number, L1_Response_Analyst = Value, L1_Response_Time = response_time, L1_Time_of_Response = Start)
first_L2_response <- all_history %>% filter(L2_response == 1) %>%
  select(Number, L2_Response_Analyst = Value, L2_Response_Time = response_time, L2_Time_of_Response = Start)
first_L3_response <- all_history %>% filter(L3_response == 1) %>%
  select(Number, L3_Response_Analyst = Value, L3_Response_Time = response_time, L3_Time_of_Response = Start)
first_aloha_response <- all_history %>% filter(aloha_response == 1) %>%
  select(Number, aloha_Response_Analyst = Value, aloha_Response_Time = response_time, aloha_Time_of_Response = Start)
first_payments_response <- all_history %>% filter(payments_response == 1) %>%
  select(Number, payments_Response_Analyst = Value, payments_Response_Time = response_time, payments_Time_of_Response = Start)

# get start time of last assigned team for time-to-restore-service by assignment time (instead of creation)
# (reopened tickets will still have old resolve time, and thus incorrectly show a TTRS)
last_team_start <- team_history %>% group_by(Number) %>% filter(Start == max(Start)) %>%
  mutate(team_time_to_restore_service = difftime(Resolved, Start, units = 'mins')) %>%
  select(Number, last_team = Value, last_team_start = Start, team_time_to_restore_service)

# import incident list. join aggregation data to incident list ----
OnePOS_Incidents_Import <- readxl::read_excel(path = "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\incident.xlsx")
out <- OnePOS_Incidents_Import %>% 
  left_join(first_team) %>%
  left_join(L1_assigns) %>%
  left_join(first_L1_response) %>% left_join(first_L2_response) %>% left_join(first_L3_response) %>%
  left_join(first_aloha_response) %>% left_join(first_payments_response) %>%
  left_join(last_team_start)
if (nrow(out) == nrow(OnePOS_Incidents_Import)) "data is good" else "not good"  # check for duplicates/problems

# export extended incident list ----
writexl::write_xlsx(x = out, path = "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\extended_metrics.xlsx")


# notes:
# all_history: most tickets sequentially start with team assignments (because of join order). just ~10 tickets do not.
# Value == "" literally means analyst assignment was removed. shows "assigned to [Empty]" in ticket history

# reference: convert string to POSIXct UTC time 
# t1 <- ymd_hms("2018-07-19 08:08:22")

# reference: workspace mgmt
# rm(list=ls()[str_detect(ls(), "test")])

# reference: benchmarking time
# start_time <- Sys.time()
# writeLines(paste("Starting:", start_time))
writeLines(paste0("Start time: ", start_time,
                  "\nEnd time: ", Sys.time(),
                  "\nElapsed time: ", round(difftime(Sys.time(),start_time, units='secs'),2), " seconds."))
