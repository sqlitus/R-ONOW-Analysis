# Mean Time to Response (MTTR) & Mean Time Restore Service (MTRS) Analysis
# Relies on both team pass history & analyst assignment history & full export of all OnePOS Incidents

library(tidyverse); library(lubridate); library(tools)
start_time <- Sys.time()
writeLines(paste("Starting:", start_time))

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
  ## add distinct('everything-but-import-sheet') line
  return(out)
}

# import team history data ----
writeLines(paste("Importing team history at:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
team_history <- import_files(team_history_files) %>% distinct(Number, Field, Value, Start, End, Resolved, State) %>% arrange(Start)



# FILTER OUT: flash assignments - team passes ----
filter_out_teams <- team_history %>% filter(Start == End)  # anti join to filter out flash assignments, while keeping NA times
team_history <- team_history %>% anti_join(filter_out_teams)



# FILTER OUT: later redundant re-assignments records to same team (team1, then same team1 later; doesn't always show in audit log) ----
# (this sometimes occurs during other ticket updates. hypothesis: due to an assn. group change unsaved???)
writeLines(str_glue('Filtering out consecutive redundant assignments. Elapsed time: {round(difftime(Sys.time(),start_time, units="secs"),1)} seconds'))
assignment_group_reassignments <- team_history %>% 
  group_by(Number) %>%
  arrange(Start) %>%
  mutate(prev_team = lag(Value)) %>%
  filter(Value == prev_team)
team_history <- team_history %>% anti_join(assignment_group_reassignments)



# import assign history data (from csv) & filter out blank & flash assignments ----
writeLines(paste("Importing assignment history at:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))

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



# FILTER OUT: flash assignments ----
filter_out_assigns <- assign_history %>% filter(Start == End)  # anti join to filter out flash assignments, while keeping NA times
assign_history <- assign_history %>% anti_join(filter_out_assigns)



# FILTER OUT: later redundant re-assignments to same person ----
filter_out_reassigns <- assign_history %>%
  group_by(Number) %>%
  arrange(Start) %>%
  mutate(prev_analyst = lag(Value)) %>%
  filter(Value == prev_analyst)
assign_history <- assign_history %>% anti_join(filter_out_reassigns)


# get first team per ticket ----
first_team <- team_history %>% group_by(Number) %>% filter(Start == min(Start)) %>% select(Number, first_team = Value)

# get count of times each ticket was assigned to L1 ----
L1_assigns <- team_history %>% filter(Value == 'Retail Support') %>% count(Number) %>% rename(L1_assigns = n)
  
# get first L1/L2/L3/aloha/payments/supply-chain team assignment per ticket ----
print(paste("Calculating initial team assignment times per ticket:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))

first_L1 <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Support') %>% filter(Start == min(Start)) %>%
  mutate(first_L1 = 1) %>% select(Number, Start, first_L1)
first_L2 <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Support L2') %>% filter(Start == min(Start)) %>%
  mutate(first_L2 = 1) %>% select(Number, Start, first_L2)
first_L3 <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Support L3') %>% filter(Start == min(Start)) %>%
  mutate(first_L3 = 1) %>% select(Number, Start, first_L3)
first_aloha <- team_history %>% group_by(Number) %>% filter(str_detect(Value, 'Aloha Support')) %>% filter(Start == min(Start)) %>%
  mutate(first_aloha = 1) %>% select(Number, Start, first_aloha)
first_payments <- team_history %>% group_by(Number) %>% filter(Value == 'Retail Payments') %>% filter(Start == min(Start)) %>%
  mutate(first_payments = 1) %>% select(Number, Start, first_payments)
first_supplychain <- team_history %>% group_by(Number) %>% filter(str_detect(Value, 'Supply Chain.*Merchandising Support')) %>% filter(Start == min(Start)) %>%
  mutate(first_supplychain = 1) %>% select(Number, Start, first_supplychain)


# additional teams ----
first_dvo_tech <- team_history %>% group_by(Number) %>% filter(str_detect(Value, 'DVO Technical Support')) %>% filter(Start == min(Start)) %>%
  mutate(first_dvo_tech = 1) %>% select(Number, Start, first_dvo_tech)
first_irma_tech <- team_history %>% group_by(Number) %>% filter(str_detect(Value, 'IRMA Technical Support')) %>% filter(Start == min(Start)) %>%
  mutate(first_irma_tech = 1) %>% select(Number, Start, first_irma_tech)
first_vip_tech <- team_history %>% group_by(Number) %>% filter(str_detect(Value, 'VIP Technical Support')) %>% filter(Start == min(Start)) %>%
  mutate(first_vip_tech = 1) %>% select(Number, Start, first_vip_tech)



# compile all team & analyst passing history to compare team assignments w/ initial analyst assignments ----
writeLines(paste("Calculating initial ANALYST reponses:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
all_history <- bind_rows(team_history, assign_history)
all_history <- all_history %>% group_by(Number) %>% arrange(Start) %>%
  mutate(event_num = row_number(),
         prev_time = lag(Start),
         prev_action = lag(Field),
         response_time = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' ~ 
                                     difftime(Start, prev_time, units='hours')))

# get first analyst responses for teams L1/L2/L3/aloha/payments/supply-chain ----
# AD-HOC FILTER: DON'T CALCULATE RESOLVED TICKETS...
writeLines(paste("Blending team & analyst histories. Calculating initial ANALYST reponses:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))

all_history <- all_history %>% left_join(first_L1) %>% left_join(first_L2) %>% left_join(first_L3) %>%
  left_join(first_aloha) %>% left_join(first_payments) %>% left_join(first_supplychain) %>%
# additional
  left_join(first_dvo_tech) %>%
  left_join(first_irma_tech) %>%
  left_join(first_vip_tech) 


all_history <- all_history %>% group_by(Number) %>% arrange(Start) %>% 
  mutate(L1_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_L1 == 1) ~ 1),  # & (Start < Resolved | (!is.na(Start) & is.na(Resolved)))
         L2_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_L2 == 1) ~ 1),
         L3_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_L3 == 1) ~ 1),
         aloha_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_aloha == 1) ~ 1),
         payments_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_payments == 1) ~ 1),
         supplychain_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_supplychain == 1) ~ 1),
         
         # additional
         dvo_tech_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_dvo_tech == 1) ~ 1),
         irma_tech_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_irma_tech == 1) ~ 1),
         vip_tech_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' & lag(first_vip_tech == 1) ~ 1)
         )

writeLines(paste("Partitioning ANALYST reponses into tables:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
first_L1_response <- all_history %>% filter(L1_response == 1) %>% 
  mutate(L1_Time_of_Response_Date = date(Start)) %>%
  select(Number, L1_assignment_time = prev_time, L1_Response_Analyst = Value, L1_Response_Time = response_time, L1_Time_of_Response = Start, L1_Time_of_Response_Date) %>%
  mutate(L1_assignment_date = date(L1_assignment_time))
first_L2_response <- all_history %>% filter(L2_response == 1) %>%
  mutate(L2_Time_of_Response_Date = date(Start)) %>%
  select(Number, L2_assignment_time = prev_time, L2_Response_Analyst = Value, L2_Response_Time = response_time, L2_Time_of_Response = Start, L2_Time_of_Response_Date) %>%
  mutate(L2_assignment_date = date(L2_assignment_time))
first_L3_response <- all_history %>% filter(L3_response == 1) %>%
  mutate(L3_Time_of_Response_Date = date(Start)) %>%
  select(Number, L3_assignment_time = prev_time, L3_Response_Analyst = Value, L3_Response_Time = response_time, L3_Time_of_Response = Start, L3_Time_of_Response_Date) %>%
  mutate(L3_assignment_date = date(L3_assignment_time))
first_aloha_response <- all_history %>% filter(aloha_response == 1) %>%
  mutate(aloha_Time_of_Response_Date = date(Start)) %>%
  select(Number, aloha_assignment_time = prev_time, aloha_Response_Analyst = Value, aloha_Response_Time = response_time, aloha_Time_of_Response = Start, aloha_Time_of_Response_Date) %>%
  mutate(aloha_assignment_date = date(aloha_assignment_time))
first_payments_response <- all_history %>% filter(payments_response == 1) %>%
  mutate(payments_Time_of_Response_Date = date(Start)) %>%
  select(Number, payments_assignment_time = prev_time, payments_Response_Analyst = Value, payments_Response_Time = response_time, payments_Time_of_Response = Start, payments_Time_of_Response_Date) %>%
  mutate(payments_assignment_date = date(payments_assignment_time))
first_supplychain_response <- all_history %>% filter(supplychain_response == 1) %>%
  mutate(supplychain_Time_of_Response_Date = date(Start)) %>%
  select(Number, supplychain_assignment_time = prev_time, supplychain_Response_Analyst = Value, supplychain_Response_Time = response_time, supplychain_Time_of_Response = Start, supplychain_Time_of_Response_Date) %>%
  mutate(supplychain_assignment_date = date(supplychain_assignment_time))

# additional
first_dvo_tech_response <- all_history %>% filter(dvo_tech_response == 1) %>%
  mutate(dvo_tech_Time_of_Response_Date = date(Start)) %>%
  select(Number, dvo_tech_assignment_time = prev_time, dvo_tech_Response_Analyst = Value, dvo_tech_Response_Time = response_time, dvo_tech_Time_of_Response = Start, dvo_tech_Time_of_Response_Date) %>%
  mutate(dvo_tech_assignment_date = date(dvo_tech_assignment_time))
first_irma_tech_response <- all_history %>% filter(irma_tech_response == 1) %>%
  mutate(irma_tech_Time_of_Response_Date = date(Start)) %>%
  select(Number, irma_tech_assignment_time = prev_time, irma_tech_Response_Analyst = Value, irma_tech_Response_Time = response_time, irma_tech_Time_of_Response = Start, irma_tech_Time_of_Response_Date) %>%
  mutate(irma_tech_assignment_date = date(irma_tech_assignment_time))
first_vip_tech_response <- all_history %>% filter(vip_tech_response == 1) %>%
  mutate(vip_tech_Time_of_Response_Date = date(Start)) %>%
  select(Number, vip_tech_assignment_time = prev_time, vip_tech_Response_Analyst = Value, vip_tech_Response_Time = response_time, vip_tech_Time_of_Response = Start, vip_tech_Time_of_Response_Date) %>%
  mutate(vip_tech_assignment_date = date(vip_tech_assignment_time))



# get start time of last assigned team for time-to-restore-service by assignment time (instead of creation)
# (reopened tickets will still have old resolve time, and thus incorrectly show a TTRS. filter out non-opens in workbook)
# (2019-04-09: this data has been unused for some time)
writeLines(paste("Getting last team start time, for MTRS by individ. team time:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
last_team_start <- team_history %>% group_by(Number) %>% filter(Start == max(Start)) %>%
  mutate(last_team_start_date = date(Start),
         team_TRS_hours = difftime(Resolved, Start, units = 'hours')) %>%
  select(Number, last_team = Value, last_team_start = Start, last_team_start_date, team_TRS_hours)

# import incident list from multiple sheets. join aggregation data to incident list ----
writeLines(paste("Importing full OnePOS+SCMS INC list:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))

onepos_files <- list.files("\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\OnePOS Incidents", "(?i)onepos_inc", full.names = TRUE)
OnePOS_Incidents_Import <- import_files(files = onepos_files)
OnePOS_Incidents_Import$import_sheet <- NULL
OnePOS_Incidents_Import <- OnePOS_Incidents_Import %>% distinct()


# to add: filter by quarter
print(paste("Joining reponse times to INC list:", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))

out <- OnePOS_Incidents_Import %>% 
  left_join(first_team) %>%
  left_join(L1_assigns) %>%
  left_join(first_L1_response) %>% left_join(first_L2_response) %>% left_join(first_L3_response) %>%
  left_join(first_aloha_response) %>% left_join(first_payments_response) %>%
  left_join(first_supplychain_response) %>%
  # additional
  left_join(first_dvo_tech_response) %>%
  left_join(first_irma_tech_response) %>%
  left_join(first_vip_tech_response) %>%
  
  left_join(last_team_start)
if (nrow(out) == nrow(OnePOS_Incidents_Import)) "data is good" else "not good"  # check for duplicates/problems

# export extended incident list ----
writeLines(paste("Exporting file now at", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
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
writeLines(paste0("DONE.",
                  "\nStart time: ", start_time,
                  "\nEnd time: ", Sys.time(),
                  "\nElapsed time: ", round(difftime(Sys.time(),start_time, units='secs'),2), " seconds."))
