# GHD transfer analysis

# Purpose: prep GHD transfer vs resolve dataset. Only show GHD assignment records, w/ data on what happened next.

# Who: Phil

# Note
# Just uses team history. 
# Noticed "additional comments" creates a redundant team assignment?

# Log: 2019-04-08 distinct GHD INC list was not distinct. Corrected. Also filtering out redundant assignments


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)
start_time <- Sys.time()
print(paste("Starting:", start_time))



# import only assignment group change data files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
team_history_files <- list.files(path, "(?i)team_hist", full.names = TRUE)



# merge all assignment (team) history
team_history <- data_frame()
for (i in 1:length(team_history_files)){
  data <- readxl::read_excel(team_history_files[i])
  # data$import_sheet <- str_extract(team_history_files[i], "(?<=/).*") # positive lookbehind
  team_history <- bind_rows(team_history, data)
}
team_history <- team_history %>% distinct(Number, Field, Value, Start, End, Resolved, State, .keep_all = TRUE)



# FILTER OUT: get distinct tickets ever touched by GHD, and keep just those
tickets_GHD_touched <- team_history %>% filter(Value == "Global Help Desk") %>% select(Number) %>% distinct()
out <- team_history %>% inner_join(tickets_GHD_touched)



# FILTER OUT: remove 'flash' assignments with same start & end dates
flash_assignments <- out %>% filter(State == 'Canceled' | Start == End)
out <- out %>% anti_join(flash_assignments)  



# FILTER OUT: redundant re-assignments
assignment_group_reassignments <- out %>% 
  group_by(Number) %>%
  arrange(Start) %>%
  mutate(prev_team = lag(Value), prev_team_start = lag(Start)) %>%
  filter(Value == prev_team)
out <- out %>% anti_join(assignment_group_reassignments)



# CALC: with clean GHD passing data, calc prev/next teams
out <- out %>% 
  group_by(Number) %>% 
  arrange(Start) %>% 
  mutate(prev_team = lag(Value), next_team = lead(Value), next_team_assign_time = lead(Start))



# CALC: determine if an assignment was then transferred, resolved, or neither.
out <- out %>% mutate(t_or_r = case_when(next_team_assign_time <= Resolved | (!is.na(next_team_assign_time) & is.na(Resolved)) 
                                         ~ "Transferred",
                                         next_team_assign_time > Resolved | (is.na(next_team_assign_time) & !is.na(Resolved))
                                         ~ "Resolved",
                                         is.na(next_team_assign_time) & is.na(Resolved)
                                         ~ "Still open"
                                         ))



# OLD: calcs for GHD team handling
# FILTER: keep direct GHD assignments only. The analysis shows where tickets went (Transferred or resolved)
out <- out %>% filter(Value == "Global Help Desk")
out <- out %>% mutate(Transferred_to = case_when(str_detect(next_team, "(?i)Regional IT") | 
                                                   str_detect(next_team, "(?i)msi shared") ~ next_team,
                                                 TRUE ~ "Other"),
                      Transfer_grouping = case_when(next_team %in% c('MSI Shared Services Support', 
                                                                     'Cross Regional Infrastructure Support',
                                                                     'Cross Regional Application Support',
                                                                     'Client Solutions and Support') ~ next_team,
                                                    TRUE ~ "All Other Teams"),
                      Handled = case_when(t_or_r == "Transferred" & 
                                            (str_detect(next_team, "(?i)Regional IT") | str_detect(next_team, "(?i)msi shared"))
                                          ~ paste0("Transferred - ", next_team),
                                          t_or_r == "Transferred" ~ paste0("Transferred - Other"),
                                          TRUE ~ t_or_r))



writeLines(paste("Exporting file now at", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
# write.csv(out, na="", row.names=FALSE, "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History\\GHD Transfer History\\ghdt.csv")
writexl::write_xlsx(out, path= "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History\\GHD Transfer History\\GHD_handling.xlsx")
writeLines(paste0("DONE.\nStart time: ", start_time,
                  "\nEnd time: ", Sys.time(),
                  "\nTotal elapsed time: ", round(difftime(Sys.time(),start_time, units='secs'),2), " seconds."))
