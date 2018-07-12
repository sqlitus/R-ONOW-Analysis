# Phil's GHD transfer analysis

library(tidyverse); library(lubridate)
start_time <- Sys.time()
print(paste("Starting:", start_time))

# import all appropriately named files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
team_history_files <- list.files(path, "(?i)team_hist", full.names = TRUE)

# merge assignment(team) history
team_history <- data_frame()
for (i in 1:length(team_history_files)){
  data <- readxl::read_excel(team_history_files[i])
  data$import_sheet <- str_extract(team_history_files[i], "(?<=/).*") # positive lookbehind
  team_history <- bind_rows(team_history, data)
}
team_history <- team_history %>% distinct(Number, Field, Value, Start, End, Resolved, State, .keep_all = TRUE)

# prune assignment history. remove assignments with same start & end dates, and reassignments to same team.
# should represent true transfer history between teams
out <- team_history
out <- out %>% filter(State != "Canceled", Start != End)  # filtered out 'flash' reassignments to same team
out <- out %>% group_by(Number) %>% mutate(prev_team = lag(Value, order_by = Start), 
                                           next_team = lead(Value, order_by = Start))
out <- out %>% filter(prev_team != Value | is.na(prev_team))  # include nulls 
out <- out %>% group_by(Number) %>% mutate(prev_team = lag(Value, order_by = Start), 
                                           next_team = lead(Value, order_by = Start), 
                                           next_team_assign_time = lead(Start, order_by = Start))

out <- out %>% mutate(t_or_r = case_when(next_team_assign_time < Resolved | (!is.na(next_team_assign_time) & is.na(Resolved)) 
                                         ~ "Transferred",
                                         next_team_assign_time > Resolved | (is.na(next_team_assign_time) & !is.na(Resolved))
                                         ~ "Resolved",
                                         is.na(next_team_assign_time) & is.na(Resolved)
                                         ~ "Still assigned"
                                         ))
out <- out %>% arrange(Number, Start)
out <- out %>% filter(Value == "Global Help Desk")

write.csv(out, na="", row.names=FALSE, "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History\\GHD Transfer History\\ghdt.csv")



# # flash <- out %>% filter(Start == End)
# # mult <- out %>% filter(n() > 1)
# # test <- out %>% filter(next_team == Value & !is.na(prev_team))
# # test2 <- out %>% filter(Number %in% test$Number & Number %in% mult$Number)
# out %>% filter(Number %in% c("INC0030507", "INC0030592", "INC0018115", "INC0011140")) %>% View()
# 
# # checking flash reassingments to diff team ( don't filter start != end.)
# flash <- out %>% filter(Start == End & Value != next_team) %>% select(Number) # checking flash assigns to diff team
# out %>% filter(Number %in% flash$Number) %>% arrange(Number, Start) %>% View()
# out %>% filter(Number %in% c("INC0015184", "INC0010560", "INC0016940")) %>% View() 


