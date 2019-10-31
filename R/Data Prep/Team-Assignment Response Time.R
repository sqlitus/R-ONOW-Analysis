#' RESPONSE TIME
#' 
#' Purpose: Response Times for all team passes. Passed to team, then assigned to  analyst. Delta between those 2 events.
#' 
#' Date requested: 10/22/2019
#' 
#' Data:
#' assignment group history
#' assigned to history
#' 
#' Metric Definitions:
#' Response - delta between team pass & analyst pass
#' 
#' # Who: PN (& PL) - GHD

# Log: 2019-04-08 distinct GHD INC list was not distinct. Corrected. Also filtering out redundant assignments


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, readxl, writexl, data.table)
start_time <- Sys.time()
print(paste("Starting:", start_time))


# VARIABLES ----
path_import <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
path_export <- paste0(path_import, "\\test - Response times.xlsx")
import_teams <- list.files(path_import, "INC_team_history.*xlsx", full.names = TRUE) #; print(import_teams)
import_analysts <- list.files(path_import, "INC_assign_history..*csv", full.names = TRUE) #; print(import_analysts)


# FUNCTIONS ----
import_files <- function(files){
  require(tools); require(readxl); require(data.table); require(tidyverse)
  
  fstart <- Sys.time()
  files_passed <- deparse(substitute(files))
  
  writeLines(str_glue('Importing "{files_passed}" at {fstart}'))
  
  out <- data_frame()
  for (i in 1:length(files)){
    if (file_ext(files[i]) == "xlsx") { data <- readxl::read_excel(files[i])
    } else if (file_ext(files[i]) == "csv") { data <- fread(files[i])  # make change: convert columns on import
    } else { break }
    # data$import_sheet <- str_extract(files[i], "(?<=/).*") # positive lookbehind
    out <- bind_rows(out, data)
  }
  
  print(paste0('import done at ', round(difftime(Sys.time(),fstart, units="secs"),1)))
  writeLines(str_glue('import for "{files_passed}" done at {Sys.time()}. {round(difftime(Sys.time(),fstart, units="secs"),1)}'))
  
  return(out)
}

clean_data <- function(df){
  require(tools); require(data.table); require(tidyverse)
  
  # remove flash assigns
  r1 <- df[Start == End]
  df <- df %>% anti_join(r1)
  
  # remove redundant reassigns
  r2 <- df %>% 
    group_by(Number) %>%
    arrange(Start) %>%
    mutate(prev_team = lag(Value)) %>%
    filter(Value == prev_team)
  
  df <- df %>% anti_join(r2)
  
  return(df)
}



# IMPORT DATA ----
hist_team <- import_files(import_teams)
hist_analyst <- import_files(import_analysts)


# CLEAN - EXCEL - TEAM HIST ----
# 1 - remove any duplicated rows
# r0 <- hist_team %>% group_by_all() %>% filter(n() > 1)  # duplicated rows
hist_team <- hist_team %>% distinct()


# 2- remove all flash assigns
r1 <- hist_team %>% filter(Start == End)
hist_team <- hist_team %>% anti_join(r1)


# 3 - remove redundant reassigns
r2 <- hist_team %>% 
  group_by(Number) %>%
  arrange(Start) %>%
  mutate(prev_team = lag(Value)) %>%
  filter(Value == prev_team)
hist_team <- hist_team %>% anti_join(r2)


# CLEAN - CSV - ANALYST HIST ----
# 0 - rename & type text columns
# hist_analyst <- as.data.table(hist_analyst)

hist_analyst <- hist_analyst %>%
  rename(Number=inc_number, Field=mi_field, Value=mi_value, Start=mi_start, End=mi_end, Resolved=inc_resolved_at, State=inc_state)

hist_analyst$Number <- as.character(hist_analyst$Number)
hist_analyst$Field <- as.character(hist_analyst$Field)
hist_analyst$Value <- as.character(hist_analyst$Value)
hist_analyst$Start <- as.POSIXct(hist_analyst$Start, format = "%m-%d-%Y %H:%M:%S", tz = "UTC") # other dt funcs default UTC
hist_analyst$End <- as.POSIXct(hist_analyst$End, format = "%m-%d-%Y %H:%M:%S", tz = "UTC")
hist_analyst$Resolved <- as.POSIXct(hist_analyst$Resolved, format = "%m-%d-%Y %H:%M:%S", tz = "UTC")
hist_analyst$State <- as.character(hist_analyst$State)


# 0 - remove empty assign records
hist_analyst <- hist_analyst %>% filter(Value != '')


# 1 - remove any duplicated rows
# a0 <- hist_analyst %>% group_by_all() %>% filter(n() > 1)  # duplicated rows
hist_analyst <- hist_analyst %>% distinct()


# 2- remove all flash assigns
a1 <- hist_analyst %>% filter(Start == End)
hist_analyst <- hist_analyst %>% anti_join(a1)


# 3 - remove redundant reassigns
a2 <- hist_analyst %>% 
  group_by(Number) %>%
  arrange(Start) %>%
  mutate(prev_analyst = lag(Value)) %>%
  filter(Value == prev_analyst)
hist_analyst <- hist_analyst %>% anti_join(a2)





# CALC: team & analyst orders. repeat assigned count ----
hist_team <- hist_team %>% arrange(Start) %>% group_by(Number) %>% 
  mutate(team_order = row_number()) %>%
  group_by(Number, Value) %>%
  mutate(team_assigned_count = row_number()) %>%
  arrange(Number, Start)

hist_analyst <- hist_analyst %>% arrange(Start) %>% group_by(Number) %>%
  mutate(analyst_order = row_number()) %>%
  group_by(Number, Value) %>%
  mutate(analyst_assigned_count = row_number()) %>%
  arrange(Number, Start)

# LIST - team names synonyms. first name is current.
teamnames_r10_hardware <- c("Retail Hardware", "R10 Hardware Support")
teamnames_aloha <- c("Aloha Support", "Aloha Support Team")
teamnames_aloha_hardware <- c("Aloha Hardware", "Aloha Hardware Support")
teamnames_scms <- c("Supply Chain and Merchandising Support", "Supply Chain & Merchandising Support")
teamnames_scm_arch <- c("Supply Chain and Merchandising Architecture", "Supply Chain & Merchandising Architecture")


# MERGE ANALYST/TEAM HIST ----
hist_full <- bind_rows(hist_team, hist_analyst) %>% arrange(Number, Start)


# CLEAN - NORMALIZE TEAM NAMES ----
hist_full <- hist_full %>% ungroup %>% mutate(Value = case_when(Value %in% teamnames_aloha ~ teamnames_aloha[1],
                                                         Value %in% teamnames_aloha_hardware ~ teamnames_aloha_hardware[1],
                                                         Value %in% teamnames_r10_hardware ~ teamnames_r10_hardware[1],
                                                         Value %in% teamnames_scms ~ teamnames_scms[1],
                                                         Value %in% teamnames_scm_arch ~ teamnames_scm_arch[1],
                                                         TRUE ~ Value)
)


# CALC - event order, prev actions ----
hist_full <- hist_full %>% group_by(Number) %>% arrange(Number,Start) %>%
  mutate(event_order = row_number(),
         prev_time = lag(Start),
         prev_action = lag(Field))

# CALC - response time, initial responses
#TODO: INITIAL RESPONSE TIME FLAGS
hist_full <- hist_full %>% group_by(Number) %>% arrange(Number,Start) %>%
  mutate(flag_response = case_when(Field == 'assigned_to' & prev_action == 'assignment_group' ~ 1)) %>%
  mutate(flag_initial_response = case_when(flag_response == 1 & team_assigned_count == 1 ~ 1)) %>%
  mutate(response_time_mins = difftime(Start, prev_time, units='mins'))

# or 2:
hist_responses <- hist_full %>% group_by(Number) %>% arrange(Number,Start) %>%
  filter(Field == 'assigned_to' & prev_action == 'assignment_group') %>%
  mutate(flag_response = 1)

hist_initial_responses <- hist_responses %>% group_by(Number) %>% arrange(Number,Start) %>%
  filter(flag_response == 1 & team_assigned_count == 1) %>%
  mutate(flag_initial_response = 1) %>%
  mutate(response_time_mins = difftime(Start, prev_time, units='mins'))


hist_full <- hist_full %>% 
  left_join(hist_responses) %>%
  left_join(hist_initial_responses)

# TEST ----
z <- hist_team %>% select(Value) %>% filter(str_detect(Value, "(?i)retail")) %>% distinct()
z <- hist_full %>% group_by(Number) %>% filter(n() > 5)
z <- hist_full %>% select(Value_norm) %>% distinct()
z <- hist_full %>% filter(Value != Value_norm) 
z %>% select(Value) %>% distinct()
z %>% select(Value_norm) %>% distinct()
hist_full$Value_norm <- NULL
z <- hist_team %>% group_by(Number) %>% filter(n() > 5)
z <- hist_team %>% filter(Number == "INC0011502")
z <- hist_analyst %>% filter(Number == "INC0011502")
z <- hist_full %>% select(Value)%>% filter(str_detect(Value, "(?i)retail")) %>% distinct()

# Delivery 1 ----
# ghd overall avg
#' from ghd to INITIAL response time
#' 
#' 
#' 
#' Delivery 2 ----
#' state change after assignment ^
#' 
#' Delivery 3 ----
#' 
#' 
#' tickets that went to GHD, then came back to GHD...
#' 
#' 
#' 
