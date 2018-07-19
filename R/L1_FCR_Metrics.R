# L1 FCR Metrics


library(tidyverse)
OnePOS_Incidents_Import <- readxl::read_excel(path = "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\incident.xlsx")
inc_data <- OnePOS_Incidents_Import

## calculated columns ----
inc_data$title_extracted_BU <- str_extract(inc_data$`Short description`, "\\b\\d{5}\\b")
inc_data$title_extracted_DeviceName <- inc_data$`Short description` %>%
  str_extract("(?i)wfm\\s?\\d{5}\\s?[a-z]{3}\\s?\\d{2,3}") %>%
  str_replace_all("\\s", "") %>% toupper()
inc_data$title_extracted_DeviceName_BU <- str_extract(inc_data$title_extracted_DeviceName, "\\d{5}")
inc_data$derived_BU <- case_when(
  !is.na(inc_data$title_extracted_BU) ~ inc_data$title_extracted_BU,
  !is.na(inc_data$title_extracted_DeviceName_BU) ~ inc_data$title_extracted_DeviceName_BU,
  TRUE ~ inc_data$BU)
inc_data$derived_BU_source <- case_when(
  !is.na(inc_data$title_extracted_BU) ~ "short description BU#",
  !is.na(inc_data$title_extracted_DeviceName_BU) ~ "short description device name",
  TRUE ~ "BU field")
inc_data$derived_Lane <- inc_data$`Short description` %>%
  str_extract("(?i)(lane|reg|tab|pck|svr|aha)\\W{0,2}\\d{2,3}") %>% toupper() %>%
  str_replace_all("\\W", "") %>%
  str_replace("([A-Z])(\\d)", "\\1 \\2") %>%
  str_replace("(?i)lane", "REG")
inc_data$derived_Phase_Num <- 
  str_extract(inc_data$`Short description`, "(\\d{1,2}½?|\\d[ ]?\\d/\\d)[ ]?[.]\\d[.]\\d")  # 3½ or 3 1/2 or 3.5 etc
inc_data$extracted_Location <- 
  str_extract(inc_data$`Short description`, "\\b(((?i)(CE|FL|MA|MW|NA|NC|NE|PN|RM|SP|SW|TS))|SO|365)\\b\\W{0,3}[A-z]{3}\\b") %>%
  toupper()
inc_data$derived_Location <- case_when(
  !is.na(inc_data$extracted_Location) ~ inc_data$extracted_Location, TRUE ~ inc_data$Location)
inc_data$derived_Location_source <- case_when(
  !is.na(inc_data$extracted_Location) ~ "short description", TRUE ~ "Location field")
inc_data$derived_Region <- str_sub(inc_data$derived_Location, 1, 2) %>% toupper()

# get rid of "builder" columns
inc_data$title_extracted_BU <- NULL
inc_data$title_extracted_DeviceName <- NULL
inc_data$title_extracted_DeviceName_BU <- NULL
inc_data$extracted_Location <- NULL


## import historical data ----
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


## join INC list with historical data ----
team_summary <- team_history %>%
  group_by(Number) %>%
  summarise(first_team = first(Value, order_by = Start), last_team = last(Value, order_by = Start), team_count = n())
inc_data <- inc_data %>% left_join(team_summary, by="Number")

## FCR calculation for L1
l1_groups <- c("Retail Support", "R10 Hardware Support", "Aloha Support Team", "Aloha Hardware Support")

inc_data_FCR <- inc_data
inc_data_FCR <- inc_data_FCR %>% mutate(RCA_ticket = case_when(str_detect(`Short description`, "(?i)RCA") ~ 1),
                                        L1_origin = case_when(first_team %in% l1_groups ~ 1),
                                        days_to_resolve = as.Date(Resolved) - as.Date(Created),
                                        FCR_phone = case_when(L1_origin == 1 & 
                                                                days_to_resolve == 0 & 
                                                                `Contact type` == "Phone" & 
                                                                (`Assignment group` %in% l1_groups | str_detect(`Short description`, "(?i)RCA")) ~ 1),
                                        FCR_portal = case_when(L1_origin == 1 &
                                                                 (`assignee change counter` <= 1 | is.na(`assignee change counter`)) &
                                                                 str_detect(`Contact type`, "(?i)portal") &
                                                                 (`Assignment group` %in% l1_groups | str_detect(`Short description`, "(?i)RCA")) ~ 1),
                                        FCR = case_when(FCR_phone == 1 | FCR_portal == 1 ~ 1)
                        )
                
# next steps:
## change FCR RCA criteria: it won't be in L1 groups...




# output
write.csv(inc_data_FCR, na = "", row.names = FALSE, "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\L1 metrics\\L1_Analyst_Metrics.csv")
