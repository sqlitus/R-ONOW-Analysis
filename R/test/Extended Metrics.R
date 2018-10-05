# Extended Metrics for INC List
library(tidyverse); library(lubridate); library(tools)

## import ticket list ----
library(tidyverse)
OnePOS_Incidents_Import <- readxl::read_excel(path = "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\incident.xlsx")
inc_data <- OnePOS_Incidents_Import

#### TODO: check file type. Rename columns if CSV... 
import_files <- function(files){
  out <- data_frame()
  for (i in 1:length(files)){
    if (file_ext(files[i]) == "xlsx") { data <- readxl::read_excel(files[i])
    } else if (file_ext(files[i]) == "csv") { data <- read_csv(files[i])  # make change: convert columns on import
    } else { break }
    data$import_sheet <- str_extract(files[i], "(?<=/).*") # positive lookbehind
    out <- bind_rows(out, data)
  }
  ## add distinct('everything-but-import-sheet') line
  return(out)
}

path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\textsearch file\\"
files <- list.files(path, full.names = TRUE)
OnePOS_Incidents_Import <- import_files(files)
inc_data <- OnePOS_Incidents_Import



## EXCEL FILE calculated columns ----
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

## more calc columns...
inc_data <- inc_data %>% mutate(free_groceries = 
                                  case_when(str_detect(`Short description`, "(?i)(free\\W{0,2}groceries)|(store\\W{0,2}charge)") ~ 1),
                                free_groceries_2 =
                                  case_when(str_detect(`Short description`, "(?i)free\\W{0,2}?groceries") ~ 1,
                                            str_detect(`Short description`, "(?i)free\\W{0,2}?(groceries|money|charge|tender)") ~ 1,
                                            str_detect(`Short description`, "(?i)store\\W{0,2}?(groceries|money|charge|tender)") ~ 1),
                                free_groceries_3 = 
                                  case_when(str_detect(`Short description`, "(?i)lane\\W{0,2}?(down|in|off)") ~ 1)
                                )


inc_data %>% filter(free_groceries_2 == 1) %>% View()
inc_data %>% filter(free_groceries_3 == 1) %>% View()






#### CSV CALCULATED COLUMNS ----
inc_data <- OnePOS_Incidents_Import
inc_data <- inc_data %>% mutate(all_text_fields = paste(short_description, description, user_input, work_notes_list, work_notes,
                                                        sep = "\n=============\n=============\n"))

# julie find words
inc_data <- inc_data %>% mutate(free_groceries_sd =
                                  case_when(str_detect(`short_description`, "(?i)free\\W{0,2}?groceries") ~ 1,
                                            str_detect(`short_description`, "(?i)free\\W{0,2}?(groceries|money|charge|tender)") ~ 1,
                                            str_detect(`short_description`, "(?i)store\\W{0,2}?(money|charge|tender)") ~ 1),
                                free_groceries_d =
                                  case_when(str_detect(`description`, "(?i)free\\W{0,2}?groceries") ~ 1,
                                            str_detect(`description`, "(?i)free\\W{0,2}?(groceries|money|charge|tender)") ~ 1,
                                            str_detect(`description`, "(?i)store\\W{0,2}?(money|charge|tender)") ~ 1),
                                free_groceries_wn =
                                  case_when(str_detect(`work_notes`, "(?i)free\\W{0,2}?groceries") ~ 1,
                                            str_detect(`work_notes`, "(?i)free\\W{0,2}?(groceries|money|charge|tender)") ~ 1,
                                            str_detect(`work_notes`, "(?i)store\\W{0,2}?(money|charge|tender)") ~ 1),
                                free_groceries_flag =
                                  case_when(free_groceries_sd == 1 | free_groceries_d == 1 | free_groceries_wn == 1 ~ 1),
                                free_groceries_found_in =
                                  case_when(free_groceries_sd == 1 ~ 'short description',
                                            free_groceries_d == 1 ~ 'description',
                                            free_groceries_wn == 1 ~ 'work notes'),
                                free_groceries_found_line =
                                  case_when(free_groceries_sd == 1 ~ str_extract(short_description, "(?i).{0,20}(?i)free\\W{0,2}?(groceries|money|charge|tender).{0,20}"),
                                            free_groceries_d == 1 ~ str_extract(description, "(?i).{0,20}(?i)free\\W{0,2}?(groceries|money|charge|tender).{0,20}"),
                                            free_groceries_wn == 1 ~ str_extract(work_notes, "(?i).{0,20}(?i)free\\W{0,2}?(groceries|money|charge|tender).{0,20}")
                                  )
                                )

inc_data %>% filter(free_groceries_2 == 1) %>% View()
inc_data %>% filter(free_groceries_flag == 1) %>% View()

out <- inc_data %>% filter(free_groceries_flag == 1)
out <- out %>% mutate(julie_string = case_when(free_groceries_sd == 1 ~ str_extract(short_description, "(?i).{0,20}?(?i)free\\W{0,2}?(groceries|money|charge|tender).{0,20}?"),
                        free_groceries_d == 1 ~ str_extract(description, "(?i).{0,44}(?i)free\\W{0,2}?(groceries|money|charge|tender).{0,111}"),
                        free_groceries_wn == 1 ~ str_extract(work_notes, "(?i).{0,44}(?i)free\\W{0,2}?(groceries|money|charge|tender).{0,111}")),
                      julie2 = str_extract(all_text_fields, "(?i).{0,221}store\\W{0,2}?(money|charge|tender).{0,221}")
                      )

test <- out

out <- out %>% select(number, short_description, description, work_notes, mention = free_groceries_found_in, 
                      details = julie2, location, category, priority)


writexl::write_xlsx(out, "C:\\Users\\chris.jabr\\Downloads\\julie.xlsx")
write_csv(out, "C:\\Users\\chris.jabr\\Downloads\\julie.csv")
