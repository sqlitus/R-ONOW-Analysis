# Mean Time to Response (MTTR) & Mean Time Restore Service (MTRS) Analysis
# Combining team pass history & assignment pass history

library(tidyverse); library(lubridate); library(tools)
start_time <- Sys.time()
print(paste("Starting:", start_time))

# import all appropriately named files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
team_history_files <- list.files(path, "(?i)team_hist", full.names = TRUE)
assign_history_files <- list.files(path, "(?i)assign_hist", full.names = TRUE)


team_history <- list.files(path = path, pattern = "(?i)team_hist.*", full.names = TRUE) %>% map_df(readxl::read_excel)
team_history2 <- team_history_files %>% map_df(readxl::read_excel)

import_files <- function(files){
  out <- data_frame()
  for (i in 1:length(files)){
    if (file_ext(files[i]) == "xlsx") { data <- readxl::read_excel(files[i])
    } else if (file_ext(files[i]) == "csv") { data <- read.csv(files[i])
    } else { break }
    data$import_sheet <- str_extract(files[i], "(?<=/).*") # positive lookbehind
    out <- bind_rows(team_history, data)
  }
  return(out)
}

test <- import_files(team_history_files)


## why are team history files & test different row counts???