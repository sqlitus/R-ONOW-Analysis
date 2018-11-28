# Open Volume Over Time - SC TASKS - HISTORICAL BACKLOG REPORT #
# Using SC TASKS Team History & State History to find open queue volumes

# conditionally install packages if not already installed, then load them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

start_time <- Sys.time()
print(paste("Starting:", start_time))

# import all appropriately named files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\GHD SC Task History"
sc_task_history_files <- list.files(path, pattern = "(?i)sc_task", full.names = TRUE)  # !!! rename after multiple files used


ImportAndMergeFilesThenFilterOutDuplicates <- function(files){
  df <- data_frame()
  for (i in 1:length(files)){
    data <- readxl::read_excel(files[i])
    df <- bind_rows(df, data)
  }
  df <- df %>% distinct()
  filter_out <- df %>% filter(Start == End)
  df <- df %>% anti_join(filter_out)
  return(df)
}

team_history <- ImportAndMergeFilesThenFilterOutDuplicates(sc_task_history_files)

# set TZ of imported times to CST
# team_history[c('Start','End', 'Closed')] <- force_tz(team_history[c('Start','End')], tzone = 'US/Central')

# get all distinct tasks from dataset(s)
distinct_tasks <- team_history %>% select(Number) %>% distinct()


OpenVolumeTicketListByDay <- function(history_df, start_x_weeks_ago){
  #  history df requires Start, End, Closed dates
  
  calendar_start <- (Sys.Date() - (7*start_x_weeks_ago)) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
  calendar <- data_frame(
    date = seq.Date(from = calendar_start, to = today(), by = "days"),
    datetime = seq.POSIXt(from = as.POSIXct(paste(calendar_start, "08"), tz = "GMT", format = "%Y-%m-%d %H"), 
                          to = as.POSIXct(today()+1), 
                          by = "DSTday")
  )
  ovot <- data_frame()
  for (i in 1:nrow(calendar)){
    insert_day <- distinct_tasks %>% 
      mutate(datetime = calendar$datetime[i], date = calendar$date[i]) %>% 
      select(-Number, Number) %>%   # reorder columns w/ dates first then number
      left_join(history_df, by = "Number") %>%
      filter(Start <= calendar$datetime[i] & 
               (calendar$datetime[i] < End | 
                  is.na(End) & calendar$datetime[i] < Closed |
                  is.na(End) & is.na(Closed)))
    ovot <- bind_rows(ovot, insert_day)
  }
  return(ovot)
}

ovot <- OpenVolumeTicketListByDay(team_history, 14)


# output
writeLines(paste("Exporting file now at", Sys.time(),"\n Elapsed time:", round(difftime(Sys.time(),start_time, units='secs'),2)))
writexl::write_xlsx(x = ovot, path = paste0(path, "\\ovot_GHD_tasks.xlsx"))  # switch to this
writeLines(paste0("DONE. \nStart time: ", start_time,
                  "\nEnd time: ", Sys.time(),
                  "\nElapsed time: ", round(difftime(Sys.time(),start_time, units='secs'),2), " seconds."))

