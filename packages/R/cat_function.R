# This first line is title I guess




#' here's the line directly above the function. requires 'apos to see
cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}


#' import csv or excel files.
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

# Usage:
# files <- list.files(path, full.names = TRUE)
# OnePOS_Incidents_Import <- import_files(files)


# Package guides:
#### 2018-10-16 - creating a package ----
# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

# library(devtools)
# library(roxygen2)
# getwd()
# normalizePath(getwd())
# paste0(normalizePath(getwd()), "\\packages\\R") # works. cwd+
# document(paste0(normalizePath(getwd()), "\\packages\\R"))  # document the function
# 
# # install("packages")  # first time
# library(packages)
# ?cat_function
# cat_function
# cat_function()