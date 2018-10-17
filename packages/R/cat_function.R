# This first line is title I guess




#' here's the line directly above the cat_function. requires 'apos to see
cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}



# ' this is the EM function. can't see its documentation somehow.
em_func <- function(df_string, col_string){
  require(dplyr); require(stringr )
  df <- get(df_string)  # get the df object by name
  
  df['extract'] <- stringr::str_extract(df[[col_string]], "\\b\\d{5}\\b")  # create column
  df$title_extracted_BU <- str_extract(df[[col_string]], "\\b\\d{5}\\b")  # second method. change formula $ to [[
  df[paste0(col_string, '_extracted_BU')] <- stringr::str_extract(df[[col_string]], "\\b\\d{5}\\b")
  return(df)
}










# Usage:
# files <- list.files(path, full.names = TRUE)
# OnePOS_Incidents_Import <- import_files(files)







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