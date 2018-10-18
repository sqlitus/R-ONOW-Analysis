# ' Find Trending Issues from Key Words
#'
#' input snow df & column to keyword search
#' @param df_string df name in string format
#' @param col_string col name in string format
#' @keywords metrics
#' @export
#' @examples
#' em_keywords()
em_keywords <- function(df_string, col_string){
  require(dplyr); require(stringr )
  df <- get(df_string)  # get the df object by name
  
  # 3 slightly diff ways
  # df['extract'] <- stringr::str_extract(df[[col_string]], "\\b\\d{5}\\b")  # create column
  # df$title_extracted_BU <- str_extract(df[[col_string]], "\\b\\d{5}\\b")  # second method. change formula $ to [[
  df[paste0(col_string, '_extracted_BU')] <- stringr::str_extract(df[[col_string]], "\\b\\d{5}\\b")
  
  # stringr create columns w/ dplyr
  df <- df %>% mutate(Trending_Issue = case_when(str_detect(eval(as.name(col_string)), "5") ~ "User Account",
                                                 str_detect(eval(as.name(col_string)), "0+") ~ 'zeros'),
                      number = str_extract(eval(as.name(col_string)), "\\d+"))
  
  return(df)
}