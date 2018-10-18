# Extended Metrics Dervied Columns

#' @title Extended Metrics Dervied Columns
#' @description input snow df & column to keyword search
#' @param df_string df name in string format
#' @param col_string col name in string format
#' @keywords metrics, calculated
#' @export
#' @examples
#' em_derived_attributes()
#' @return outputs the dataframe with new derived columns
#' @aliases em_da
#' @author Chris
#' @seealso test test test
em_derived_attributes <- function(df_string, col_string){
  
  require(dplyr); require(stringr)
  inc_data <- get(df_string)  # get the df object by name
  
  inc_data[[paste0(col_string,"_derived_DeviceName")]] <- inc_data[[col_string]] %>%
    str_extract("(?i)wfm\\s?\\d{5}\\s?[a-z]{3}\\s?\\d{2,3}") %>%
    str_replace_all("\\s", "") %>% toupper()
  
  return(inc_data)
}