#' Converts a column to a date column
#' @param col A data frame column.
#' @return a data frame column in the POSIX, POSIXct or Date format.
#' @description This function transforms a column into a Date if the format is recognizable.
#' @importFrom purrr possibly map_df
#' @importFrom lubridate mdy myd ymd ydm dmy dym ymd_hms
#' @export
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' data(storms)
#' storms <- storms %>% 
#'   mutate(date = paste0(year, month, day))
#'
#' to_date(storms$date)
#' }
to_date <- function(col){
  init <- col
  if (any(class(unlist(col)) == "factor") | any(class(unlist(col)) == "character")){
    col <- unlist(col)
  }

  poss_as_posixit <- purrr::possibly(as.POSIXlt, otherwise = NA_real_) 

  poss_as_posixct <- purrr::possibly(as.POSIXct, otherwise = NA_real_) 

  poss_map_df <- purrr::possibly(function(x){
    map_df(x, ~{class(.)[1]})[1]
  }, otherwise = class(unlist(col))) 

  col_class <- poss_map_df(col)

  if (!(col_class == "POSIXct" | col_class == "POSIX" | col_class == "Date")){
    suppressWarnings({
      possible_date <- data.frame(
        `Month Day Year` = lubridate::mdy(col),
        `Month Year Day` = lubridate::myd(col),
        `Year Month Day` = lubridate::ymd(col),
        `Year Day Month` = lubridate::ydm(col),
        `Day Month Year` = lubridate::dmy(col),
        `Day Year Month` = lubridate::dym(col),
        `Year Month Day Hours` = lubridate::ymd_hms(col),
        `POSIXlt` = poss_as_posixit(col),
        `POSIXct` = poss_as_posixct(col)
      )
    })
    boolean_date <- possible_date %>%
      map_df(~100 * sum(is.na(.)) / nrow(possible_date))
    # [1] if there is an equality we take the first index
    new_col <- possible_date[, which(boolean_date == min(boolean_date))[1]]
    if( class(new_col)[1] %in% c("POSIX", "POSIXct", "POSIXlt")){
      new_col <- init %>% as.factor()
    }
  } else if (col_class=="Date") {
    new_col <- col %>% as.Date(origin="1970-01-01")
  } else{
    new_col <- col
  }
  new_col
}
