#' Is a data frame column likely a date?
#' @param col A data frame column.
#' @param tol The maximum probability tolerated for this column to be a date. The lower, the stricter.
#' @return TRUE or FALSE.
#' @description This function determines, with a certain tolerance, whether a column is likely a date.
#' @export
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' data(storms)
#' storms <- storms %>% 
#'   mutate(date = paste(year, month, day, sep = "/"))
#'
#' is_likely_date_col(storms$date)
#' }
is_likely_date_col <- function(col, tol = 10){
  if (any(class(unlist(col)) == "factor")){
    col <- unlist(col)
  }
  if (all(!is.na(col)) & all(as.character(col) ==
                             gsub("[^[:digit:]]", "", col))) {
    col <- as.numeric(as.character(col))
  }
  poss_as_posixit <- possibly(as.POSIXlt, otherwise = NA_real_)

  poss_as_posixct <- possibly(as.POSIXct, otherwise = NA_real_)

  if (!(any(class(col) == "POSIXct") | any(class(col) == "POSIX"))){

    suppressWarnings({
      possible_date <- data.frame(
        `Month Day Year` = lubridate::mdy(col),
        `Month Year Day` = lubridate::myd(col),
        `Year Month Day` = lubridate::ymd(col),
        `Year Day Month` = lubridate::ydm(col),
        `Day Month Year` = lubridate::dmy(col),
        `Day Year Month` = lubridate::dym(col),
        `POSIXlt` = poss_as_posixit(col),
        `POSIXct` = poss_as_posixct(col)
      )
    })

    boolean_date <- possible_date %>%
      purrr::map_df(~100 * sum(is.na(.)) / nrow(possible_date))

    is_missing <- data.frame(
      format = colnames(boolean_date),
      missing_pct = unlist(boolean_date),
      row.names = NULL, stringsAsFactors = FALSE) %>%
      arrange(missing_pct)
    
    is_date <- is_missing %>%
      filter(missing_pct < tol)

    if (nrow(is_date) > 0) {
      is_date <- TRUE
      } else {
        is_date <- FALSE
        }
    } else {
    is_date <- TRUE
    }
  is_date
}

#' Signal to the user that a variable is likely a date
#' @param dataframe A dataframe.
#' @param tol The maximum probability tolerated for this column to be a date. The lower, the stricter.
#' @return A boolean vector. 
#' @description This function takes a dataframe as input and returns a boolean vector of positions where
#' it is likely that a column is a date. 
#' @export
#' @examples
#' \dontrun{
#' library(tidyverse)
#' data(storms)
#' storms <- storms %>% 
#'   mutate(date = paste(year, month, day, sep = "/"))
#'
#' is_likely_date(storms)
#' }
is_likely_date <- function(dataframe, tol = 5){
  purrr::map_dbl(.x = dataframe, .f = ~cleanser::is_likely_date_col(., tol))
}
