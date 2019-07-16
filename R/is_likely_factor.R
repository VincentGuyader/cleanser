#' Signal to the user that a variable is likely a factor
#' @param dataframe A dataframe
#' @param tol The maximum tolerance for this column to be a date. The lower, the stricter.
#' @return A boolean vector.
#' @description This function takes a dataframe as input and returns a boolean vector of positions where
#' it is likely that a column is a factor. For example, a column of only 0's and 1's is likely a factor.
#' @export
#' @importFrom purrr map map_lgl
#' @examples
#' \dontrun{
#' is_likely_factor(dataframe)
#' }
is_likely_factor <- function(dataframe, tol = 5){

  map_dbl(dataframe, ~{
    length(unique(.)) <= tol | is.character(.) | is.factor(.)
    })

}

