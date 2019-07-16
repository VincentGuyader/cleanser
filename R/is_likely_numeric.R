#' Signal to the user that a variable is likely a numeric
#' @param dataframe A dataframe
#' @return A boolean vector.
#' @description This function takes a dataframe as input and returns a boolean vector of positions where
#' it is likely that a column is numeric with decimal. For example, a column of only numbers with decimal is likely a numeric.
#' @export
#' @importFrom purrr map_df map_lgl
#' @examples
#' \dontrun{
#' is_likely_numeric(dataframe)
#' }
is_likely_numeric <- function(dataframe){
  suppressWarnings(
    purrr::map_df(dataframe, to_numeric) %>%
      purrr::map_dbl(~{
        any(!is.na(.))
        })
  )
}
