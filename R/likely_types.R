#' Signal to the user that variables are likely of the wrong type
#' @param dataframe A dataframe
#' @param tol Maximum number of unique values in a factor
#' @return A vector of html colors, one color per type
#' @description This function takes a dataframe as input and returns a character vector of html
#' colors.
#' @importFrom dplyr case_when
#' @export
#' @examples
#' \dontrun{
#' storms = storms %>%
#'   mutate(date = paste(year, month, day, sep = "/"))
#' is_likely_date(storms)
#' }
likely_types <- function(dataframe, tol = 5){

  likely_types_df <- tibble("rownames" = colnames(dataframe),
                            "likely_date" = is_likely_date(dataframe, tol),
                            "likely_factor" = is_likely_factor(dataframe, tol),
                            "likely_numeric" = is_likely_numeric(dataframe),
                            "unique_values" = lapply(dataframe , function(x){
                              length(unique(x))}) %>% unlist,
                            "char_values" = lapply(dataframe, function(x){
                              lchar <- nchar(unique(gsub("[[:digit:].]","", x)))
                              length(lchar[lchar>1 & !is.na(lchar)])/length(x)
                              # mean(nchar(unique(gsub("[[:digit:]]","",x))
                            })
                            
                            # df <- readr::read_csv2("/Users/user/Downloads/UC2_exoP_catalog.csv")
                            
  )
  
  # If a variable is likely date, it is a date regardless of other types 
  # If a variable is likely *only* a numeric, it is likely a numeric
  # If a variable is likely a factor, but not likely a date, it is a factor, whether it is likely or not a numeric
  
  likely_types_df %>%
    mutate(color = case_when(
      likely_date == 1 ~ "d33682", # is a date
      (likely_date == 0 & likely_factor == 0) | (likely_date == 0 & likely_numeric==1 & unique_values > tol & char_values < 0.025) ~ "fc6d26 ", # is a numeric
      (likely_date == 0 & likely_factor == 1 & likely_numeric==1 & unique_values <= tol) | (likely_date == 0 & likely_factor == 1 & likely_numeric==0) | char_values >= 0.025 ~ "2aa198"  # is a factor
    )) %>%
    pull(color)
  
}

