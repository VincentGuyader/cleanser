#' Converts a data frame column of characters to numeric
#' @param col A column of characters to convert to numeric
#' @return The column converted into a numeric
#' @description This function takes a string such as "32,486.64 €" and returns the numeric 32486.64
#' @export
#' @examples
#' x <- c("32,343,345.9 euros","12,749.548 euros","32,489.545 euros","143,564.95 euros")
#' to_numeric(x)
to_numeric <- function(col){
if(is.numeric(col)){
  out <- col
}
  if (is.character(col)) {
    out <- as.factor(col)
  }

  if (is.factor(col)) {
    levels(col) <- gsub("^FALSE$", 0, levels(col))
    levels(col) <- gsub("^TRUE$", 1, levels(col))
    levels(col) <- gsub("^F$", 0, levels(col))
    levels(col) <- gsub("^T$", 1, levels(col))
    out <- col
  }

  if (is.logical(col)) {
    # return(as.numeric(col))
    out <- col
  }

  if (class(col)[1]=="POSIXct"|class(col)[1]=="POSIXt"|class(col)[1]=="Date") {
    # return(as.numeric(col))
    out <- as.numeric(col)
  }
  
  rigth_scientific <- function(x){
    x <- gsub("E", "e", x)
    x <- gsub("e+", "e-", x)
    x <- gsub(",", ".", x)
    x
  }
  
  more_points_than <- function(out, dec=","){
    
    out <- gsub(".", "", out)
    if(dec==","){
      out <- gsub(",", ".", out)
    } else if (dec==";"){
      out <- gsub(";", ".", out)
    }
    out
  }

  n_comma <- max(stringr::str_count(col[!is.na(col)], ","), na.rm = TRUE)
  n_point <- max(stringr::str_count(col[!is.na(col)], "[.]"), na.rm = TRUE)
  n_semicol <- max(stringr::str_count(col[!is.na(col)], ";"), na.rm = TRUE)
  
  if(n_comma>0 | n_point>0 | n_semicol>0){
    out <- gsub("[^0-9'.,;]", "", col)
    decimal <- max(nchar(out), na.rm = TRUE)
    
    out <- ifelse(grepl(",..E-",col) | grepl(",.E-",col), rigth_scientific(col),col)
    

  if (n_comma == 1 & n_point > 1) {
    #format like "3.467.945,34"
    out <- gsub(".", "", out)
    out <- gsub(",", ".", out)
    } else if (n_semicol == 1 & n_point > 1) {
    #format like "3.467.945;34"

    out <- gsub(".", "", out)
    out <- gsub(";", ".", out)

  } else if (n_point > 1 ) {
    #format like "3.467.945"
    out <- gsub(".", "", out)
  } else if (n_point == 1 & n_comma > 1 ) {
    #format like "3,454,467.945"
    
    out <- gsub(",", "", out)
    
  } else if (n_point == 1 & n_semicol > 1 ) {
    #format like "3;454;467.945"

    out <- gsub(";", "", out)

  } else if (n_point == 1 & n_semicol == 1 ) {
    #format like "3;454.945"

    l_point <- stringr::str_locate(col, "[.]")[,1]
    l_point <- ifelse(is.na(l_point), 0, l_point)
    
    l_semicolon <- stringr::str_locate(col, ";")[,1]
    l_semicolon <- ifelse(is.na(l_semicolon), 0, l_semicolon)
    
    out <- ifelse((l_point > l_comma) & (l_comma > 0), gsub(",", "", out), out)
    
    out <- ifelse((l_comma > l_point) & (l_point > 0), 
                  more_points_than(out,dec=";"), out)
    
    # if(!(all(is.na(l_point)) & all(is.na(l_semicolon)))){
  # }
    } else if (n_point == 1 & n_comma == 1) {
    #format like "3,454.945"
    #position from the comma and the points
    
    l_point <- stringr::str_locate(col, "[.]")[,1]
    l_point <- ifelse(is.na(l_point), 0, l_point)
    l_comma <- stringr::str_locate(col, ",")[,1]
    l_comma <- ifelse(is.na(l_comma), 0, l_comma)

    out <- ifelse((l_point > l_comma) & (l_comma > 0), gsub(",", "", out), out)
    
    out <- ifelse((l_comma > l_point) & (l_point > 0), 
                  more_points_than(out,dec=","), out)
    

  } else if ( n_comma > 1 ) {
    #format like "3,454,467"
    out <- gsub(",", "", out)
  } else if (n_semicol > 1 ) {
    #format like "3;454;467"
    out <- gsub(";", "", out)
  } else if (n_comma == 1) {
    #format like "454,467"
    out <- gsub(",", ".", out)
  } else if (n_semicol == 1) {
    #format like "454;467"
    out <- gsub(";", ".", out)
    
  }

    # Do in one line and remove all character exept "e-"
    out <- gsub("e-", "-+.+.-", out, perl=TRUE)
    out <- trimws(gsub("[[:alpha:]]", "", out, perl=TRUE))
    out <- gsub("-+.+.-", "e-", out, perl=TRUE)
    out <- gsub("([^0-9.e-])", "", out, perl=TRUE)
    
    dd <-options()[["digits"]]
    on.exit(options(digits = dd))
    options(scipen=999) #disable scientific notation
    options(digits = min(decimal,22))
    out <- as.numeric(as.character(out))
  }

  as.numeric(as.character(out))

}

