#' @importFrom vcd assocstats
#' 
#' 



get.V <- function(df) {
  req(!is.null(df))
  cat_var <- df %>%
    map_df(~(is.character(.)|is.factor(.))) %>%
    select_if(~. == TRUE)
  
  y <- df %>% select(colnames(cat_var))
  
  
  col.y <- ncol(y)
  V <- matrix(ncol = col.y, nrow = col.y)
  for (i in 1:col.y) {
    for (j in 1:col.y) {
      V[i,j] <- assocstats(table(unlist(y[,i]), unlist(y[,j])))$cramer
    }
  }
  rownames(V) <- colnames(y)
  colnames(V) <- colnames(y)
  return(V)
}


get_quanti <- function(df){
  if(!is.null(df)){
    quant_var <- df %>%
      map_df(~!(is.numeric.Date(.)|is.numeric.POSIXt(.) |is.character(.)|is.factor(.))) %>%
      select_if(~. == TRUE)
    
    y <- df %>% select(colnames(quant_var))}
  else{y=NULL}
  y
}


get_quali <- function(df){
  if(!is.null(df)){
    quali_var <- df %>%
      map_df(~(
        is.character(.)|is.factor(.))) %>%
      select_if(~. == TRUE)
    
    y <- df %>% select(colnames(quali_var))}
  else{y=NULL}
  y
}