# a sortir
#' @importFrom R.utils withTimeout 
read.table_with_timeout <- function(...){
  out <- suppressWarnings( withTimeout(read.table(...), timeout = 1, substitute = FALSE))
  try(incProgress(amount = 1, message = glue("enc : {list(...)$fileEncoding} ({list(...)$sep }) -")), silent=TRUE)
  out
  }