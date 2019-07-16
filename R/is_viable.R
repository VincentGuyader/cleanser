#' is the object printable 
#' @param x object to test
# is_viable <- function(x){
#   j <- FALSE
# try(  zz <- file(tempfile(), open="wt"))
#   # browser()
#   try( sink(zz,type="message"))
#        try( sink(zz,type="output"))
#    try(j <- print(x))
#         try(close(zz))
#   #border ici
#   # run dev plante
#    try( sink(NULL,type="message"))
#   !setequal(j, FALSE)
# }
is_viable <- function(x){
  j <- FALSE
  sink(tempfile())
  try(j <- print(x))
  sink()
  !setequal(j, FALSE)
}
