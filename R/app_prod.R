`%||%` <- function (x, y)
{
  if (is.null(x))
    y
  else x
}
#' renvoit `TRUE` si l'app est en prod
app_prod <- function(){
  getOption( "app.prod" ) %||% TRUE
}
