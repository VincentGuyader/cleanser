# from htmltools
#' @export
paste8 <- function (..., sep = " ", collapse = NULL) 
{
  args <- c(lapply(list(...), enc2utf8), list(sep = if (is.null(sep)) sep else enc2utf8(sep), 
                                              collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)))
  do.call(paste, args)
}