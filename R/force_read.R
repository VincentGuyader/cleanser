#' Reads text files in R
#' @param file A file to read in.
#' @param ... Additional argument to pass down to methods.
#' @return A data frame.
#' @description This function is a wrapper around read_delim and read.table.
force_read <- function(file, ...){
  dots <- list(...)
  out <- NULL

  if ( dots$sampled ){
   out <- read_csv_sampled(file = file,
                     skip = dots$skip,
                     nlines = dots$nlines, #-1,
                     header = dots$col_names,
                     fileEncoding = dots$locale$encoding,
                     dec = dots$locale$decimal_mark,
                     sep = dots$delim,
                     size = dots$n_max,
                     stringsAsFactors = TRUE
                     )
  }
  if ( is.null(out)){
       try(out <- read_delim(file = file,
                         skip = dots$skip,
                         col_names = dots$col_names,
                         locale = dots$locale,
                         delim = dots$delim,
                         n_max = dots$n_max
                           ))
  }
  if ( is.null(out)){
  out <-  safe_read.table(file = file,
              skip = dots$skip,
              header = dots$col_names,
              fileEncoding = dots$locale$encoding,
              dec = dots$locale$decimal_mark,
              sep = dots$delim,
              nrows = ifelse(is.infinite(dots$n_max), -1, dots$n_max),
              stringsAsFactors = TRUE
              )
  }
  out
}