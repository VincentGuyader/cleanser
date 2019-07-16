#' Get randomly sampled lines from a file
#' @param filename The name of the file which the data are to be read from.
#' @param n integer The number of lines to sample.
#' @param nlines integer The number of lines in the file.
#' @param header logical Should the header be kept?
#' @param skip integer the number of lines of the data file to skip before beginning to read data.
#' @importFrom LaF get_lines determine_nlines
sample_lines <- function (filename, n, nlines = NULL,
                          header=TRUE, skip=0)
  {
    if (!is.character(filename))
      stop("filename should be a character vector")
    if (!is.numeric(n))
      stop("n should be a number")
    if (!is.null(nlines) && !is.numeric(nlines))
      stop("nlines should be a number")
    if (is.null(nlines)) {
      nlines <- determine_nlines(filename)
    }
    n <- n[1]
    if (n < 0)
      stop("n is negative; you can't sample a negative number of lines")
    if (n < 1)
      n <- round(n * nlines)
    lines <- sample((1 + header + skip):nlines, min(n, nlines-header), replace = FALSE)
    get_lines(filename, lines)
  }

#' Read in a file with randomly sampled lines.
#' @param file The name of the file which the data are to be read from.
#' @param size integer Number of rows to import.
#' @param header logical A logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
#' @param sep character The field separator character. 
#' @param dec character The character used in the file for decimal points.
#' @param nlines integer Total number of lines in the file
#' @param skip integer The number of lines of the data file to skip before beginning to read data.
#' @param ... Other read.table parameters.
#' @importFrom LaF sample_lines
#' @export
#' @examples
#' 
# big_iris <- rbind(iris, iris, iris, iris,iris, iris, iris, iris,iris, iris, iris, iris)
# write_csv(big_iris, "inst/dataset/big_iris.csv")
# write.csv2(big_iris, "inst/dataset/big_iris2.csv",row.names = FALSE)
#' 
#' big_iris <- system.file("dataset","big_iris.csv",package = "cleanser")
#' big_iris2 <- system.file("dataset","big_iris2.csv",package = "cleanser")
#' guess_separator(big_iris)
#' guess_separator(big_iris2)
#' read_csv_sampled(big_iris,size=5)
#' read_csv_sampled(big_iris2,size=5,sep = ";",dec=",")
read_csv_sampled <- function(file, size=100, header = TRUE, sep = ",",
                             dec = ".", nlines=NULL, skip=0,fill=TRUE, ...){
  dots <- list(...)
  if ( is.na(skip)){skip <- 0}
  first_row <- readLines(file,n = 1 + skip)[skip+1]
  tirage <- sample_lines(file, n = min(nlines,size),
                         nlines = nlines,
                         skip = skip,
                         header = header)

  if (header){
  info <- c(first_row, tirage)
    }else{

   info <- tirage
  }

  
  info %>%
    read.table(text =., header = header, sep = sep,
               dec = dec,fill=fill, ...)

}
