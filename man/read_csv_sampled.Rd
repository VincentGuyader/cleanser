% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chunked_sampled.R
\name{read_csv_sampled}
\alias{read_csv_sampled}
\title{Read in a file with randomly sampled lines.}
\usage{
read_csv_sampled(file, size = 100, header = TRUE, sep = ",",
  dec = ".", nlines = NULL, skip = 0, fill = TRUE, ...)
}
\arguments{
\item{file}{The name of the file which the data are to be read from.}

\item{size}{integer Number of rows to import.}

\item{header}{logical A logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.}

\item{sep}{character The field separator character.}

\item{dec}{character The character used in the file for decimal points.}

\item{nlines}{integer Total number of lines in the file}

\item{skip}{integer The number of lines of the data file to skip before beginning to read data.}

\item{...}{Other read.table parameters.}
}
\description{
Read in a file with randomly sampled lines.
}
\examples{


big_iris <- system.file("dataset","big_iris.csv",package = "cleanser")
big_iris2 <- system.file("dataset","big_iris2.csv",package = "cleanser")
guess_separator(big_iris)
guess_separator(big_iris2)
read_csv_sampled(big_iris,size=5)
read_csv_sampled(big_iris2,size=5,sep = ";",dec=",")
}
