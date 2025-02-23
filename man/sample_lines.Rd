% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chunked_sampled.R
\name{sample_lines}
\alias{sample_lines}
\title{Get randomly sampled lines from a file}
\usage{
sample_lines(filename, n, nlines = NULL, header = TRUE, skip = 0)
}
\arguments{
\item{filename}{The name of the file which the data are to be read from.}

\item{n}{integer The number of lines to sample.}

\item{nlines}{integer The number of lines in the file.}

\item{header}{logical Should the header be kept?}

\item{skip}{integer the number of lines of the data file to skip before beginning to read data.}
}
\description{
Get randomly sampled lines from a file
}
