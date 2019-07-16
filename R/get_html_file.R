#' @importFrom shiny HTML
get_html_file <-function(...) 
{
  file <- system.file(file.path( ...), 
                      package = "cleanser")
  out <- paste(readLines(file), collapse = "\n")
  HTML(out)
}

