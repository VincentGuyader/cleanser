#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny runApp
cleanser <- function() {
  shiny::runApp(system.file("app", package = "cleanser"))
}
