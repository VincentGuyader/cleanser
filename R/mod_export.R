#' @title Data export module
#' @description  A shiny Module that exports data to several formats
#' @param id shiny id
#' @param label file input label
#' @export
#' @examples 
#' if (interactive()) {
#' library(shiny)
#' library(cleanser)
#' library(shinyjs)
#' library(DT)
#' ui <- function() {
#'   fluidPage(
#'     titlePanel("Export module"),
#'     useShinyjs(), 
#'     sidebarLayout(
#'       sidebarPanel(
#'         mod_exportUI("export")
#'       ),
#'       mainPanel(
#'         DTOutput("tableau")
#'       )
#'     )
#'   )
#' }
#' 
#' 
#' server <- function(input, output,session) {
#'     
#'     reactive_iris <- reactive({
#'       iris
#'     })
#'     
#'   callModule(mod_export, "export", reactive_iris)
#'   
#'   output$tableau <- renderDT({
#'   iris
#'   })
#' }
#' shinyApp(ui = ui, server = server)
#' }
mod_exportUI <- function(id, label = "Upload your file") {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        selectInput(ns("extension"),label = "File extension", choices = 
                      c("rds",
                        "csv",
                        "xlsx"
                      )),
        selectInput(ns("separator"),label = "Separator",choices = 
                      c("Comma" = ",",
                        "Semicolon" = ";",
                        "Tab" = "\t",
                        "Space" =  " ",
                        "Pipe" = "|"
                      )),
        downloadButton(ns('downloadData'),'Save to file')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        DTOutput(ns("tableau"))
      )
    )
  )}

#' @param input For internal purposes
#' @param output For internal purposes
#' @param session The current session. Used internally.
#' @param data The data to export
#' @importFrom rio export
#' @importFrom data.table fwrite
#' @importFrom writexl write_xlsx
#' @export
#' @rdname mod_exportUI
mod_export <- function(input, output, session, r) {
  
  
  current <- reactiveValues(input = NULL)
  # dataset <- reactive({ data() })
  observe({
      current$input <- r$sortie
  })
  
  
  

  
  output$tableau <- renderDT({
    req(current$input)
    datatable(
      current$input,
      escape = FALSE,  
      selection = list(target="column"), 
      options = list(scrollX = TRUE,
                     lengthMenu = c( 10,1, 125, 1000),
                     ordering = FALSE,
                     preDrawCallback = JS("function() { 
                                          Shiny.unbindAll(this.api().table().node()); }"),
                     drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
}")
      )
    )
})
  observe({
    if (input$extension == "csv" ){
      show("separator")
    } else{ hide("separator") }
  })
  # Downloadable csv of selected dataset ——
  output$downloadData <- downloadHandler(
    filename <- function() {
      dataName <- paste0("dataset-", Sys.Date())
      
      paste0(dataName, ".", input$extension)
    },
    content <- function(file) {
      if(input$extension == "csv"){
        data.table::fwrite(x = current$input,file = file,sep = input$separator)
        # rio::export(x = mydata(), file = file, format = input$separator )
      } else if(input$extension == "xlsx"){
        writexl::write_xlsx(x = current$input,path=file)
      } else {
        rio::export(x = current$input, file = file)
        }
    }
  )
  

  
  # dataset
  
  }