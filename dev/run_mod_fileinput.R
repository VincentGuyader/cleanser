lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
rm(list=ls(all.names = TRUE))
.rs.api.documentSaveAll()
devtools::document('.')
devtools::load_all('.')

if (interactive()) {
  library(shiny)
  library(cleanser)
  library(shinyjs)
  library(DT)
  ui <- function() {
    fluidPage(
      titlePanel("cleanser"),
      useShinyjs(),
      sidebarLayout(
        sidebarPanel(
          mod_all_fileInput("fichier")
        ),
        mainPanel(
          DTOutput("tableau")
        )
      )
    )
  }


  server <- function(input, output,session) {

    r <- reactiveValues()
    callModule(mod_all_file,"fichier",r = r)

    output$tableau <- renderDT({r$sortie})
  }
  shinyApp(ui = ui, server = server)
  }