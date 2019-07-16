lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
rm(list=ls(all.names = TRUE))
.rs.api.documentSaveAll()
devtools::document('.')
devtools::load_all('.')
options(app.prod=FALSE)

if (interactive()){
  library(shiny)
  library(cleanser)
  library(shinyjs)
  library(DT)
  library(readr)
  ui <- fluidPage(
    titlePanel("Example: mod_impute"),
    selectInput("go","change data", c("boys", "avirer",
                                      "vins_missing")
                ),
    mod_imputeInput("pierre")
  )

  server <- function(input, output, session) {
r <- reactiveValues()
    donnee <- reactive({
      md <- function(x){system.file("dataset", x, package = "cleanser")}
      switch (input$go,
              "avirer" = iris
              ,
              "boys" = readr::read_csv(md("boys.csv"))%>% mutate_if(is.character,as.factor),
              "vins_missing" = readr::read_csv(md("vins_missing.csv"), locale = locale(encoding = 'ISO-8859-1'))%>% mutate_if(is.character,as.factor)
      )
    })
    observe({
      r$sortie <- donnee()
    })
    
    callModule(mod_impute,"pierre", r = r)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}


# le multiple marche bien, avec affichage couleur etc, faire pareil pour le simpleCondition(simplifié les methode de stockages aussi)

