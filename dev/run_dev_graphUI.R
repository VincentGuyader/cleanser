lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
rm(list=ls(all.names = TRUE))
.rs.api.documentSaveAll()
devtools::document('.')
devtools::load_all('.')
options(app.prod=FALSE)


if (interactive()){
library(shiny)
library(cleanser)
library(readr)
library(DT)
ui <- fluidPage(
titlePanel("Example: mod_graphUI"),
selectInput("go","change data",c("boys",
                                 "vins",
                                 "vins_missing",
                                 "one_factor",
                                 "right_csv",
                                 "right_xls",
                                 "demo2_csv",
                                 "demo3_xlsx",
                                 "demo4_rds"
                                 )),
mod_graphUI("truc2"),
tags$div(class="table-wrapper", DTOutput("tableau"))
)

server <- function(input, output,session) {

    
    donnee <- reactive({

    md <- function(x){system.file("dataset",x,package = "cleanser")}
  switch (input$go,
    "boys" = readr::read_csv(md("boys.csv")),# %>% dplyr::mutate_all(as.factor),
    "vins" = readr::read_csv(md("vins.csv")),
    "vins_missing" = readr::read_csv(md("vins_missing.csv"), locale = locale(encoding = 'ISO-8859-1')),
    "one_factor" = readr::read_csv(md("one_factor.csv")),
    "right_csv" = readr::read_csv(md("right.csv")),
    "right_xls" = readxl::read_excel(md("right.xls"),1),
    "demo2_csv" = readr::read_delim(md("demo2.csv"), locale = locale(encoding = 'ASCII'), delim=";"),
    "demo4_rds" = readr::read_rds(md("demo4.rds")),
    "demo3_xlsx" = readxl::read_excel(md("demo3.xlsx"),1)
  )
})

    output$tableau <- renderDT({
      
      datatable(
        donnee(), 
        selection = list(target="column")

      )
})
  
data_selected <-reactive({
  donnee() %>%
    select(
  as.numeric(input$tableau_columns_selected) %>%
    setdiff(0)
  )

})


  callModule(mod_graph, "truc2",
             data = data_selected
             )

}
# Run the application
shinyApp(ui = ui, server = server)
}
