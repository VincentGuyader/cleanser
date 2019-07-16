#' mod_select_column and mod_select_columnUI  function
#' @param id The id of the current element you are entering
#' @description Shiny Module that helps select columns from data and shows summary statistics.
#' @export
#' @importFrom DT renderDT DTOutput replaceData
#' @examples
#' if (interactive()){
#'   library(shiny)
#'   library(cleanser)
#'   library(readr)
#'   ui <- fluidPage(
#'     titlePanel("Example: mod_select_columnUI"),
#'     selectInput("go","change data",c("boys",
#'                                      "vins",
#'                                      "vins_missing",
#'                                      "one_factor",
#'                                      "right_csv",
#'                                      "right_xls",
#'                                      "demo2_csv",
#'                                      "demo3_xlsx",
#'                                      "demo4_rds"
#'     )),
#'     mod_select_columnUI("truc")
#'   )
#'   
#'   server <- function(input, output,session) {
#'     donnee <- reactive({
#'       
#'       md <- function(x){system.file("dataset",x,package = "cleanser")}
#'       # switch (input$go,
#'       out <- switch (input$go,
#'                      "boys" = readr::read_csv(md("boys.csv")),
#'                      "vins" = readr::read_csv(md("vins.csv")),
#'                      "vins_missing" = readr::read_csv(md("vins_missing.csv"), 
#'                      locale = locale(encoding = 'ISO-8859-1')),
#'                      "one_factor" = readr::read_csv(md("one_factor.csv")),
#'                      "right_csv" = readr::read_csv(md("right.csv")),
#'                      "right_xls" = readxl::read_excel(md("right.xls"),1),
#'                      "demo2_csv" = readr::read_delim(md("demo2.csv"), 
#'                      locale = locale(encoding = 'ASCII'), delim=";"),
#'                      "demo4_rds" = readr::read_rds(md("demo4.rds")),
#'                      "demo3_xlsx" = readxl::read_excel(md("demo3.xlsx"),1)
#'       )
#'       
#'     })
#'     
#'     
#'     callModule(mod_select_column, "truc",data= reactive({
#'       list(df = donnee())
#'     }))
#'   }
#'   #Run the application
#'   shinyApp(ui = ui, server = server)
#' }
mod_select_columnUI <- function(id){
  ns <- NS(id)
  useShinyjs()
  out<-tagList(
    tags$head(
      tags$style(HTML("
                            .table-wrapper {
                            overflow-x:auto;
                            overflow-y:hidden;
                            width: 100%;
                            }
                            
                            "))
    ),
    tags$div(class="row_1", tags$h3("Missing data"),
             HTML(" 
 The barplot on the left hand side shows the amount of missing values in each variable.<br/>  
                  The aggregation plot on the right hand side shows all existing combinations of
                  missing and non-missing values in the dataset observations.<br/>
                  <b><span style=' color:  #649cd5 ;' >Non-missing  values are colored in blue. </span></b><br/>
                  <b><span style=' color:  #ce648c ;'>Missing values are color in red.</span></b> <br/>
                  Additionally, there are horizontal barplots on the right side of the graphic
                  that indicates how frequent are the combinations in the dataset. <br/>
                  "),
             plotOutput(ns("graph_missing"))),
    checkboxInput(ns("sortvars"),value = FALSE,label = "Sort variables by missingness"),
    checkboxInput(ns("remove_full"),value = FALSE,label = "Remove variables without missingness"),
    numericInput(ns("taille_police"),value = 10,min = 1,max = 20,label = "Font size",step = 0.5),
    tags$div(class="table-wrapper", 
             HTML("<h3>Remove columns</h3>
In the following table, users has the possibility to choose which columns he wants to keep for his final output.<br/>
3 bouttons are availables:<br/>
<b> reset </b> leads to come back to the original data set.<br/>
<b> inverse selection </b> leads to select columns that were not selected previously.<br/>
<b> delete columns </b> leads to delete variables.<br/><br/>

                  "),
             DTOutput(ns("tableau"))),
    actionButton(ns("reset"), "Reset"),
    actionButton(ns("deleteCols"), "Delete columns"),
    actionButton(ns("inverseSelect"), "Inverse selection"),
    tags$h3("Summary table for the quantitative variables"),
    tags$div(class="table-wrapper", DTOutput(ns("tableau_skmr_quanti"))),
    tags$h3("Summary table for the qualitative and date variables"),
    tags$div(class="table-wrapper", DTOutput(ns("tableau_skmr_quali"))),
    "dev1"=verbatimTextOutput(ns("log3")),
    "dev2"=verbatimTextOutput(ns("log2")),
    "dev3"=verbatimTextOutput(ns("log")),
    br(),
    column(6,actionButton(ns("valider"),"Validate changes",class="btn-primary",style="display:block;margin:auto;")),
    column(6,actionButton(ns("reset_all"),"Reset changes",class="btn-danger",style="display:block;margin:auto;")),
    br(),
    br()
  )
  if(app_prod()){ 
    out <-   out[! (names(out) %>% str_detect("^dev"))]
  }
  out
}

#' @param input internal
#' @param output internal
#' @param session internal
#' @param data as reactive
#' @import shiny ggplot2
#' @importFrom stats na.omit
#' @importFrom stats cor
#' @importFrom graphics hist
#' @importFrom DT datatable dataTableProxy JS replaceData selectColumns
#' @importFrom tidyr spread
#' @importFrom VIM aggr
#' @importFrom skimr skim kable skim_to_wide
#' @importFrom magrittr '%>%'
#' @importFrom dplyr select filter 
#' @importFrom utils capture.output
#' @export
#' @rdname mod_select_columnUI
mod_select_column <- function(input, output, session, r){

  current <- reactiveValues(data=NULL)
  
  observe({
      current$data <- r$sortie
          })
  
 output$tableau <- renderDT({
    # message("ac")
    datatable(
      current$data, 
      selection = list(target="column"), 
      options = list(scrollX = TRUE,ordering = FALSE,lengthMenu = c( 10,1, 125, 1000),
                     preDrawCallback = JS("function() { 
                                          Shiny.unbindAll(this.api().table().node()); }"),
                     drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
}")
      )
    )
  })
  
  
  proxy <- dataTableProxy('tableau')
  
  output$graph_missing <-  renderPlot({
    req(current$data)
    df <- current$data
    if ( input$remove_full ){
            df <- df %>%  select_if(~any(is.na(.)))
    }
    validate(
      need(ncol(df)!=0, "No variables with missing values")
    )
    void <- capture.output( 
    v <- VIM::aggr(df, col=mdc(1:2),
              numbers=TRUE,
              sortVars=input$sortvars, 
              # labels=names(current$data), 
              cex.axis=input$taille_police/10,
              gap=3,
              oma=c(10,5,5,5),
              ylab=c("Proportion of missingness","Missingness Pattern")))
    v
  })
  
  
  skmr_quantitative <- reactive({
    validate(
      need(!is.null(current$data) , " ")
    )
    df_num <- current$data %>% 
      select_if(is.numeric)
    validate(
      need(ncol(df_num)>0 , " No quantitative variable ")
    ) 
    skimr::skim_to_wide(df_num) %>% 
      dplyr::select(variable, complete, missing, n, mean, sd, p0, p25, p50, p75, p100, hist)
  })

  skmr_qualitative <- reactive({
    validate(
      need(!is.null(current$data) , " ")
    )
    df_quali <- current$data %>% 
      select_if(~{!is.numeric(.)}) # what about Date ?
    
    validate(
      need(ncol(df_quali)>0 , " No qualitative variable ")
    ) 
    
    skimr::skim_to_wide(df_quali) %>% 
      # select(variable, complete, missing, n, n_unique)
      select(everything())
  })
  
  output$tableau_skmr_quanti <- renderDT({
    datatable(options = list(scrollX = TRUE,lengthMenu = c( 10,1, 125, 1000)),
      skmr_quantitative()
    )
  })

  output$tableau_skmr_quali <- renderDT({
    datatable(options = list(scrollX = TRUE,lengthMenu =c( 10,1, 125, 1000)),
      skmr_qualitative()
    )
  })

  observeEvent(input$deleteCols,{
    if (!is.null(input$tableau_columns_selected)) {
      a_virer <- as.numeric(input$tableau_columns_selected) %>%
        setdiff(0) # on ne met pas ZERO
      
      if (length(a_virer) >0){
      current$data <- current$data[,-a_virer]
      }
      }
  })
# 
  observeEvent(input$reset,{
    current$data <- r$sortie
  })

  observeEvent(input$inverseSelect,{
    if (!is.null(input$tableau_columns_selected)) {
      inverse <-  setdiff(seq_along(current$data),as.numeric(input$tableau_columns_selected))
      proxy %>% selectColumns(inverse)
    }
  })

  output$log <- renderPrint({
    current$data[,-as.numeric(input$tableau_columns_selected)]
  })
  output$log2 <- renderPrint({
    input$tableau_columns_selected
  })
  
  observeEvent(r$onglet,{

    ns <- session$ns
    if(!isTRUE(all.equal(r$sortie,current$data)) & r$onglet != "filter"){

      showModal(modalDialog(title = "You have not validated your changes on 'Filter Columns'",
                  footer=tagList(actionButton(ns("ok"),"Validate"),
                  actionButton(ns("reset2"),"Reset"))
                  ))
    }
  })

 observeEvent(input$valider,{
   r$sortie <- current$data
 })
 
 observeEvent(input$ok,{
   removeModal(session = getDefaultReactiveDomain())
   r$sortie <- current$data
 })
 
 observeEvent(input$reset2,{
   removeModal(session = getDefaultReactiveDomain())
   current$data <- r$sortie 
 })
 
 observeEvent(input$reset_all,{
   current$data <-  r$sortie
 })
 
  
}

