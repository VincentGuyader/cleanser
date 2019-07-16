#' mod_format and mod_formatUI  function
#' @param id The id of the current element you are entering
#' @description Shiny Module that allows to change the variable types: qualitative, quantitative or date.
#' @export
#' @importFrom DT renderDT DTOutput replaceData
#' @importFrom shinyjs runjs
#' @examples
#' if (interactive()){
#'   library(shiny)
#'   library(cleanser)
#'   library(readr)
#'   ui <- fluidPage(
#'     titlePanel("Example: mod_FormatUI"),
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
#'     mod_formatUI("truc2")
#'   )
#'   
#'   server <- function(input, output,session) {
#'     
#'     donnee <- reactive({
#'       
#'       md <- function(x){system.file("dataset",x,package = "cleanser")}
#'       switch (input$go,
#'               "boys" = readr::read_csv(md("boys.csv")),
#'               "vins" = readr::read_csv(md("vins.csv")),
#'               "vins_missing" = readr::read_csv(md("vins_missing.csv"), 
#'               locale = locale(encoding = 'ISO-8859-1')),
#'               "one_factor" = readr::read_csv(md("one_factor.csv")),
#'               "right_csv" = readr::read_csv(md("right.csv")),
#'               "right_xls" = readxl::read_excel(md("right.xls"),1),
#'               "demo2_csv" = readr::read_delim(md("demo2.csv"), 
#'               locale = locale(encoding = 'ASCII'), delim=";"),
#'               "demo4_rds" = readr::read_rds(md("demo4.rds")),
#'               "demo3_xlsx" = readxl::read_excel(md("demo3.xlsx"),1)
#'       )
#'     })
#'     
#'     callModule(mod_format, "truc2", reactive({donnee()}))
#'   }
#'   # Run the application
#'   shinyApp(ui = ui, server = server)
#' }
mod_formatUI <- function(id){
  ns <- NS(id)
  tagList(useShinyjs(),
          tags$head(
            tags$style(HTML("
                            .table-wrapper {
                            overflow-x:auto;
                            overflow-y:hidden;
                            width: 100%;
                            }
                            
                            "))
            ),
          tags$div(class="table-wrapper",
                   DTOutput(ns("tableau")),
          shinyjs::hidden(actionButton(inputId = ns("reset_var"),
                                       icon = icon(name = "ban"),
                                       label = "Unselect all variables"))),
          verbatimTextOutput(ns("log2")),
          verbatimTextOutput(ns("log3")),
          verbatimTextOutput(ns("log")),
          br(),
          div(actionButton(ns("valider"),"Valider les changements",class="btn-primary",style="display:block;margin:auto;")),
          br()
            )[
              if(app_prod()){ c(1, 2, 3, 4) }else{ c(1, 2, 3, 4, 5, 6,7)}
              ]
  }




#' @param input internal
#' @param output internal
#' @param session internal
#' @param r dataset as reactive
#'
#' @import shiny ggplot2 dplyr vcd corrplot
#' @importFrom stats na.omit cor
#' @importFrom DT datatable dataTableProxy JS replaceData
#' @importFrom purrr map map_df
#' @export
#' @rdname mod_formatUI
mod_format <- function(input, output, session, r){
  
  gen_selectinput <- function(df, class, indic, session){
    map(1:ncol(df),
        function(i) {
          if ( class[i] == "numeric" | class[i]=="integer") {
            opt1 <- "Numeric variable"
            opt2 <- "Categorical variable"
            opt3 <- "Date variable"
          }  else if ( class[i] == "Date variable" | class[i] == "POSIXct"| class[i] == "POSIXlt"| class[i] == "POSIX" ) { 
            opt1 <- "Date variable"
            opt2 <- "Numeric variable"
            opt3 <- "Categorical variable"
          }else{  
            opt1 <- "Categorical variable"
            opt2 <- "Numeric variable"
            opt3 <- "Date variable"
          }
          selectInput(
            inputId = session$ns(paste0(paste0("col",indic), i)),
            label = NULL, selected = opt1,
            choices = c(opt1, opt2, opt3))
          
          
          
        }
    )
    
  }
  flag <- reactiveValues(indic = "",
                         typage_out = NULL,
                         typage_in = NULL
  )
  
  current <- reactiveValues(data=NULL, init=NULL)
  
  mydata <- reactive({
    df <- r$sortie
    current$data <- r$sortie
    current$init <- r$sortie
    df %>% mutate_if(is.character,as.factor)
  })
  
  updateData <- reactive({
    validate(
      need(!is.null(mydata())  & !is.null(unlist(mydata())), " ")
    )
    df <- mydata()
    
    class <- isolate( flag$typage_in )
    if ( !is.null(current$data)) {
      df <-  current$data
      if(!app_prod()){
        message( "flag: ", input[[paste0(paste0("col",flag$indic), 3)]])
      }
      map(1:ncol(df), function(i){
        
        id <- paste0(session$ns(''),'col',flag$indic,i)
        
        if (length(input[[paste0(paste0("col",flag$indic), i)]]) > 0) {
          if (input[[paste0(paste0("col",flag$indic), i)]] == "Numeric variable") {
            
            
            shinyjs::runjs(
              #background
              glue::glue("document.getElementById('{id}').style.color ='{color}'",
                         color="#fc6d26"
              )
            )
            
            
            
            
            df[,i] <<- unlist(current$init[,i]) %>% to_numeric()
          } else if (input[[paste0(paste0("col",flag$indic), i)]] == "Categorical variable") {
            shinyjs::runjs(
              #background
              glue::glue("document.getElementById('{id}').style.color ='{color}'",
                         color="#2aa198"
              )
            )
            
            df[,i] <<- unlist(current$init[,i]) %>% as.factor
          }else if (input[[paste0(paste0("col",flag$indic), i)]] == "Date variable") {
            
            shinyjs::runjs(
              #background
              glue::glue("document.getElementById('{id}').style.color ='{color}'",
                         color="#d33682"
              )
            )
            
            if(map_df(current$init, ~{class(.)[1]})[i] %in% c("POSIX", "POSIXct", "POSIXlt") ){
              df[,i] <<- current$init[,i] #%>% to_date()
              # df[,i] <<- unlist(current$init[,i]) %>% as.factor
            } else if(map_df(current$init, ~{class(.)[1]})[i] =="Date" ){
              df[,i] <<- unlist(current$init[,i]) %>% as.Date(origin="1970-01-01")
            } else{ df[,i] <<- unlist(current$init[,i]) %>% to_date() }
          }
        }
      })}
    current$data <- df
    df
  })
  
  output$log3 <- renderPrint({
    reactiveValuesToList(input)
    
  })
  
  output$tableau <- renderDT({
    validate(
      need(!is.null(mydata())  & !is.null(unlist(mydata())), "Check that all input are selected on your left")
    )
    if(!app_prod()){
      message("mise a jour flag")
    }
    flag$indic <- as.character(rnorm(1))
    df <-  mydata()
    class <- map_df(df, ~{class(.)[1]})
    
    flag$typage_in <- class
    
    if(!app_prod()){
      message(flag$indic)
    }
    tableSelectInput <- gen_selectinput(df = df,
                                        class = class,
                                        indic = flag$indic,
                                        session = session)
    
    l <-  length(tableSelectInput)
    type_cat <- selectin  <-  seq_len(l)
    
    for (i in 1:l) {
      selectin[i] <- as.character(tableSelectInput[[i]])
      pos <- gregexpr("selected>",selectin[i])[[1]][1]
      type_cat[i] <- substr(selectin[i], (pos + 9), (pos + 11))
    }
    
    colors_types <- likely_types(df)
    
    col_names <- paste0( '<font color = "', colors_types, '">',colnames(df), "</font><br/>", selectin)
    
    datatable(isolate(
      updateData()), selection = list(target="column"), 
      options = list(ordering = FALSE, lengthMenu = c(3, 10,1, 125, 1000),
                     # scrollX = TRUE, # casse le code !
                     initComplete = JS("function() {
                                       $('.colors').remove();
                                       $(document).ready(function() {if ( $('.table-wrapper').height()>0 && $('body > div > div > ul > li:nth-child(1)').attr('class')==='active'){
                                       $( '.well' ).after('<div class=\\042 colors \\042 ><p> The colors help you decide which type to attribute to a variable.</p><p style=\\042 color:  #2aa198 ;\\042 ><b>Green</b> indicates that a variable is likely a Categorical variable. </p><p style=\\042 color: #d33682 ;\\042> <b>Pink</b> indicates that a variable is likely a Date variable.</p><p style=\\042 color:  #fc6d26 ;\\042><b>Orange</b> indicates that a variable is likely a Numeric variable.</p></div>' )}
                                       else { $('.colors').hide();
                                       }})}"),
                     preDrawCallback = JS("function() { 
                                          Shiny.unbindAll(this.api().table().node()); }"),
                     drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
}")
      ),
      colnames = col_names, 
      escape = FALSE
      )
    }
    )
  
  proxy <- dataTableProxy('tableau')
  
  observe({
    replaceData(proxy, updateData(), resetPaging = TRUE )
  })
  
  output$log <- renderPrint({
    updateData()
  })
  output$log2 <- renderPrint({
    class <- map_df(updateData(), ~{class(.)[1]})
  })
  
  observeEvent(input$reset_var,{
    if (!app_prod()){
      
      message("reset selection")
    }
    
    DT::selectColumns(proxy = proxy,selected = NULL)
  })
  observeEvent(updateData(),{
    shinyjs::show("reset_var")
    
  })
  
  sortie <- reactive({
    list(df = updateData(),
         selected = as.numeric(input$tableau_columns_selected))
  })
  
  observeEvent(input$valider,{
    r$sortie <- sortie()
  })
  }




