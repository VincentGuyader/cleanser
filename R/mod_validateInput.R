#' mod_validateInput UI function
#' @param id The id of the current element you are entering
#' @export
#' @examples
#' if (interactive()) {
#' library(cleanser)
#' ui <- shinyUI(
#'   fluidPage(
#'     titlePanel("Validation module"),
#'     mod_validateInput("val_mod")
#'     
#'   ))
#' 
#' server <- function(input, output,session) {
#'   data <- reactive({
#'     boys
#'   })
#'   callModule(mod_validate,"val_mod", reactive(data()))
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)
#' 
#'}
mod_validateInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$script(
     paste0('Shiny.addCustomMessageHandler("refocus",
      function(NULL) {
      document.getElementById("', ns("rules"), '").focus();
      });')
    ),
    # tags$div(id="wait",conditionalPanel(condition = paste0("$('html').hasClass('shiny-busy')"),
    #                                     tags$div(id ="contentLoad", tags$img( id = "loading", src = "www/loading.gif")
    #                                     )
    # 
    # )),
    
    tags$div(
      id = "active",
      # conditionalPanel(
        # condition = paste0("!$('html').hasClass('shiny-busy')"),
        
        HTML(
          "
          
          <h4>Data validation</h4>
          <div class='col-sm-6'>
          <p>This tab allows you to quickly validate your data using simple rules. You can enter rules in the text area below. Write one rule per line. <br>
          Suppose that the name of the column that you want to check is <i>weigth</i> and that you want to highlight all rows where <i>weight</i>.is greater than 5.
          This can be done like so:</p>
          
          <div style = 'background-color: #fcc; /*width: 100%; height: 100px;*/ border-radius: 3px;'>
          weigth > 5
          </div>
          
          <p>Rows where <i>weight</i> is greater than 5 will be highlighted in the table below.</p>
          <p>If you want to test several rules, you need to write one rule per line:</p>
          <div style = 'background-color: #fcc; /*width: 100%; height: 100px;*/ border-radius: 3px; margin-bottom: 2rem;
          '>
          weight > 5 <br>
          size < 10.8
          </div></div>
          "
        ),
        fluidRow(
         
          selectInput(ns("var_possible"),label = "",choices = NULL)
          
          ,
          actionButton(ns("add_var_possible"),label = "add")
          
          
          ,
          actionButton(ns("add_is_na"),label = "is.na()"),
          actionButton(ns("add_c"),label = "c()"),
          actionButton(ns("add_in"),label = "%in%"),
          actionButton(ns("add_return"),label = "",icon = icon("console", lib = "glyphicon"))
          
      ),
      fluidRow(
        actionButton(ns("add_equal"),label = "=="),
        actionButton(ns("add_more"),label = ">"),
        actionButton(ns("add_less"),label = "<"),
        actionButton(ns("add_less_equal"),label = "<="),
        actionButton(ns("add_more_equal"),label = ">=")
        
      ),
        textAreaInput(ns("rules"), "Write rules here", "", 
                      width = "1000px"),
        actionButton(ns("validate_button"), "Check rules on data"),
        downloadButton(ns('downloadData'),'Export offending rows'),
        actionButton(ns("set_to_na_button"), "Set offending values to NA"),
        HTML(
          "
          <h4>Data correction</h4>
          <div class='col-sm-6'>
          <p>You can use the text box below to enter simple modification rules to apply to your data.<br>
          As an example suppose you are working with data on wages, and you have to topcode the yearly earnings to 60000 euro.
          This can be done like so:</p>
          
          <div style = 'background-color: #fcc; /*width: 100%; height: 100px;*/ border-radius: 3px;'>
          if (yr_earnings >= 60000) yr_earnings <- 60000
          </div>
          
          <p>It is also possible to set the offending values to NA, and then impute these values using the next tab of the app.</p>
          </div>
          "
        ),
        
        textAreaInput(ns("modify_rules"), "Write modifications here", "if (sexe3 == 1) sexe3 <- 54", width = "1000px"),
        actionButton(ns("modify_button"), "Modify data according to rules"),
        # actionButton(ns("set_to_na_button"), "Set offending values to NA"),
        actionButton(ns("reset"), "Reset"),
        tags$div(class="table-wrapper", DTOutput(ns("tableau")))
      # )
    ),
    br(),
    column(6,actionButton(ns("valider"),"Validate changes",class="btn-primary",style="display:block;margin:auto;")),
    column(6,actionButton(ns("reset_all"),"Reset changes",class="btn-danger",style="display:block;margin:auto;")),
    br(),
    br()
    )
}

#' mod_validate server function
#' @param input For internal purposes
#' @param output For internal purposes
#' @param data A reactive expression returning the dataset to work on
#' @param session The current session.
#'
#' @importFrom validate confront validator as.data.frame
#' @importFrom dcmodify modifier modify
#' @importFrom tidyr nest
#' @importFrom shinyjs onclick
#' @importFrom purrr reduce map possibly
#' @importFrom tibble rowid_to_column
#' @importFrom DT renderDT DTOutput dataTableProxy selectRows
#' @import dplyr
#' 
#' @export
mod_validate <- function(input, output, session, r) {
  
  
  # dataset <- reactive({ data() })
  
  
  # Define out$nok_indices as null for R CMD check
  
  out <- reactiveValues(nok_indices = NULL)

  current <- reactiveValues(data = NULL)
  
  observe({
      current$data <- r$sortie 
  })
  
  
  observe({
    updateSelectInput(session = session,inputId = 
    "var_possible",choices = current$data %>% names())
    
  })
  
  output$tableau <- renderDT({
  
    datatable(
      current$data,
      selection = list(target='row+column'),
      # selection = list(target='column'),
      options = list(ordering = FALSE,
                     scrollX = TRUE,lengthMenu = c( 10,1, 125, 1000),
                     preDrawCallback = JS("function() { 
                                          Shiny.unbindAll(this.api().table().node()); }"),
                     drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
}")
      )
    )
    
  })#on ne laisse pas l'edition possible
  
  proxy <- dataTableProxy('tableau')
  
  observeEvent(input$reset,{
     message("reset")
    current$data <- r$sortie
    replaceData(proxy, current$data, resetPaging = FALSE)
  })
  
  observeEvent(input$tableau_cell_edit, {
     # message("ad")
    info <- input$tableau_cell_edit
    #str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    current$data[i, j] <<- DT::coerceValue(v, current$data[i, j])
    replaceData(proxy, current$data, resetPaging = FALSE)  # important
  })
  
  observeEvent(input$validate_button, {
     # message("ae")
    df <- current$data
    to <- tempfile(fileext = ".txt")
    writeLines(input$rules, con = to)
    
    validator_obj <- create_validator(df, to)
    
    # Get indices where at least one rule does not pass
    indices <- purrr::possibly(get_indices, otherwise = NULL)(df, validator_obj)
    
    # "nok" stands for not ok, indices that do not pass the rules
    out$nok_indices <<- purrr::possibly(get_unique_indices, otherwise = NULL)(indices)
    if(is.null(out$nok_indices)){
      showModal(modalDialog(
        title = "Validating the rules(s) returned an error.",
        "One possible explanation is that the column names are wrong.",
        "Check that the column names are correct.",
        easyClose = TRUE
      ))}
    
    proxy %>% selectRows(as.numeric(out$nok_indices))
    
  })
  
  offending_rows <- reactive({
     # message("af")
    df <- current$data
    
    offending_rows <- df %>% 
      tibble::rowid_to_column("offending_rows") %>%
      dplyr::slice(as.numeric(out$nok_indices))
  })
  
  output$downloadData <- downloadHandler(
    # "txt","csv","xls","xlsx","rds"
    filename = function() {
      dataName <- paste0("offending_rows-", Sys.Date())
      paste0(dataName, ".csv")
    },
    content = function(file) {
      rio::export(x = offending_rows(), file = file) # TODO remplacer rio par autre chose pour supprimer dependance
    }
  )
  
  output$offending_index <- renderPrint({
    out$nok_indices
  })
  
  observe({
     # message("ah")
    if (is.null(out$nok_indices)) {
      shinyjs::disable("downloadData")
    } else {
      shinyjs::enable("downloadData")
    }
  })
  
  # Disable download after clicking on it, thus forcing the user to validate the rules again
  onclick("downloadData", {
     # message("ai")
    shinyjs::disable("downloadData")
  })
  
  observeEvent(input$set_to_na_button, {
     # message("aj")
    df <- current$data
    to <- tempfile(fileext = ".txt")
    writeLines(input$rules, con = to)
    
    validator_obj <- create_validator(df, to)
    
    # Get indices where at least one rule does not pass
    indices <- purrr::possibly(get_indices, otherwise = NULL)(df, validator_obj)
    
    df <- purrr::possibly(set_cols_to_na, otherwise = NULL)(df, validate::variables(validator_obj), indices)
    if(is.null(df)){
      showModal(modalDialog(
        title = "Setting values to NA failed.",
        "One possible explanation is that no values violate any rules.",
        easyClose = TRUE
      ))}
    
    current$data <- df
    
    replaceData(proxy, df, resetPaging = FALSE)  # important
    
  })
  
  observeEvent(input$modify_button, {
     # message("ak")
    df <- current$data
    to <- tempfile(fileext = ".txt")
    writeLines(input$modify_rules, con = to)
    
    modifier_obj <- create_modifier(to)
    
    # df <- purrr::possibly(dcmodify::modify, otherwise = NULL)(df, modifier_obj)
    # 
    df <-  df %>%  applique_modifier_sur_factor(modifier_obj)
    
    
   if(is.null(df)){
      showModal(modalDialog(
        title = "Modifying values failed.",
        "One possible explanation is that no values need modification.",
        easyClose = TRUE
      ))}
    
    current$data <- df
    
    replaceData(proxy, df, resetPaging = FALSE)  # important
    
  })
  
  sortie_val <- reactive({current$data     })
  
  observeEvent(input$add_equal,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " == ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_more,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " > ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_less,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " < ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_less_equal,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " <= ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_more_equal,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " >= ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_in,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " %in% ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_return,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, "\n",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_var_possible,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " `",input$var_possible ,"` ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_is_na,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " is.na(  ) ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  observeEvent(input$add_c,{
    updateTextInput(session = session,
                    inputId = "rules",
                    value = paste0(input$rules, " c( , ) ",sep = " ")
                    )
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  observeEvent(r$onglet,{
    ns <- session$ns
    if(!isTRUE(all.equal(r$sortie,sortie_val())) & r$onglet != "validate"){
      showModal(modalDialog(title = "You have not validated your changes on 'Validate'",
                            footer=tagList(actionButton(ns("ok"),"Validate"),
                                           actionButton(ns("reset2"),"Reset"))
      ))
      }
  })
  
  
  observeEvent(input$reset2,{
    message("reset")
    removeModal(session = getDefaultReactiveDomain())
    current$data <- r$sortie
    replaceData(proxy, current$data, resetPaging = FALSE)
  })
  
  observeEvent(input$reset_all,{
    message("reset")
    current$data <- r$sortie
    replaceData(proxy, current$data, resetPaging = FALSE)
  })
  
  observeEvent(input$ok,{
    removeModal(session = getDefaultReactiveDomain())
    r$sortie <- current$data
  })
  
  observeEvent(input$valider,{
    r$sortie <- current$data
  })
  
}
