#' @title   mod_all_fileInput and mod_all_file
#' @description  A shiny Module that imports everything
#' @param id shiny id
#' @param label file input label
#' @description Module that imports, csv, Excel and rds files.
#' @importFrom readr guess_encoding
#' @importFrom shinyjs hidden useShinyjs
#' @importFrom shinydashboard box
#' @importFrom forcats as_factor
#' @import shinyWidgets
#' @export
#' @examples 
#' if (interactive()) {
#' library(shiny)
#' library(cleanser)
#' library(shinyjs)
#' library(DT)
#' ui <- function() {
#'   fluidPage(
#'     titlePanel("cleanser"),
#'     useShinyjs(), 
#'     sidebarLayout(
#'       sidebarPanel(
#'         mod_all_fileInput("fichier")
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
#'   data <- callModule(mod_all_file,"fichier")
#'   
#'   output$tableau <- renderDT({data()})
#' }
#' shinyApp(ui = ui, server = server)
#' }
mod_all_fileInput <- function(id, label = "Upload your file") {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    tags$div(id = "wait", conditionalPanel(condition = paste0("$('html').hasClass('shiny-busy')"),
                                           tags$script(src = "$(body).click(function(e){e.preventDefault();});")
    )),
    fileInput(ns("file"), label),
    fluidRow(
      column(6,
             # checkboxInput(inputId = ns("sampled"),label = "sampled",value = FALSE),
             radioGroupButtons(inputId = ns("total_sampled"), 
                               label = "Import", selected = "partiel", choices = c("total",
                                                                                   "partiel", "aleatoire"),
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
             numericInput(inputId = ns("nombre_ligne"),
                          label = "Number of rows to load",
                          min = 1, max = Inf, value = 100)
      ),
      
      column(6, numericInput(inputId = ns("skip"),
                             label = "Number of rows to skip", min = 0, max = Inf, value = 0))
    ),
    
    fluidRow(
      column(6, hidden(selectInput(inputId = ns("file_type"), label = "type de fichier", choices = c("txt/csv", "xls/xlsx", "rds", "ods")))),
      # column(6,switchInput(inputId = ns("header"),label="Header",value = TRUE,inline=TRUE))
      column(3, checkboxInput(inputId = ns("header"), label="Header", value = TRUE)),
      column(3, checkboxInput(inputId = ns("clean_names"), label="Clean Names", value = TRUE))
    )
    ,
    fluidRow(
      column(6, hidden(selectInput(ns("separator"), label = "Separateur", choices = 
                                     c("virgule" = ",",
                                       "point-virgule" = ";",
                                       "tabulation" = "\t",
                                       "expace" =  " ",
                                       "autre" = "autre"
                                     )))),
      column(6, hidden(selectInput(ns("decimal"), label = "Decimal",
                                   c("point" = ".",
                                     "virgule" = ","
                                   )
      )))
    ),
    hidden(textInput(ns("separator_autre"), label = "Autre separateur", placeholder = "separateur")),
    hidden(selectInput(ns("encodage"), label = "Encodage", choices = 
                         get_available_encoding())),
    hidden(selectInput(ns("sheet"), label = "Onglet a charger", choices = NULL)),
    hidden(actionBttn(inputId =ns("brute_force"), label = "force detection",
                      style = "gradient", color = "royal",
                      icon = icon("crosshairs"))),
    tags$br(),
    htmltools::tags$b(tags$a(icon("plus-square"), "Show raw data", `data-toggle`="collapse", href="#showme")),
    htmltools::tags$div(
      class="collapse", id="showme",
      pre(uiOutput(ns("preview")))
    )
    
    
  )
}

#' @title Server function of mod_all_fileInput module
#' @param input For internal purposes
#' @param output For internal purposes
#' @param session The current session. Used internally.
#' @param stringsAsFactors Should strings be imported as factors? True by default.
#' @importFrom readr read_delim locale
#' @importFrom readxl read_excel excel_sheets 
#' @importFrom readODS ods_sheets read_ods
#' @importFrom shinyjs disable enable hide
#' @importFrom glue glue
#' @export
#' @rdname mod_all_fileInput
mod_all_file <- function(input, output, session, stringsAsFactors = TRUE, r) {
  options(shiny.maxRequestSize=Inf)
  state <- reactiveValues(go=FALSE)
  
  observe({
    input$file
    # message("prems?")
    state$go <- FALSE # en vrai faut FALSE
    # on est la
  })
  
  observe({
    #temp#  message("aa")
    
    hide("separator")
    hide("decimal")
    hide("sheet")
    hide("encodage")
    hide("separator_autre")
    hide("file_type")
    hide("brute_force")
  })
  
  context <- reactiveValues(file_type = NULL,
                            separator = ",",
                            encodage = "UTF-8"
  )
  
  type_mime <- reactive({
    #temp#  message("ab")
    get_content_type(path_fichier())
  })
  
  path_fichier <- reactive({
    #temp#  message("ac")
    correct_extension(userFile()$datapath)
  })
  
  userFile <- reactive({
    #temp#  message("ad")
    validate(need(input$file, message = FALSE))
    input$file
  })
  

  observe({
    #temp#  message("ad2")
    message( glue("File {userFile()$name} uploaded" ) )
    if (!app_prod()){
      
      message( glue("Type { type_mime() }" ) )
      
    }
    ###TESTstate$go <- NULL
  })
  
  
  
  observe({
    #temp#  message("ae")
    req(context$file_type)
    if (context$file_type %ni% c("ods","rds")) {
      if (input$total_sampled == "total") {
        hide("nombre_ligne")
      }
      if (input$total_sampled == "partiel") {
        show("nombre_ligne")
      }
      if (input$total_sampled == "aleatoire") {
        show("nombre_ligne")
      }
    }
  })
  
  # gestion auto des types de fichier
  observe({
    #temp#  message("af")
    req(type_mime())
    state$go <- FALSE
    if ( is_csv( type_mime() ) ){
      if (!app_prod()){
        message('   csv detecte')
        
      }
      enc <- guess_encoding(path_fichier())$encoding
      separator <- guess_separator(path_fichier())[1]
      context$separator <- separator
      
      
      if ( length(enc) == 0 ){
        enc <- "ASCII"
      }
      
      context$encodage <- enc[1]
      
      if (!app_prod()){
        message(glue("         encodage : {enc}"))
      }
      # on ordonne les encodages du plus probable au moins probable
      choix <- c(enc, get_available_encoding()) %>% unique() %>% setNames(.,.)
      
      updateSelectInput(session = session,
                        inputId = "encodage",
                        selected = enc[1],
                        choices = choix
      )
      updateSelectInput(session = session,
                        inputId = "file_type",selected = "txt/csv"
      )
      updateSelectInput(session = session,
                        inputId = "separator",selected = separator
      )
      context$file_type <- "txt/csv"
    }
    
    if ( is_rds( type_mime())){
      if (!app_prod()){
        message('   rds detecte')
      }
      updateSelectInput(session = session,
                        inputId = "file_type",
                        selected = "rds")
      context$file_type <- "rds"
    }
    if ( is_ods( type_mime())){
      if (!app_prod()){
        message('   ods detecte')
      }
      updateSelectInput(session = session,
                        inputId = "file_type",
                        selected = "ods")
      choix <- NULL
      try(choix <- readODS::ods_sheets(path_fichier()))
      if (!is.null(choix)) {
        updateSelectInput(session = session,
                          inputId = "sheet",
                          choices = choix,
                          selected = 1)
        
      }
      
      updateSelectInput(session = session,
                        inputId = "file_type",
                        selected = "ods")
      
      context$file_type <- "ods"
    }
    if ( is_excel( type_mime() ) ){
      if (!app_prod()){
        message('   excel detecte')
      }
      choix <- NULL
      
      try(choix <- readxl::excel_sheets(path_fichier()))
      if (!is.null(choix)) {
        updateSelectInput(session = session,
                          inputId = "sheet",
                          choices = choix#
                          ,selected = 1)
        
        updateSelectInput(session = session,
                          inputId = "file_type",
                          selected = "xls/xlsx")
        
        context$file_type <- "xls/xlsx"
      }
    }
    
    state$go <- TRUE # on peut faire l'import
  })
  
  observeEvent(input$separator,{
    context$separator <-  input$separator
    
  })
  observeEvent(input$encodage,{
    context$encodage <-  input$encodage
    
  })
  
  # affichage /masquage des input
  observe({
    #temp#  message("ag")
    # si ya un fichié
    req(path_fichier())
    req(context$file_type)
    if (context$file_type == "rds" ){
      hide("sheet")
      show("file_type")
      hide("separator")
      hide("decimal")
      hide("brute_force")
      hide("encodage")
      hide("separator_autre")
      hide("nombre_ligne")
      hide("skip")
      hide("header")
      hide("total_sampled")
    }
    if (context$file_type == "xls/xlsx" ){
      show("sheet")
      show("file_type")
      
      if ( input$total_sampled != "total"){
        show("nombre_ligne")
      }
      show("total_sampled")
      updateRadioGroupButtons(session = session,
                              inputId = session$ns("total_sampled"),
                              choices = c("total","partiel"),
                              selected = "partiel",
                              checkIcon = list(yes = icon("ok",lib = "glyphicon")))
      
      show("skip")
      show("header")
      hide("separator")
      hide("decimal")
      hide("brute_force")
      hide("encodage")
      hide("separator_autre")
    }
    if (context$file_type == "ods" ){
      if (!app_prod()){
        message("cas ods")
      }
      show("sheet")
      show("file_type")
      # if ( input$total_sampled != "total"){
      #   show("nombre_ligne")
      # }
      hide("nombre_ligne") # pas encore utilisé
      show("total_sampled")
      # ptet faire un import différents
      updateRadioGroupButtons(session = session,
                              inputId = session$ns("total_sampled"),
                              # choices = c("total"),#,"partiel"),
                              selected = "total",
                              checkIcon = list(yes = icon("ok", lib = "glyphicon")))
      show("skip")
      show("header")
      hide("separator")
      hide("decimal")
      hide("brute_force")
      hide("encodage")
      hide("separator_autre")
    }
    if (context$file_type == "txt/csv" ){
      hide("sheet")
      if ( input$total_sampled != "total"){
        show("nombre_ligne")
      }
      show("total_sampled")
      updateRadioGroupButtons(session = session,
                              inputId = session$ns("total_sampled"),
                              choices = c("total", 
                                          "partiel",
                                          "aleatoire"), selected = input$total_sampled,
                              checkIcon = list(yes = icon("ok", lib = "glyphicon")))
      show("skip")
      show("header")
      show("file_type")
      show("separator")
      show("decimal")
      show("brute_force")
      show("encodage")
      if ( input$separator == "autre"){
        show("separator_autre")
      } else{
        hide("separator_autre")
      }
    }
    
    state$go <- TRUE # on peut faire l'import
  })
  
  observeEvent(input$separator,{
    #temp#  message("ah")
    if ( input$separator == "autre"){
      show("separator_autre")
    } else{
      hide("separator_autre")
    }
  })
  
  observeEvent(input$file_type, {
    #temp#  message("ai")
    context$file_type <- input$file_type
  })
  
  dnlines <- reactive({
    #temp#  message("aj")
    LaF::determine_nlines(path_fichier())
  })
  
  nlines <- reactive({
    #temp#  message("ak")
    if ( context$file_type == "txt/csv"){
      out <-   dnlines()
    }else{
      out <- Inf
    }
    out
  })
  
  observeEvent(nlines(),{
    #temp#  message("al")
    updateNumericInput(session = session,
                       inputId = "nombre_ligne",
                       value = min(input$nombre_ligne,nlines()),
                       max = nlines() # voir pour prendre en compte le header
    )
  })
  
  # import des données
  sortie <- reactive({
    #temp#  message("am")
    out <- data.frame()
    req(userFile())
    req(type_mime())
    req(isTRUE(state$go))
    ###TEST req(state$go)
    # browser()
    # state$go <-FALSE # ou pas ?!
    if (context$file_type == "txt/csv" ){
      try( out <- force_read(sampled=ifelse(input$total_sampled == "aleatoire",TRUE,FALSE),
                             nlines=isolate(nlines()),
                             file = path_fichier(),
                             n_max = ifelse(input$total_sampled == "total",Inf,input$nombre_ligne),
                             delim = ifelse(context$separator !="autre",context$separator,input$separator_autre ),
                             locale = locale(encoding = context$encodage,
                                             decimal_mark = input$decimal),
                             escape_backslash = FALSE,
                             escape_double = TRUE,
                             col_names = input$header,
                             skip = input$skip))
    }
    
    if (context$file_type == "rds" ){
      try( out <-   safe_read_rds(path = path_fichier()))
    }
    
    if (context$file_type == "ods" ){
      try(les_onglets <- ods_sheets(path_fichier()))
      if (!is_viable(les_onglets)){ return(data.frame())}
      if ( ! input$sheet %in% les_onglets){
        return(data.frame())
      } # etonnant de devoir faire ca, c 'est signe qu'il importe avant que l'onglet n'ai été modifié.
      
      try(   out <-   safe_read_ods(path = path_fichier(),
                                    # n_max = ifelse(input$total_sampled == "total",Inf,input$nombre_ligne),
                                    sheet = input$sheet,
                                    col_names = input$header,
                                    skip = input$skip))
    }
    
    if (context$file_type == "xls/xlsx" ){
      path_to_excel <- path_fichier()
      
      # TODO fixer si c'est xls ou xlsx
      # TODO verifier en premier la vrai extension avant de copier... pour eviter de perdre du temps d ecriture disque
     
      # TODO : 
      # readxl::excel_format()
      
       path_to_xlsx <- path_to_excel %>%  
        with_this_extension("xlsx")
      
      path_to_xls <- path_to_excel %>% 
        with_this_extension("xls")
      
      if ( is_viable(excel_sheets(path_to_xls))){
        path_to_excel <- path_to_xls
      } else {
        path_to_excel <- path_to_xlsx
      }
      
      try(les_onglets <- excel_sheets( path_to_excel ))
      if (!is_viable(les_onglets)){ return(data.frame())}
      
      if ( ! input$sheet %in% les_onglets){
        return(data.frame())
      } # etonnant de devoir faire ca, c 'est signe qu'il importe avant que l'onglet n'ai été modifié.
      
      try(out <- safe_read_excel(path = path_to_excel,
                                 n_max = ifelse(input$total_sampled == "total",Inf,input$nombre_ligne),
                                 sheet = input$sheet,
                                 col_names = input$header,
                                 skip = input$skip))
    }
    
    if (!is_viable(out)){ return(data.frame())}
    if (input$clean_names){
      try(out <- clean_names(out))
    }
    
    if (!is_viable(tolower(names(out)))){ return(data.frame())}
    out %>% mutate_if(considere_comme_factor,forcats::as_factor)
    
  })
  observeEvent(input$brute_force,{
    #temp#  message("an")
    disable("file")
    disable("file_type")
    disable("total_sampled")
    disable("separator")
    disable("decimal")
    disable("clean_names")
    updateActionButton(session = session,inputId = "brute_force",icon = icon("hourglass"),
                       label = "..please wait.."
    )
    disable("brute_force")
    disable("encodage")
    disable("separator_autre")
    disable("header")
    disable("decimal")
    disable("nombre_ligne")
    disable("skip")
    
    # server
    # updateProgressBar(session = session, id = "pb", value = 10)
    withProgress(
      expr =     {bourin <-   detect_encoding_brute(path_fichier()) },
      min = 0,max = length(get_available_encoding())*4,
      value = 0,session = session,
      message = "Search parameters"
    )
    # updateProgressBar(session = session, id = "pb", value = 100)
    if(!app_prod()){
      message("fin bourinage")
      print("print fin bourinage")
    }
    updateSelectInput(session = session,
                      inputId = "encodage",
                      selected = bourin$encoding[1]
    )
    updateSelectInput(session = session,
                      inputId = "separator",
                      selected = bourin$sepa[1]
    )
    updateSelectInput(session = session,
                      inputId = "decimal",
                      selected = bourin$dec
    )
    
    updateSelectInput(session = session,
                      inputId = "file_type",
                      selected = "txt/csv"
    )
    enable("file")
    enable("total_sampled")
    enable("file_type")
    enable("separator")
    enable("decimal")
    enable("clean_names")
    enable("brute_force")
    updateActionButton(session = session,
                       inputId = "brute_force",label = "force detection",
                       icon = icon("crosshairs"))
    enable("encodage")
    enable("separator_autre")
    enable("header")
    enable("decimal")
    enable("nombre_ligne")
    enable("skip")
  })
  
  
  output$preview <- renderUI({
    req(userFile())
    # readLines(userFile()$datapath)
    
    pre(
      # includeText(readLines(userFile()$datapath)[1:3])
      paste8(readLines(userFile()$datapath)[1:3], collapse = "\r\n")
    )
    
  })
  
  observe({r$sortie <- sortie()})
  
  sortie
}