#' mod_factor and mod_factorUI  function
#' @param id The id of the current element you are entering
#' @description Shiny Module that allows to recode,reorder, factor levels.
#'
#' @export
#' @importFrom stringr str_replace str_detect
#' @importFrom graphics plot
#' @importFrom purrr map2
#' @importFrom forcats fct_collapse lvls_expand
#' @importFrom htmltools htmlEscape
#' @examples 
#' 
#' if (interactive()){
#' 
#' library(shiny)
#' library(cleanser)
#' library(shinyjs)
#' library(DT)
#' library(readr)
#' ui <- function() {
#'   fluidPage(
#'     titlePanel("cleanser"),
#'     useShinyjs(),
#'     sidebarLayout(
#'       sidebarPanel(
#'         selectInput("go","change data",
#'         c("demo7_rds","boys",
#'          "vins",
#'          "vins_missing",
#'          "one_factor",
#'     "right_csv",
#'     "right_xls",
#'     "demo2_csv",
#'     "demo3_xlsx",
#'     "demo4_rds"
#'         ))
#'       ),
#'       mainPanel(
#'         mod_factorUI("demo")
#'       )
#'     )
#'   )
#' }
#' 
#' 
#' server <- function(input, output,session) {
#'   
#'   
#'   donnee <- reactive({
#'     
#'     md <- function(x){system.file("dataset", x, package = "cleanser")}
#'     switch (input$go,
#'   "boys" = readr::read_csv(md("boys.csv")),
#'   "vins" = readr::read_csv(md("vins.csv")),
#'   "vins_missing" = readr::read_csv(md("vins_missing.csv"),
#'    locale = locale(encoding = 'ISO-8859-1')),
#'   "one_factor" = readr::read_csv(md("one_factor.csv")),
#'   "right_csv" = readr::read_csv(md("right.csv")),
#'   "right_xls" = readxl::read_excel(md("right.xls"),1),
#'   "demo2_csv" = readr::read_delim(md("demo2.csv"),
#'    locale = locale(encoding = 'ASCII'), delim=";"),
#'   "demo4_rds" = readr::read_rds(md("demo4.rds")),
#'   "demo7_rds" = readr::read_rds(md("demo7.rds")),
#'   "demo3_xlsx" = readxl::read_excel(md("demo3.xlsx"),1)
#'     )
#'   })
#'   
#'   callModule(mod_factor,"demo",donnee)
#'   
#' }
#' shinyApp(ui = ui, server = server)
#' }
#' 
mod_factorUI <- function(id){
  ns <- NS(id)
  out <- tagList(
  "a1"=  tags$head(
      ## Custom JS
      tags$script(get_html_file("js", "jquery-ui.js")),
      tags$script(get_html_file("js", "iorder.js")),
      tags$style( get_html_file("css", "ifuncs.css"))
    ),
  "a2"=useShinyjs(),
  "a3"=DTOutput(ns("quali_list")),
  "a4"=tags$h1(textOutput(ns("nom_var"))),
  "a5"=
    tabsetPanel(id=ns("le_tab"),
    tabPanel( "Histogram",
             plotOutput(ns("dessin")) 
    ),
    tabPanel( "Contingency table",
              DTOutput(ns("contingency_table")) 
    )
  )
  ,
  
  "a6"=uiOutput(ns("liste_variables")),
  "a7"=wellPanel(id=ns("well"),htmlOutput(ns("levelsInput"))),
  "dev8"=verbatimTextOutput(ns("log3")),
  "dev9"=verbatimTextOutput(ns("les_input")),
  "a10"=DTOutput(ns("output")),
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


#' @export
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data dataset as reactive
#' @importFrom stats rnorm
#' @importFrom forcats fct_recode fct_collapse
#' @importFrom shinyjs hide show
#' @importFrom purrr walk
#' @rdname mod_factorUI
#' 
mod_factor <- function(input, output, session, r) {
 
  
  
  current <- reactiveValues(input=NULL,
                            var =NULL,
                            init=TRUE,
                            flag=NULL,
                            inc =0,
                            output =NULL,
                            base_out = NULL
  )
  
  observe({
    message('init input dans factorUI')
    # browser()
    current$input <- r$sortie
    isolate(    current$base_out <- r$sortie )
    isolate(    current$output <- NULL )
  })
  
  observe({
    #(1);   message("b")
      if ( is.null(data_quali()) ){
      hide("le_tab")
      hide("well")
    }else if( !is.null(input$quali_list_cell_clicked$row) && input$quali_list_cell_clicked$row >0){
      show("le_tab")
      show("well")
    }else{
      hide("le_tab")
      hide("well")
      }
    
  })
  
  data_quali <- reactive({
    req(current$input)
    #(1);    message("c")
    out <- current$input %>% select_if(is.factor)
    if (ncol(out)==0){out<-NULL}
    out
  })
  
  observeEvent(current$quali_list_rows_selected,{
    #(1);   message("d")
    if (!app_prod()){
    message("changement de variable")
    message("-> update flag")
    }
    current$var  <- NULL
    current$init <- TRUE
    # current$flag <- paste0(rnorm(1),rnorm(1))# a voir
    current$flag <- "flag"# a voir
    # current$flag <- paste0(rnorm(2),collapse = "")# a voir
  })
  observeEvent(input$quali_list_rows_selected ,{
    #(1);   message("e")
    current$quali_list_rows_selected <- input$quali_list_rows_selected
    
  })
  
 
  observe({
    #(1);   message("f")
  if ( is.null(current$base_out)){
    
    if (!app_prod()){
    message("reinit base_out")
    }

    current$base_out <- current$input
  }
    })
  
  
  observeEvent(current$input,{
    #(1);   message("g")
    if(!app_prod()){
    message("update flag")
    }
    # current$flag <- rnorm(1)
    current$flag <- "flag"
    # current$flag <- paste0(rnorm(2),collapse = "")
    current$var  <- NULL
    current$init <- TRUE
    current$output <- NULL
    current$quali_list_rows_selected <- NULL
    
  })
  
  
  selected_var_name_orig  <-reactive({
    req(data_quali())
    req(current$quali_list_rows_selected)
    #(1);   message("h")
    data_quali() %>%
      names() %>%
      .[current$quali_list_rows_selected]
  })
  
  selected_var_name <- reactive({
    req(selected_var_name_orig())
    #(1);   message("i")
    selected_var_name_orig() %>% 
      clean_vec() %>% 
      paste0(.,"_",current$quali_list_rows_selected)
  })

  
  selected_var <- reactive({
    req(data_quali())
    req(current$quali_list_rows_selected)
    # browser()
    #(1);   message("j")
    # la on part de data_quali, qui est la base, mais si on 
    # ecrase et qu'on revient, et bien il ne faut pas partir de ce data_quali, mais du version qui va extraire
    # les factordans le bon ordre
    
    # mais ca suffit pas , faut aussi les correspondance!!!
    
      # tmp <-  data_quali() %>%
    # ici on devriat pter pas partir de base_out mais de l'input
      # tmp <-  isolate(current$base_out) %>% 
      tmp <-  isolate(current$input) %>% 
        select_if(is.factor) %>% 
      select(current$quali_list_rows_selected)
      out <- tmp %>%  unlist()
       
      # on va prendre l'ordre de current$base_out 
    #    
    # out %>% 
    #     meme_ordre(isolate(current$base_out),tmp)  
    #   
      out
  })
  
  data_levels <- reactive({
    req(selected_var())
    #(1);   message("k")
    selected_var() %>%
      levels() %>% 
      data.frame(modalite = .)
  })
  
  data_list <- reactive({
    req( data_levels() )
    #(1);   message("l")
    data_levels() %>%
      unlist()  %>%
      as.character()
  })
  
  all_input <- reactive({
    # req(FALSE)
    #(1);   message("m")
    if (!is.null(current$var) && current$var != selected_var()){
      if(!app_prod()){
      message("update current var")
      }
      current$var <- selected_var()
      return(NA)
    }
    
    req(data_list())
    
    

    
    
    out<-  seq_along(data_list()) %>% 
      map(~input[[
        # NOP session$ns(
          paste0("var_",selected_var_name(),"_",current$flag,"_", .x)
          # )
        ]]) %>% 
      c(NA)# %>% unlist()
    
    req(all(out!="")) # A LAISSER JE PENSE
    
    if(!app_prod()){
    message(out)
    }
    out[out==""]<-"truc"
    out
    
  })
  
  
  
  
  output$liste_variables <- renderUI({
    #(1);   message("n")
    req(data_list())
    # browser()
    input$reset2
    input$reset_all
    list(tags$h1("Recode Levels"),
         map2(
           .x = data_list(),
           .y = seq_along(data_list()),
           ~{
             fluidRow(
               column(tags$h2(paste0(.x," ->")),width = 3),
               column(selectizeInput(options = list(create = TRUE),
                                     inputId = session$ns(
                                       paste0("var_",selected_var_name(),"_",current$flag,"_", .y)
                                       ),
                                     label = "",
                                     choices = data_list(),
                                     selected = .x),
                      width = 4)
               
             )})
    )%>% tagList() %>% wellPanel()
    
  })
  
  
  output$levelsInput <- renderText({
   req(length(new_var())) # list ou factor ???
    #(1);    message("o")
     out <- glue::glue("<h1>Reorder Levels</h1><ol id='{session$ns('')}sortable' class='sortable'>")

     for (l in levels(new_var())){ out <- paste0(out,
                                                '<li><span class="glyphicon glyphicon-move"> </span>&nbsp; <span class="level">',
                                                htmlEscape(l),
                                                '</span></li>')}
    out <- paste0(out, "</ol>")
    HTML(out)
  })
  
  output$les_input <- renderPrint({
   #(1); message("p")
    # new_var_ordered()
    if(!app_prod()){
    message("les_input")
    # invalidateLater(3000)
    }
    c(
    reactiveValuesToList(input),
    reactiveValuesToList(current)
    )
  })
  
  
  # un init par select, des input avec nom de colonne 
  observe({
    req(data_list())
    #(1);  message("q")
    
    lapply(seq_along(data_list()), function (x) {
      observeEvent(input[[
        # session$ns(
        paste0("var_",selected_var_name(),"_",current$flag,"_", x)
        # )
        ]], {
        current$inc <-   current$inc + 1
        # a faire tout le temps
        
        # ICI all_input a verifier
        
        map(seq_along(data_list()),
            ~{
              updateSelectInput(session=session,
                                inputId = 
                                  # session$ns(
                                  paste0("var_",selected_var_name(),"_",current$flag,"_", .x)
                                  # )
                                ,
                                choices = unique(c(data_list(),all_input())) %>% as.character()
                                ,
                                selected = 
                                  
                                  input[[
                                    # session$ns(
                                    paste0("var_",selected_var_name(),"_",current$flag,"_", .x)
                                    # )
                                    ]]
                                
              )})
        if ( current$init & TRUE){
          # a faire qu'au init, juste pour etre sur
          if(!app_prod()){
          message("init")
          }
          map(seq_along(data_list()),
              ~updateSelectInput(session=session,
                                 inputId = 
                                   # session$ns(
                                   paste0("var_",selected_var_name(),"_",current$flag,"_", .x)
                                   # )
                                 ,
                                 
                                 selected = data_list()[[.x]] 
                                 
              ))
          current$init <- FALSE
        }
        
        
      })
    })
  }
  )
  
  corresp <- eventReactive(current$inc,{
    #(1);  message("r")
 
   out <-  data.frame(avant = data_list(),
               loc = seq_along(data_list()) %>% paste0("var_",selected_var_name(),"_",current$flag,"_", .)
    ) %>% 
      mutate(apres = loc %>% map(~input[[
        # session$ns(
        as.character(.x)
        # )
        ]])) %>% 
      mutate(apres=unlist(apres)) %>% 
      select(-loc)
    req(all(out$apres!="")) # pour ne pas reagir si on efface un terme
    out
  })
  
  base <- reactive({
    req(corresp())
    #(1);  message("s")
    # a spliter pour perf
    kk <- corresp() %>%
      group_by(apres) %>%
      summarise(avant=list(avant))
    base <- kk$avant %>% set_names(kk$apres) %>% map(as.character)
    base
  })
  
  # new_var <- reactive({
  new_var <- eventReactive(base(),{
    message("newvar")
    # req(base())
    # req(selected_var())
    # browser()
    #(1);   message("t")
    # a spliter pour perf
    # if (!app_prod()){
    #   message("conversion :")
    #   print(base())
    # }
    # CEST 2 TRUC DOIVENT ETRE PAREIL POUR EVITER LE COLLAPSE POUR RIEN
    req( isTRUE(all.equal(sort(levels(selected_var())),    sort(unique(unlist(base()))))))
    out<-do.call(fct_collapse,c(list(.f=selected_var()),base())) %>% 
      fct_recode(NULL="NA")
    current$output <- out 
    out
  })
  
  new_var_ordered <- reactive({
    req(length(current$output))
    #(1);  message("u")
    # req(input$sortable)
    if ( is.factor(current$output) && isTRUE(all.equal(sort(levels(current$output)),
                         suppressWarnings(sort(input$sortable))))) {
      try(current$output <- current$output %>%
        lvls_expand(input$sortable))
    }
    current$output
  })
  
  observe({

   req(length(new_var_ordered()))
    # req(new_var_ordered()) # remis comme avant pour eviter la suppression de colonne
    req(isolate(selected_var_name()))
    #(1);   message("v")
    if ( !is.null(new_var_ordered())){
    col <- isolate({selected_var_name()}) %>%
      str_replace("_[0-9]*$","")
    if (!meme_factor(new_var_ordered(),current$base_out[[col]]) ){
      # si c'est pareil on n'ecrase pas
      if (!app_prod()) {
        message("update data")
      }
    current$base_out[[col]] <- new_var_ordered()
    }
    }
  })
  
  output$output <- renderDT({
    req(sortie_fac())
    #(1);  message("w")
    sortie_fac()
  })
  
  output$log <- renderPrint({
    #(1);  message("x")
    selected_var()})
  
  
  
  
  
  output$quali_list <- renderDT({
    #(1);  message("y")
    validate(
      need(!is.null(data_quali()), "No qualitative variable to recode")
    )
    
    tibble(`Choose a qualitative variable to recode and reorder` = 
             data_quali() %>% 
             names
           
    ) %>% 
      datatable_cleanser()
    
    
  })
  output$dessin <- renderPlot(
    {
      #(1);    # message("z")
      # req(selected_var())
      # req(current$quali_list_rows_selected)
      req(new_var_ordered())
      plot(new_var_ordered())
    }
  )
  
  output$contingency_table <- renderDT({
    req(new_var_ordered())
    #(1);  message("aa")
    table(new_var_ordered(),useNA = "ifany") %>%
      as.data.frame() %>%
      rename(levels = Var1)
  })
  
  sortie_fac <- reactive({
    #(1);  message("ab")
    req(current$base_out)
    req(current$input)
    out <- current$base_out
    try(out <-  current$base_out[names(current$input)], silent = TRUE)
    out
  })
  

  observe({
    if(r$onglet == "recode"){
      r$test <- sortie_fac()
    }else{
      r$test <- r$sortie
    }
  })
  observeEvent(r$onglet,{
    # browser()
    ns <- session$ns
    if(!isTRUE(all.equal(r$sortie,r$test)) & r$onglet != 'recode'){
      showModal(modalDialog(title = "You have not validated your changes on 'Recode'",
                            footer=tagList(actionButton(ns("ok"),"Validate"),
                                           actionButton(ns("reset2"),"Reset"))
      ))
      
          }
  })
  
  observeEvent(input$ok,{
    removeModal(session = getDefaultReactiveDomain())
    r$sortie <- sortie_fac()
  })
  
  observeEvent(input$reset2,{
    removeModal(session = getDefaultReactiveDomain())
    current$input <- r$sortie
  })
  
  observeEvent(input$valider,{
    r$sortie <- sortie_fac()
  })
  
  observeEvent(input$reset_all,{
    current$input <-  r$sortie
  })
}