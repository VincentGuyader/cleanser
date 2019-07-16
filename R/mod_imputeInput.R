#' mod_imputeInput UI function
#' @param id The id of the current element you are entering
#' @examples
#' if (interactive()) {
#' library(cleanser)
#' ui <- shinyUI(
#'   fluidPage(
#'     titlePanel("Imputation module"),
#'     mod_imputeSpinInput("imp")
#'
#'   ))
#'
#' server <- function(input, output,session) {
#'   data <- reactive({
#'     boys
#'   })
#'   callModule(mod_imputeSpin,"imp", reactive(data()))
#' }
#'
#' shiny::shinyApp(ui = ui, server = server)
#'}
#'
#' @import mice  naniar
#' @importFrom shinyjs useShinyjs hidden hide show
#' @importFrom DT DTOutput renderDT datatable
#' @export
mod_imputeInput <- function(id) {
  ns <- NS(id)
  # useShinyjs()
  tagList(useShinyjs(),
    tags$div(id="active", sidebarLayout(
                                            sidebarPanel(
                                              numericInput(ns("m"),label= "Number of total imputations (may vary, depending on how many cores you use)", value = 5,min=1,max=Inf),
                                              numericInput(ns("n_core"),label= "CPU cores to use for multiple imputation", value = parallel::detectCores(),min=1,max=Inf),
                                              numericInput(ns("maxit"),label= "Maximum number of iterations", 5,min=1,max=Inf),
                                                       actionButton(ns("single_impute_button"), "Single imputation"),
                                                       actionButton(ns("multiple_impute_button"), "Multiple imputation"), # soucis de fiabilite
                                                       actionButton(ns("reset"), "Reset")),
                                            mainPanel(plotOutput(ns("vismiss")),
                                                      tags$div(class="table-wrapper", 
                                                               DT::DTOutput(ns("tableau"))
                                                               
                                                               ),
                                                      br(),
                                                      column(6,actionButton(ns("valider"),"Validate changes",class="btn-primary",style="display:block;margin:auto;")),
                                                      column(6,actionButton(ns("reset_all"),"Reset changes",class="btn-danger",style="display:block;margin:auto;")),
                                                      br(),
                                                      br()
                                            )
                                          )
                                          
    )
    
  )
}

#' mod_impute Server function
#' @param input For internal purposes
#' @param output For internal purposes
#' @param data A reactive expression returning the dataset to work on
#' @param session The current session.
#' @import mice
#' @importFrom naniar vis_miss
#' @importFrom purrr map_df
#'
#' @export
mod_impute <- function(input, output, session, r) {
  ns <- session$ns
  
  # dataset <- reactive({ data() })
  
  current <- reactiveValues( input = NULL,
                             tableau_a_afficher = NULL,
                             tableau_a_dessiner = NULL
                             )
  
  observe({
      # current$data <- r$sortie
      current$tableau_a_afficher <- r$sortie
      current$tableau_a_dessiner <- r$sortie
    })
  
  output$vismiss <- renderPlot({
    df <- current$tableau_a_dessiner
    purrr::possibly(naniar::vis_miss, otherwise = NULL)(df)})
  
  output$tableau <- renderDT({
     message("genere tableau")
    req(current$tableau_a_afficher)
    df <- current$tableau_a_afficher
    datatable(
      df,
      escape = FALSE, 
      selection = list(target="column"), 
      options = list(scrollX = TRUE,ordering = FALSE,lengthMenu = c( 10,1, 125, 1000),
                     initComplete = JS("function() {
                                       $('.colors').remove();}"),
                     preDrawCallback = JS("function() { 
                                          Shiny.unbindAll(this.api().table().node()); }"),
                     drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
                                       }")
      )
  )
    
    }#,escape = FALSE
  )
  
 
  
  observeEvent(input$single_impute_button, {

     message("lancement single")
    shinyjs::disable("single_impute_button")
    shinyjs::disable("multiple_impute_button")
    shinyjs::disable("reset")
    shinyjs::disable("reset_all")
    shinyjs::disable("valider")
    hide("tableau")
    df <- current$tableau_a_dessiner
    # browser()
    res_simple_imp <- NULL
    single_imp_data <- df
    print_single_imp_data <- df
    o <- NULL
    o <- try({
    res_simple_imp <- mice(df, m = 1, maxit = as.numeric(input$maxit))
    single_imp_data <- res_simple_imp %>% mice::complete()
    })
    
    if (inherits(o,"try-error")){
      shinyalert::shinyalert(title = "error during Imputation", type = "error")
          }
    
    
    if(any(res_simple_imp$where==TRUE)){
      if(nrow(df)>1) {
        same_indices <- which(df == single_imp_data, arr.ind = TRUE) %>% as.data.frame
        # nrow(df)
        # ncol(df)
        all_indices <- data.frame(
          row=rep(1:nrow(df),ncol(df)),
          col=rep(1:ncol(df), each=nrow(df))
        )
        
        not_same <- anti_join(all_indices, same_indices,by = c("row", "col"))
        
        # i=2
        
        # print_single_imp_data is a copy of the data but where factors are characters to avoid 
        # problems printing the data frame. This is needed because if we try to print the original
        # data we get errors when trying to paste the CSS into a cell of a column that is a factor
        # (following warning: invalid factor level, NA generated
        # Warning in `[<-.factor`(`*tmp*`, iseq, value = "<span style='font-weight:bold; color:brown'>5</span>") :)
        print_single_imp_data <- single_imp_data %>% 
          # dplyr::mutate_if(is.factor, as.character)
          dplyr::mutate_all(as.character)
        for(i in seq_len(nrow(not_same))){
          # if(length(is.na(single_imp_data[not_same[i,1],not_same[i,2]]))>0){
          if(is.na(single_imp_data[not_same[i,1],not_same[i,2]])){
            imput_color <- "red"
          }else{
            imput_color <- "brown"
          }
          # }
          
          print_single_imp_data[not_same[i,1], not_same[i,2]] <-  paste0("<span style='font-weight:bold; color:",imput_color,"'>",print_single_imp_data[not_same[i,1],not_same[i,2]],"</span>")
        }
      }
      
    } 
    
    
    current$tableau_a_afficher <- print_single_imp_data
    current$tableau_a_dessiner <- single_imp_data
    show("tableau")
    shinyjs::enable("single_impute_button")
    shinyjs::enable("multiple_impute_button")
    shinyjs::enable("reset")
    shinyjs::enable("reset_all")
    shinyjs::enable("valider")

  })
  
  
  observeEvent(input$multiple_impute_button, {
    message("multiple")
    shinyjs::disable("single_impute_button")
    shinyjs::disable("multiple_impute_button")
    shinyjs::disable("reset")
    shinyjs::disable("reset_all")
    shinyjs::disable("valider")
    hide("tableau")
    
    
    
    
    df <- current$tableau_a_dessiner
    
    is_na_val <- purrr::map_lgl(df, ~any(is.na(.)))
    print_multiple_imp_data <- df
    multiple_imp_data <- df
    o <- NULL
    o <- try({  
    if (any(is_na_val)) {
      res_multiple_imp <- parlMICE(df,
                                   n.core = as.numeric(input$n_core),
                                   m = as.numeric(input$m),
                                   maxit = as.numeric(input$maxit))
      
      # Only keep the mean of the imputed values (or mode in case of factor)
      multiple_imp_data <- res_multiple_imp %>% 
        mice::complete("long") %>% 
        group_by(.id) %>% 
        mutate_if(is.numeric, mean) %>% 
        mutate_if(~{!is.numeric(.)}, sample_mode) %>% 
        ungroup() %>% 
        slice(seq_along(1:nrow(df))) %>% 
        select(-.imp, -.id)
      
    
      
      
      if(any(res_multiple_imp$where==TRUE)){
        if(nrow(df)>1) {
          same_indices <- which(df == multiple_imp_data, arr.ind = TRUE) %>% as.data.frame
          # nrow(df)
          # ncol(df)
          all_indices <- data.frame(
            row=rep(1:nrow(df),ncol(df)),
            col=rep(1:ncol(df), each=nrow(df))
          )
          
          not_same <- anti_join(all_indices, same_indices,by = c("row", "col"))
          
          
          # print_multiple_imp_data is a copy of the data but where factors are characters to avoid 
          # problems printing the data frame. This is needed because if we try to print the original
          # data we get errors when trying to paste the CSS into a cell of a column that is a factor
          # (following warning: invalid factor level, NA generated
          # Warning in `[<-.factor`(`*tmp*`, iseq, value = "<span style='font-weight:bold; color:brown'>5</span>") :)
          print_multiple_imp_data <- multiple_imp_data %>% 
            dplyr::mutate_all(as.character) 
          
          
          for (i in seq_len(nrow(not_same))) {
            # if(length(is.na(multiple_imp_data[not_same[i,1],not_same[i,2]]))>0){
            if (is.na(multiple_imp_data[not_same[i,1], not_same[i,2]])){
              imput_color <- "red"
            }else{
              imput_color <- "brown"
            }
            # }
            
            print_multiple_imp_data[not_same[i,1], not_same[i,2]] = paste0("<span style='font-weight:bold; color:",imput_color,"'>",print_multiple_imp_data[not_same[i,1],not_same[i,2]],"</span>")
          }
        } 
      }
   
      
    }else{
      print_multiple_imp_data <- df
      multiple_imp_data <- df
        }
    })
    if (inherits(o,"try-error")){
      shinyalert::shinyalert(title = "error during Imputation", type = "error")
    }
    
    current$tableau_a_afficher <- print_multiple_imp_data
    current$tableau_a_dessiner <- multiple_imp_data
    show("tableau")
    shinyjs::enable("single_impute_button")
    shinyjs::enable("multiple_impute_button")
    shinyjs::enable("reset")
    shinyjs::enable("reset_all")
    shinyjs::enable("valider")
    
  })
 
  observeEvent(input$reset,{
    message("reset")
    
    current$tableau_a_afficher <- r$sortie
    current$tableau_a_dessiner <- r$sortie
    hide("tableau_multi")
    show("tableau_single")
  })
  
  # sortie <- eventReactive(current$tableau_a_dessiner,{
  #   current$tableau_a_dessiner
  # })

  observeEvent(r$onglet,{
  
    if(!isTRUE(all.equal(r$sortie,current$tableau_a_dessiner)) & r$onglet != "imputation"){
      showModal(modalDialog(title = "You have not validated your changes on 'Imputation'",
                            footer=tagList(actionButton(ns("ok"),"Validate"),
                                           actionButton(ns("reset2"),"Reset"))
      ))
          }
  })
  
  observeEvent(input$ok,{
    removeModal(session = getDefaultReactiveDomain())
    r$sortie <- current$tableau_a_dessiner
  })
  
  observeEvent(input$reset2,{
    removeModal(session = getDefaultReactiveDomain())
    current$tableau_a_afficher <- r$sortie
    current$tableau_a_dessiner <- r$sortie
    hide("tableau_multi")
    show("tableau_single")
  })
  
observeEvent(input$valider,{
  r$sortie <- current$tableau_a_dessiner 
  })
  

observeEvent(input$reset_all,{
  message("reset")
  
  current$tableau_a_afficher <- r$sortie
  current$tableau_a_dessiner <- r$sortie
  hide("tableau_multi")
  show("tableau_single")
})
  }
