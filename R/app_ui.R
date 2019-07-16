#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyalert::useShinyalert(),
    # List the first level UI elements here 
    fluidPage(
      titlePanel("cleanser"),
      # tags$head(tags$style(csscode)),
      # useShinyjs(),
      # shinyjs::extendShinyjs(text = jscode),
      tabsetPanel(id = "onglet",type = "tabs",
                  tabPanel("Import & Typage",
                           sidebarLayout(
                             sidebarPanel(
                               mod_all_fileInput("fichier")
                             ),
                             mainPanel(
                               mod_graphUI("graph_import"),
                               # mod_formatUI("format")
                               DT::DTOutput("tableau")
                             ))),
                  tabPanel("Filter columns",
                           value = "filter",
                           mod_select_columnUI("filter_columns")
                  ),
                  tabPanel("Recode",
                           value = "recode",
                           mod_factorUI("factor")
                  ),
                  tabPanel("Validate",
                           value = "validate",
                           mod_validateInput("validation_rule"),
                           mod_graphUI("graph_validation_rule")
                  ),
                  tabPanel("Imputation",
                           value = "imputation",
                           mod_imputeInput("imp"),
                           mod_graphUI("graph_imp")
                  ),
                  tabPanel("Export",
                           value = "export",
                           mod_exportUI("export"),
                           mod_graphUI("graph_export")
                  )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'cleanser')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$head(tags$style(csscode)),
    useShinyjs(),
    shinyjs::extendShinyjs(text = jscode)
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )

}
