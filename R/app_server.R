#' @import shiny
app_server <- function(input, output,session) 
  {
  # List the first level callModules here
  
  ###  ONGLET 1 - Import ( et typage ... )
  r <- reactiveValues()
  observe({
    r$onglet <- input$onglet
  })
  # in récupere les donnée d'input d'orgine
  # data <- callModule(mod_all_file,"fichier", r = r)
  callModule(mod_all_file,"fichier", r = r)
  
  # et on les affiches en peremettant à l'utilisateur de selectionner des colonnne dont il veut tracer des graph
 
  # TODO remplacer ce tableau par mod_format et permettre à l'utilisateur de changer le typage des données,
  # le module est bugé pour le moment
   output$tableau <- DT::renderDT({
    datatable(r$sortie,
              selection = list(target = "column"))
  })
  
  # on prepare un dataset minimaliste qui ne contint que les colonnes selectionnée pour le graph
  data_graph <- eventReactive( input$tableau_columns_selected , {
    req(r$sortie)
    r$sortie[,input$tableau_columns_selected %>% setdiff(0)]
    
  })
  
  
  # on affiche les graphs exploratoires.
  callModule(mod_graph,"graph_import",data = data_graph)
  
  
  
  # l'idée est que tou les autre onglet prennent en entrée data, ET retourne data, ce data est l'input et l'output de tout ce beau monde
  # pour le moment l'export est sous un autre nom, pour voir. le challenge est de tout relier
  ###  ONGLET 2 - Filtre
  
  callModule(mod_select_column, "filter_columns", r = r)

  ###  ONGLET 3 - Recode factor
  
  callModule(mod_factor, "factor", r = r)
  
  
  
  ### ONGLET 4 - validation
  callModule(mod_validate, "validation_rule", r = r)
  # callModule(mod_graph, "graph_validation", out_validate)
  # 
  callModule(mod_impute, "imp", r = r )
  
  # callModule(mod_graph, "graph_imp", out_imputation)
  # 
 callModule(mod_export, "export", r = r )
  # callModule(mod_graph, "graph_export", out_export)
  
  
  
  
  
  
  
  
  
  
  
  
}
