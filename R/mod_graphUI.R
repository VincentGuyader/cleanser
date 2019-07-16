#' mod_graph and mod_graphUI  function
#' @param id The id of the current element you are entering
#' @description Shiny Module that allows to see variable distributions or variable corrrelations.
#' @export
#' @importFrom DT renderDT DTOutput replaceData
#' @examples
#' if (interactive()){
#' library(shiny)
#' library(cleanser)
#' library(readr)
#' library(DT)
#' ui <- fluidPage(
#'   titlePanel("Example: mod_graphUI"),
#'   selectInput("go","change data",c("boys",
#'                                    "vins",
#'                                    "vins_missing",
#'                                    "one_factor",
#'                                    "right_csv",
#'                                    "right_xls",
#'                                    "demo2_csv",
#'                                    "demo3_xlsx",
#'                                    "demo4_rds"
#'   )),
#'   mod_graphUI("truc2"),
#'   tags$div(class="table-wrapper", DTOutput("tableau"))
#' )
#' 
#' server <- function(input, output,session) {
#'   
#'   donnee <- reactive({
#'     
#'     md <- function(x){system.file("dataset",x,package = "cleanser")}
#'     switch (input$go,
#'             "boys" = readr::read_csv(md("boys.csv")) %>% dplyr::mutate_all(as.factor),
#'             "vins" = readr::read_csv(md("vins.csv")),
#'             "vins_missing" = readr::read_csv(md("vins_missing.csv"), 
#'             locale = locale(encoding = 'ISO-8859-1')),
#'             "one_factor" = readr::read_csv(md("one_factor.csv")),
#'             "right_csv" = readr::read_csv(md("right.csv")),
#'             "right_xls" = readxl::read_excel(md("right.xls"),1),
#'             "demo2_csv" = readr::read_delim(md("demo2.csv"), 
#'             locale = locale(encoding = 'ASCII'), delim=";"),
#'             "demo4_rds" = readr::read_rds(md("demo4.rds")),
#'             "demo3_xlsx" = readxl::read_excel(md("demo3.xlsx"),1)
#'     )
#'   })
#'   output$tableau <- renderDT({
#'     
#'     datatable(
#'       donnee(), 
#'       selection = list(target="column"), 
#'       options = list(ordering = FALSE,
#'                      preDrawCallback = JS("function() { 
#'                                           Shiny.unbindAll(this.api().table().node()); }"),
#'                      drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
#' }")
#'         )
#'     )
#' })
#'   updateDate <- reactive({
#'     list(df = donnee(), selected = as.numeric(input$tableau_columns_selected))
#'   })
#'   
#'   
#'   
#'   callModule(mod_graph, "truc2", reactive({updateDate()}))
#'   }
#' # Run the application
#' shinyApp(ui = ui, server = server)
#' }
mod_graphUI <- function(id){
  ns <- NS(id)
  useShinyjs()
  tagList(
    tags$head(
      tags$style(HTML("
                      .table-wrapper {
                      overflow-x:auto;
                      overflow-y:hidden;
                      width: 100%;
                      }
                      
                      "))
      ),
    fluidRow(
      column(6,plotOutput(ns("qualiPlot"))
      ),
      column(6,plotOutput(ns("quantiPlot"))
      )
    )
      )
  }


#' mod_graph server function
#' @param input internal
#'
#' @param output internal
#' @param session internal
#' @param data dataset as reactive
#'
#' @import shiny ggplot2 dplyr vcd corrplot
#' @importFrom stats na.omit
#' @importFrom stats cor
#' @importFrom DT datatable dataTableProxy JS replaceData
#' @importFrom purrr map map_df
#' @export
#' @rdname mod_graphUI
mod_graph <- function(input, output, session, data){

  dataset <- reactive({
      data()
  })
  

  output$qualiPlot <- renderPlot({
    req(dataset())
    req(!is.null(dataset()))
    df <- dataset()
    validate(
      need(!is.null(df)  & !is.null(unlist(df)), " ")
    )
    if (
      !is.null(df) && ncol(df)>0
    ) {

      df_quali <- get_quali(df)
      names <- colnames(df)
      names_quali <- colnames(df_quali)
      if(!app_prod()){
        message("length tableau_columns_selected: ", ncol(df))
      }
      # validate(
      #   need(length(selected) > 0, "Click on one categorical variable to see the graph")
      # )
        show("qualiPlot")
        selected_names <- colnames(df)
        n_selected <- length(which(selected_names %in% names_quali))

        common_col <- selected_names[(selected_names %in% names_quali) & (selected_names %in% names)]

        if (n_selected == 1) {

          group_var <- common_col

          df_quali <- map_df(df_quali, as.character)

          # need na.omit to show the graph, no NA accepted
          df_quali <- df_quali %>%
            group_by_(paste0("`",group_var,"`")) %>%
            summarise( count = n()) %>%
            mutate(perc = count/sum(count)*100) %>%
            na.omit()

          x <- as.factor(unlist(df_quali[, group_var]))


          ggplot(df_quali, aes_string(x = x, y = "perc"), fill = group_var) +
            geom_bar(stat = "identity", fill = "#b0bed9") +
            labs(x = paste0("Class from ",group_var), y = "percent") +
            theme(legend.position = "none") +
            theme_minimal()

        } else if (n_selected > 1) {

          # need na.omit to show the graph, no NA accepted
          df_quali <- df[,common_col] %>%
            na.omit()
          df_quali <- map_df(df_quali, as.character)

          v <- get.V(df_quali)
          validate(
            need(ncol(df_quali) > 1, "Need more than 1 qualitative variable to show this graph")
          )

          quali_plot <- corrplot.mixed(v,upper = "ellipse")

        }

    }


  })



  output$quantiPlot <- renderPlot({
    req(dataset())
    req(!is.null(dataset()))
    df <- dataset()

    validate(
      need(!is.null(df)  & !is.null(unlist(df)), " ")
    )

    if(!is.null(df) && ncol(df)>0) {
      df_quanti <- get_quanti(df)
      names <- colnames(df)
      names_quanti <- colnames(df_quanti)


        show("quantiPlot")
        selected_names <- colnames(df)
        n_selected <- length(which(selected_names %in% names_quanti))
        common_col <- selected_names[(selected_names %in% names_quanti) & (selected_names %in% names)]

   
        if (n_selected == 1) {

          group_var <- common_col
          # need na.omit to show the graph, no NA accepted
          df_quanti <- df_quanti %>%
            na.omit()

          ggplot(df_quanti,aes_string(x=group_var)) +
            geom_density(alpha=0.25,
                         color = "black", fill = "#b0bed9") +
            theme_minimal()
        } else if (n_selected > 1) {
          # selected_names = c("height","mass")

          # need na.omit to show the graph, no NA accepted
          df_cor = df[,common_col] %>%
            na.omit()
          quanti = cor(df_cor)

          validate(
            need(ncol(df_cor) > 1, "Need more than 1 quantitative variable to show this graph")
          )
          corrplot.mixed(quanti,upper = "ellipse")
        }
    }


  })


}




