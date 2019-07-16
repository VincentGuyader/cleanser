# https://datatables.net/reference/option/language
# {
#   "decimal":        "",
#   "emptyTable":     "No data available in table",
#   "info":           "Showing _START_ to _END_ of _TOTAL_ entries",
#   "infoEmpty":      "Showing 0 to 0 of 0 entries",
#   "infoFiltered":   "(filtered from _MAX_ total entries)",
#   "infoPostFix":    "",
#   "thousands":      ",",
#   "lengthMenu":     "Show _MENU_ entries",
#   "loadingRecords": "Loading...",
#   "processing":     "Processing...",
#   "search":         "Search:",
#   "zeroRecords":    "No matching records found",
#   "paginate": {
#     "first":      "First",
#     "last":       "Last",
#     "next":       "Next",
#     "previous":   "Previous"
#   },
#   "aria": {
#     "sortAscending":  ": activate to sort column ascending",
#     "sortDescending": ": activate to sort column descending"
#   }
# }

DT_cleanser_language <- function(){
  list(
    paginate = list(
      previous = "Pr\u00E9c\u00E9dent",
      `next` = "Suivant",
      first  = "Premier",
      last = "Dernier"
    ),
    search = "Recherche",
    lengthMenu = "Montrer _MENU_ lignes",
    info = "Visualisation des lignes _START_ \u00E0 _END_ sur un total de _TOTAL_ lignes",
    zeroRecords = "Aucune donn\u00E9e ne correspond \u00E0 la recherche"
  )
}

#' @importFrom withr with_options
show_na_inf <- function (expr){
  with_options( list( htmlwidgets.TOJSON_ARGS = list(na = 'string') ), {
    expr
  } )
}

#' @importFrom DT datatable DTOutput renderDT
# @export
#' @noRd

datatable_cleanser <- function(data, rownames = FALSE, options = list(language = DT_cleanser_language()), selection = "single", ...){
  options <- append( options, list(dom = "tip", searching = FALSE, paging = FALSE, ordering = TRUE,
                                   lengthMenu = c( 10,1, 125, 1000),
                                   scrollX = TRUE
                                   
                                   ) )
  datatable( data, options = options, rownames = rownames, selection = selection, ...)
}