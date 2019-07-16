#' guess encoding (brute force)
#'
#' @param base path to file
#' @param dec decimal separator character
#' @param skip number of rows to skip
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param sep the field separator character.
#' @param nrows number of rows to import
#' @param info booleen add more information
#'
#' @export
#' @importFrom purrr safely map discard map_df map2_df
#' @importFrom dplyr arrange mutate_all mutate
#' @importFrom tibble rownames_to_column
#' @importFrom utils read.table
#' @importFrom stats setNames
#' @importFrom  glue glue
#' @examples 
#' library(dplyr)
#' file <- system.file("dataset","boys.csv",
#'                     package = "cleanser")
#' detect_encoding(file,sep = ",",nrows = 100)%>% arrange(desc(row)) %>% head()
#' 
#' 
#' 
detect_encoding <- function(base,
                            dec = ",",
                            skip = 0,
                            header = TRUE,
                            sep = "\t",
                            nrows=10,
                            info=FALSE) {

  message(glue("attempt using sep = {sep} & dec = {dec}"))

  poss_read.table <- purrr::safely(read.table_with_timeout, otherwise = data.frame(vide = 0, revide = 0))

res <-  get_available_encoding() %>% map(
    ~poss_read.table(base,
                     fileEncoding = .x,
                     dec = dec,
                     skip = skip,
                     header = header,
                     sep = sep,
                     nrows = nrows
                     )) %>%
    # map(~`$`(.x,"result")) %>%
  map("result") %>%
    discard(~nrow(.x) == 0) %>%
    map_df(~c(nrow = nrow(.x),
              ncol = ncol(.x),
              "NA" = sum(is.na(.x)))) %>%
    t() %>%
    as.data.frame() %>%
    setNames(c("row", "col", "NA")) %>%
  rownames_to_column("encoding") %>%
    arrange(`NA`, desc(col), desc(row))

# try(incProgress(amount = 25))

if (info){
  res <- res %>%
    mutate(dec = dec,
           sepa = sep)
}

res
}



#' guess encoding (brute force) with all dec and sep parameters
#'
#' @param base path to file 
#' @param nrows number of row to import
#' @importFrom purrr map2_df
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)                   
#' file <- system.file("dataset","boys.csv",
#'                     package = "cleanser")
#' detect_encoding_brute(file) %>% arrange(desc(row)) %>% head()
#' }

detect_encoding_brute <- function(base, nrows=10){
    all_dec <- c(",", ".")
  all_dec <- c(".")
  all_sep <- c(",", ";", "\t", " ")
  combinaisons <- expand.grid(all_dec = all_dec,
                   all_sep = all_sep) %>% 
    mutate_all(as.character)
  map2_df(combinaisons$all_dec,
          combinaisons$all_sep,
          ~detect_encoding(base = base, info = TRUE, dec = .x, sep = .y)
          ) %>%
    mutate(cel = row * col) %>%
    arrange(desc(cel), `NA`) %>%
    select(-cel)
}
