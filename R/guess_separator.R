tous_pareil <- function(vec){
  length(unique(vec))==1
}
#' guess txt file separator
#'
#' @param file path to csv or txt file
#' @param separator vector of possible separator to test
#' @param n_max number of row to parse
#'
#' @return sorted vector of possible separator
#' @export
#'
#' @examples
#' file <- system.file("dataset","demo.csv",package = "cleanser")
#' guess_separator(file)
#' file2 <- system.file("dataset","demo2.csv",package = "cleanser")
#' guess_separator(file2)
#' @importFrom stringr str_count
#' @importFrom purrr map_dbl map_lgl set_names
#' @importFrom dplyr arrange_at desc pull vars
#' @importFrom tibble tibble
#' 
guess_separator <- function(file,
                            separator=c(",",";"," ","\t"),
                            n_max=1000
                            ){
   base <- readLines(file,n=n_max)
info<-  separator %>% 
    map(~base %>%   map_dbl(str_count,.x)) %>% 
    set_names(separator)

tp <- info %>% map_lgl(tous_pareil)
z <- info %>% map_lgl(~any(.x != 0))
nbz <- info %>% map_dbl(~sum(.x == 0))
ssd <-info %>% map_dbl(sd)
tibble(separator,ssd,tp,z,nbz) %>% 
  # arrange_at(vars(-separator),desc) %>% 
  arrange(nbz,tp,desc(z),ssd) %>% 
  pull(separator)
}




