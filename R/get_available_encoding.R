#' Return all avaible encoding option
#'
#' @export
#' @importFrom magrittr %>% 
#' @examples
#' get_available_encoding()
get_available_encoding <- function(){
#https://w3techs.com/technologies/overview/character_encoding/all + ascii	
top <-   c("UTF-8"	,"ASCII",
    "ISO-8859-1","windows-1251", "windows-1252", "shift-jis", "gb2312", "EUC-KR", 
    "GBK", "euc-jp", "iso-8859-2", "windows-1250", "iso-8859-15", 
    "big5", "windows-1256", "iso-8859-9", "windows-1254")
c(top,iconvlist()) %>% unique() %>% setNames(.,.)
# c("UTF-8","UTF-16LE")%>% setNames(.,.)
}
