safe_modify <-  purrr::possibly(dcmodify::modify, otherwise = NULL)
# library(dcmodify)
# library(tidyverse)
# m <- dcmodify::modifier( if (Species == "setosa") Species <- "banana" )
# iris <- iris %>% dplyr::mutate(couleur=as.factor(sample(letters[1:5],150,replace=TRUE)))
# dcmodify::modify(iris,m) %>% summary()
# applique_modifier_sur_factor(iris,m) %>% summary()
#' @importFrom forcats as_factor fct_expand
#' @importFrom dplyr select_if mutate_if
applique_modifier_sur_factor <- function(df,modifier_obj) {
  
  # on va mémoriser le détail des facteurs et les réappliquer en partant
 avant<- df %>% 
    select_if(is.factor) %>% 
    map(levels)

  a_remettre <- df %>% 
    mutate_if(is.factor,as.character) %>% 
    safe_modify( modifier_obj )

  a_remettre
  # a_remettre %>% map_if(.f = )
  
  # on remet, avec une boule à l'ancienne
  for ( i in seq_along(avant)){

    a_remettre[[names(avant)[i]]] <- 
      as_factor(a_remettre[[names(avant)[i]]]) %>% 
      fct_expand(avant[[i]]) %>%
      droplevels()
    
  }
  a_remettre
  }

