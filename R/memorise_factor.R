memorise_factor <- function(df){
  df %>% 
    select_if(is.factor) %>% 
    map(levels)
}

applique_memo <- function(df,memoire){
   print("memoire")
   print(Sys.time())
   
   for ( i in seq_along(memoire)){
     
     df[[names(memoire)[i]]] <- 
       as_factor(df[[names(memoire)[i]]]) %>% 
       fct_expand(memoire[[i]]) %>%
       droplevels()
     
   }
   
   df
 }