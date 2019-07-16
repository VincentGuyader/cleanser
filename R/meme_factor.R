meme_factor <- function(a,b){
  if(!isTRUE(all.equal(levels(a),  levels(b)))){
    return(FALSE)
  }
  
  all(as.numeric(a) == as.numeric(b),na.rm=TRUE) &
     (all(levels(a) == levels(b),na.rm=TRUE))
}
