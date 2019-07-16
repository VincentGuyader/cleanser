parse_type <- function(column, tol){

  is_entier <- function(x){
    !(x %% 1)
  }

  nas_column <- is.na(column)
  uniques <- length(unique(column))

  num_column <- to_numeric(column)
  nas_num_column <- is.na(num_column)
  num_uniques <- length(unique(num_column))

  # 100% factor
  # Case when variable looks like c("man", "woman", "man", "man") etc
  if (sum(nas_num_column) > sum(nas_column)){
    likely_type <- "100% factor"
  }

  # 90% factor, 10% numeric
  # Case when variabe looks like c(1,2,2,1,1,2) etc

  if (!all(nas_num_column) & num_uniques <= tol & all(is_entier(num_column))){ # only integers
    likely_type <- "90% factor, 10% numeric"
  }

  # 100% numeric
  # Case when variable is full of numerics

  if (!all(nas_num_column) & num_uniques > tol & !any(is_entier(num_column))){ # not only integers
    likely_type <- "100% numeric"
  }

  return(likely_type)

}