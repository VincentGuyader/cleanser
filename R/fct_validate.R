
create_validator <- function(df, file){
  validate::validator(df, .file = file)
}

create_modifier <- function(file){
  dcmodify::modifier(.file = file)
}

get_variables <- function(validator_obj){
  validate::variables(validator_obj)
}

get_indices <- function(df, validator_obj){
  validate::confront(df, validator_obj) %>%
    validate::as.data.frame() %>%
    group_by(name) %>%
    nest() %>%
    mutate(indices = map(data, ~which(.$value == TRUE))) %>%
    pull(indices)
}

get_unique_indices <- function(indices){
  indices %>%
    reduce(append) %>%
    unique()
}
