#' Modify the `yr` row in the `wage` [data.frame], copying data from the `yr_copy` row
#'
#' @param wage A weight-at-age [data.frame] as created by [load_ss_model_data()]
#' @param yr The year to modify the row data for in the [data.frame]
#' @param yr_copy The year to use as source data row for the copy. If `NULL`, the first row will be used
#'
#' @return The same `wage` [data.frame] with the year row modified
#'
#' @export
modify_wage_df <- function(wage = NULL, yr = NULL, yr_copy = NULL){

  stopifnot("Yr" %in% colnames(wage))

  line <- which(wage[, "Yr"] == yr)
  if(is.null(yr_copy)){
    line_copy <- 1
  }else{
    line_copy <- which(wage[, "Yr"] == yr_copy)
  }

  if(!nrow(wage)){
    warning("The wage data frame provided is empty")
    return(wage)
  }
  if(!length(line)){
    warning("The year ", yr, " was not found in the wage data frame provided")
    return(wage)
  }
  if(!length(line_copy)){
    warning("The yr_copy year ", yr, " was not found in the wage data frame provided")
    return(wage)
  }
  if(length(line) > 1){
    warning("The year ", yr, " occurs multiple times in the wage data frame provided")
    return(wage)
  }
  if(length(line_copy) > 1){
    warning("The yr_copy year ", yr, " occurs multiple times in the wage data frame provided")
    return(wage)
  }
  if(line == line_copy){
    warning("The year to copy from is the same as the year to copy to")
    return(wage)
  }

  wage[line, -1] <- wage[line_copy, -1]

  wage
}

