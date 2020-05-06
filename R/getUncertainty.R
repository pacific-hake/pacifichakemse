#' Get uncertainty (TODO: Improve docs on this function)
#'
#' @param name The parameter name
#' @param data Data
#' @param sdrep A [data.frame] of standard deviations
#'
#' @return A [data.frame] containing uncertainty
#' @export
getUncertainty <- function(name,
                           data,
                           sdrep){

  df <- data.frame(value = sdrep[rep.values == name,1])
  df$SE <- sdrep[rep.values == name,2]
  df$min <- df$value-2*df$SE
  df$max <- df$value+2*df$SE

  if(dim(df)[1] == data$tEnd){
    df$year <- data$years
  }

  if(dim(df)[1] == (data$tEnd*data$nage)){
    # TODO
  }

  if(dim(df)[1] == (data$tEnd*data$age_maxage)){
    df$age <- rep(1:data$age_maxage, data$tEnd)
    df$year <- rep(data$year, each =length(1:data$age_maxage))
  }
  df
}

