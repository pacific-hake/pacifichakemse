#' Plot selectivity for years
#'
#' @param df.new parameters and life history values
#' @param parameters estimates parameters
#'
#' @return Nothing

#' @export
plotSelectivity <- function(df.new,
                            parameters){
  yrs <- df.new$years[df.new$selYear]:df.new$years[length(df.new$years)]
  nyrs <- length(yrs)
  PSEL <- parameters[which(names(parameters) == 'PSEL')]
  psel_fish <- parameters[which(names(parameters) == 'psel_fish')]
  yr.idx <- seq(1,nyrs*length(psel_fish), by = length(psel_fish))
  par(mfrow = c(ceiling(sqrt(nyrs)), floor(sqrt(nyrs))), mar = c(1,1,1,1))
  for(i in 1:(nyrs-1)){
    p_sel <- PSEL[yr.idx[i]:(yr.idx[i+1]-1)]
    tmpsel <- get_select(df.new$age,
                         p_sel = p_sel,
                         s_min = df.new$s_min,
                         s_max =df.new$s_max)
    plot(df.new$age, tmpsel, type = 'l')
  }
}
