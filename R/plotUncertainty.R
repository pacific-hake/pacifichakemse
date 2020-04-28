#' Plot uncertainty for a given parameter
#'
#' @param est Parameter name
#' @param data A [data.frame] of uncertainty data
#'
#' @return Nothing
#' @export
plotUncertainty <- function(est, data){
  yl <- ylimits(est$name, data)
  year <- 1:length(est$name)

  plot(year,est$name,
       ylim = yl,
       type = 'l',
       col = 'red',
       lwd = 2,
       ylab = '',
       xlab = '')

  points(year,
         data,
         lwd = 2)

  polygon(c(year, rev(year)),
          c(est$max, rev(est$min)),
          border = NA,
          col = "#FF000050")
}