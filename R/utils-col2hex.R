#' Convert color name to hex string. Code borrowed from [gplots::col2hex()]
#'
#' @param cname Color name as a common name. If a hex string is supplied,
#'  it will be returned unchanged.
#'
#' @return The HEX string representing the color
#'
#' @export
col2hex <- function(cname){

  col_mat <- col2rgb(cname)

  rgb(red = col_mat[1, ] / 255,
      green = col_mat[2, ] / 255,
      blue = col_mat[3, ] / 255)
}