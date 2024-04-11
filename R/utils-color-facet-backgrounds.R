#' Color the backgrounds of the facet labels in a ggplot object
#'
#' @param g The ggplot object
#' @param facet_back_cols A vector of the colors to apply to the facet
#'  backgrounds
#' @param facet_back_alpha transparency between 0 and 99
#'
#' @return The modified ggplot object
#'
#' @export
color_facet_backgrounds <- function(g = NULL,
                                    facet_back_cols = NULL,
                                    facet_back_alpha = 99){

  if(facet_back_alpha < 0){
    facet_back_alpha <- 0
  }
  if(facet_back_alpha > 99){
    facet_back_alpha <- 99
  }
  if(facet_back_alpha < 10){
    # Needs to be two digits for the alpha string
    facet_back_alpha <- paste0("0", facet_back_alpha)
  }

  # Add scenario colors to the strip backgrounds
  gt <- ggplot_gtable(ggplot_build(g))
  strip <- which(grepl("strip-t", gt$layout$name))
  if(!length(strip)){
    warning("The ggplot object does not contain facets, returning the ",
            "original object")
    return(g)
  }
  if(length(facet_back_cols) < length(strip)){
    warning("The facet_back_cols vector is shorter than the number of ",
            "facet backgrounds. Recycling the colors")
    facet_back_cols <- rep(facet_back_cols,
                           length(strip) %/% length(facet_back_cols) + 1)
  }
  # Need to re-order gt$layout$name[strip] as they are out of order and have
  #  the format strip-t-1-2 etc
  # This is accomplished by reversing the numbers in the ordered strip text
  #  eg. strip-t-1-2 becomes strip-t-2-1
  # If this swapping leads to strip text which doesn't exist in the grob
  # layout, use the original text instead.
  swap_rows_cols <- function(vec){
    out <- NULL
    for(i in seq_along(vec)){
      out[i] <- gsub("strip-t-([1-9]+)-([1-9]+)", "strip-t-\\2-\\1", vec[i])
    }
    out
  }
  tmp_layout_name <- sort(gt$layout$name[strip])
  strip_tmp <- match(tmp_layout_name, gt$layout$name)
  tmp_layout_name <- swap_rows_cols(tmp_layout_name)
  strip <- match(tmp_layout_name, gt$layout$name)
  if(any(is.na(strip))){
    strip <- strip_tmp
  }
  facet_back_cols <- facet_back_cols[seq_along(strip)]

  k <- 1
  for(i in strip){
    j <- which(grepl('rect', gt$grobs[[i]]$grobs[[1]]$childrenOrder))
    gt$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <-
      paste0(col2hex(facet_back_cols[k]), facet_back_alpha)
    k <- k + 1
  }

  grid.draw(gt)
}

