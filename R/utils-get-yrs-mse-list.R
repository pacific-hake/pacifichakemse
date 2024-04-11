#' Get a vector of the years from a list of MSEs
#'
#' @param lst A list of MSE results, constructed from code such as:
#' ls_plots <- map(fls,~{readRDS(.x)}) where fls is a list of RDS filenames
#' which have been written by [run_mses()]. See [setup_mse_plot_objects()]
#' for a closer look at how this list is constructed
#'
#' @return A vector of the years from a list of MSEs
#'
#' @export
get_yrs_mse_list <- function(lst){

  ls_all <- map(lst, ~{
    as.numeric(attributes(.x$Catch)$dimnames$year)
  })
  first_elem <- ls_all[[1]]
  are_identical <- map_lgl(ls_all, ~{
    identical(.x, first_elem)
  })
  if(!all(are_identical)){
    stop("The MSE runs in `lst` have different years: eg - ",
         "`attributes(lst[[1]]$Catch)$dimnames$year`",
         call. = FALSE)
  }

  ls_all[[1]]
}

