#' Test whether or not files exist. They can be URLs or regular files
#'
#' @param fns_vec A character vector of URLs or regular full path filenames.
#' The function [base::file.exists()] will be used for filenames not
#' containing 'https://' or 'http://'
#'
#' @return A logical vector of `TRUE` and `FALSE` corresponding to
#' the `fns_lst` file
#' locations
#'
#' @export
#' @importFrom httr HEAD
files_exist <- function(fns_lst){

  http_status_ok <- 200

  map_lgl(fns_lst, ~{

    is_url <- length(grep("https?:\\/\\/", .x))
    if(is_url){
      hd <- HEAD(.x)
      status <- hd$all_headers[[1]]$status
      ret <- status == http_status_ok
    }else{
      ret <- file.exists(.x)
    }

    ret
  })
}