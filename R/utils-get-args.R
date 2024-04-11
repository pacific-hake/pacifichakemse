#' Get the arguments of the calling function
#'
#' @return a list of the arguments
#'
#' @export
get_args <- function(){

  cl <- sys.call(-1)
  f <- get(as.character(cl[[1]]), mode = "function", sys.frame(-2))
  cl <- match.call(definition = f, call = cl)
  as.list(cl)[-1]
}

