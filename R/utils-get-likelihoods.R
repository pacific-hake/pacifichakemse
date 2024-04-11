#' Get likelihood values for a model object the begin with 'ans'
#'
#' @param report The object returned by the `report()` function of the
#'  returned object of the minimizer.
#'
#' @return A list of the likelihoods whose names begin with 'ans'. Add
#'  these in pacifichakemse.cpp at the end as a REPORT
#' @export
#'
#' @examples
#' \dontrun{
#' obj <- MakeADFun(d, p, DLL = "pacifichakemse", silent = FALSE)
#' report <- obj$report()
#' get_likelihoods(report)
#' }
get_likelihoods <- function(report){

  map2(names(report), report, ~{
    if(length(grep("ans", .x))){
      ret <- .y;names(ret) <- .x;ret}}) |>
    unlist() %>%
    `[`(!is.na(names(.)))
}

