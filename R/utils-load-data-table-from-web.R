#' Read in a data table from a CSV data file at a URL on the web
#'
#' @param url The full URL of the data file including the name of the file
#' and the .csv extension
#'
#' @return The data table as a [tibble::tibble] or `NULL` if there was a
#' problem with the URL or loading from it
#' @export
load_data_table_from_web <- function(url){

  d <- tryCatch({
    request(url) |>
      req_perform() |>
      resp_body_string() |>
      read_csv()
  },
  error = function(e){
    cat(red(symbol$cross),
        red(paste0("The data file located at the following URL does not ",
                   "exist. Check the URL or table name in a web browser ",
                   "and try again.\n", url, "\n", "`catch_country` object ",
                   "is NULL\n\n")))
    NULL
  })

  d
}