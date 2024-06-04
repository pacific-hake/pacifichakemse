#' Extract the catch by country using the assessment landings file
#'
#' @param ss_model_data_csv_dir The directory in which the assessment csv
#' files containing catch by month are located. Can be a URL
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A data frame with three columns, the year, the Canadian catch
#' and the US catch
#'
#' @export
extract_catch_country <- function(data_tables_url = NULL,
                                  ...){

  url <- file.path(data_tables_url,
                   hake::landings_tac_fn)

  d <- load_data_table_from_web(url)

  if(is.null(d)){
    return(NULL)
  }

  can <- d |>
    mutate(can = `Canada Foreign` +
             `Canada Joint-venture` +
             `Canada Shoreside` +
             `Canada Freezer-trawler`) |>
    select(year = Year, can)

  usa <- d |>
    mutate(usa = `U.S. Foreign` +
             `U.S. Joint-venture` +
             `U.S. Mothership` +
             `U.S. Catcher-processor` +
             `U.S. Shore-based` +
             `U.S. Research`) |>
    select(year = Year, usa)

  out <- can |>
    left_join(usa, by = "year")

  out
}