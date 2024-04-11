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
extract_catch_country <- function(ss_model_data_csv_dir = NULL,
                                  ...){

  fn <- file.path(ss_model_data_csv_dir, hake::landings_tac_fn)

  if(!files_exist(fn)){
    cat(red(symbol$cross),
        red(paste0("The data file:\n`", fn, "`\n does not exist. ",
                   "`catch_country` object is NULL")))
    return(NULL)
  }
  d <- read_csv(fn, col_types = cols())

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