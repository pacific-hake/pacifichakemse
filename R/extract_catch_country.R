#' Extract the catch by country using the assessment landings file
#'
#' @param data_csv_dir The directory in which the assessment csv files containing catch by month are located
#'
#' @return A data frame with three columns, the year, the Canadian catch and the US catch
#' @export
#' @importFrom readr read_csv
extract_catch_country <- function(data_csv_dir = NULL){

  fn <- file.path(data_csv_dir, "landings-tac-history.csv")
  if(!file.exists(fn)){
    cat(red(symbol$cross),
        red(paste0("The data file ", fn, " does not exist. catch_country object is NULL\n")))
    return(NULL)
  }
  d <- read_csv(fn, col_types = cols())

  can <- d %>%
    select(Year, CAN_forgn, CAN_JV, CAN_Shoreside) %>%
    mutate(can = CAN_forgn + CAN_JV + CAN_Shoreside) %>%
    select(year = Year, can)
  usa <- d %>%
    select(Year, US_foreign, US_JV, atSea_US_MS, atSea_US_CP, US_shore, USresearch) %>%
    mutate(usa = US_foreign + US_JV + atSea_US_MS + atSea_US_CP + US_shore + USresearch) %>%
    select(year = Year, usa)

  j <- can %>%
    left_join(usa, by = "year")

  # TODO: This is hardwired to try to match the output of Nis' code. The data in the catch_per_country.csv
  # file are the same as in the landings-tac-history.csv file up to 2005, when they diverge. Nis used
  # catch_per_country.csv. To return to correct, comment the statement below out and return j
  k <- read_csv(system.file("extdata/csv-data/catch_per_country.csv",
                                        package = "pacifichakemse",
                                        mustWork = TRUE),
                            col_types = cols()) %>%
    rename(can = Can, us = US) %>%
    select(year, can, us)

  #df$Catch.country <- as.matrix(Catch.country[,2:3])[,c(2,1)]
  as.matrix(k)
}