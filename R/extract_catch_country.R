#' Extract the catch by country using the assessment landings file
#'
#' @param data_csv_dir The directory in which the assessment csv files containing catch by month are located
#'
#' @return A data frame with three columns, the year, the Canadian catch and the US catch
#' @export
#' @importFrom readr read_csv
extract_catch_country <- function(data_csv_dir = NULL){

  verify_argument(data_csv_dir, "character", 1)

  d <- read_csv(file.path(data_csv_dir, "landings-tac-history.csv"), col_types = cols())

  can <- d %>%
    select(Year, CAN_forgn, CAN_JV, CAN_Shoreside) %>%
    mutate(can = CAN_forgn + CAN_JV + CAN_Shoreside) %>%
    select(year = Year, can)
  usa <- d %>%
    select(Year, US_foreign, US_JV, atSea_US_MS, atSea_US_CP, US_shore, USresearch) %>%
    mutate(usa = US_foreign + US_JV + atSea_US_MS + atSea_US_CP + US_shore + USresearch) %>%
    select(year = Year, usa)

  can %>%
    left_join(usa, by = "year")
}