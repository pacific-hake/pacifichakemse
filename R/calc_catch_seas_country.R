#' Calculate the catch proportions by season and country as an average of the last N years
#'
#' @param data_csv_dir The directory in which the assessment csv files containing catch by month are located
#' @param n_yrs The number of year into the past to use in the calculations
#' @param weight A vector of 4 for the weights given to each season
#'
#' @return A data frame with two rows, one for each country, and 4 columns, one for
#' each season. Canada will be the first row and the US the second
#' @export
#' @importFrom readr read_csv
calc_catch_seas_country <- function(data_csv_dir = NULL,
                                    n_yrs = 10,
                                    weight = c(1, 1, 1, 1)){

  verify_argument(data_csv_dir, "character", 1)
  verify_argument(n_yrs, c("integer", "numeric"), 1)
  verify_argument(weight, c("integer", "numeric"), 4)

  helper <- function(d){
    d_seas1 <- d %>%
      select(year, `1`, `2`, `3`) %>%
      mutate(seas1 = rowSums(select(., -1))) %>%
      select(year, seas1)
    d_seas2 <- d %>%
      select(year, `4`, `5`, `6`) %>%
      mutate(seas2 = rowSums(select(., -1))) %>%
      select(year, seas2)
    d_seas3 <- d %>%
      select(year, `7`, `8`, `9`) %>%
      mutate(seas3 = rowSums(select(., -1))) %>%
      select(year, seas3)
    d_seas4 <- d %>%
      select(year, `10`, `11`, `12`) %>%
      mutate(seas4 = rowSums(select(., -1))) %>%
      select(year, seas4)
    d <- d_seas1 %>%
      left_join(d_seas2, by = "year") %>%
      left_join(d_seas3, by = "year") %>%
      left_join(d_seas4, by = "year")
    d[, -1] <- d[, -1] * weight
    d %>%
      mutate(tot = seas1 + seas2 + seas3 + seas4) %>%
      mutate_at(.vars = vars(-c(year, tot)), .funs = ~{.x / tot}) %>%
      select(-tot) %>% tail(n_yrs) %>%
      summarize_at(.vars = vars(-year), .funs = mean)
  }

  helper_load_file <- function(fn){
    fn <- file.path(data_csv_dir, fn)
    if(!file.exists(fn)){
      cat(red(symbol$cross),
          red(paste0("The data file ", fn, " does not exist.\n")))
      return(NULL)
    }
    read_csv(fn, col_types = cols())
  }
  fns_can <- list("can-ft-catch-by-month.csv",
                  "can-ss-catch-by-month.csv")
  fns_usa <- list("us-cp-catch-by-month.csv",
                  "us-ms-catch-by-month.csv",
                  "us-shore-catch-by-month.csv",
                  "us-research-catch-by-month.csv")
  df_can <- map(fns_can, helper_load_file) %>%
    map_dfr(~{.x}) %>%
    group_by(year) %>%
    summarize_all(sum) %>%
    ungroup()
  df_can <- helper(df_can)

  # USA data is in long format, so use dcast() first to make it the same as the Canadian data
  df_usa <- map(fns_usa, helper_load_file) %>%
    map_dfr(~{.x}) %>%
    group_by(year, month) %>%
    summarize(catch = sum(catch)) %>%
    ungroup() %>%
    dcast(year ~ month, value.var = "catch") %>%
    as_tibble()
  df_usa[is.na(df_usa)] <- 0
  df_usa <- helper(df_usa)

  j <- map_df(list(can = df_can, usa = df_usa), ~{.x})
  # TODO: This is hardwired to try to match code written by Ian. He used 2011 data file for
  # values up to 2010. It is likely the newer data file has slightly different values.
  # To return to correct, comment the statement below out and return j
  k <- tibble(can = c(0.001,0.188,0.603,0.208),
              usa = c(0.000,0.317,0.382,0.302)/sum(c(0.000,0.317,0.382,0.302))) %>% t() %>%
    as_tibble(.names_repair = "unique")
  names(k) <- c("seas1", "seas2", "seas3", "seas4")

  k
}