#' Compare two data object inputs for input into `pacifichakemse.cpp`
#'
#' @description Used while developing the code to compare old and new
#'  data being input. They need to be exactly the same
#'
#' @param d1 Data object 1
#' @param d2 Data object 2
#' @param p1 Parameter object 1
#' @param p2 Parameter object 2
#'
#' @export
compare_tmb_data <- function(d1, d2, p1, p2){
  d1 <- d1[order(names(d1))]
  d2 <- d2[order(names(d2))]

  d <- map2(d1, d2, ~{
    identical(.x, .y)
  }) |>
    map_df(~{.x}) |>
    t() |>
    as_tibble(rownames = "name", .name_repair = "minimal") |>
    rename(is_identical = 2) |>
    mutate(type = "data")

  p1 <- p1[order(names(p1))]
  p2 <- p2[order(names(p2))]
  p <- map2(p1, p2, ~{
    identical(.x, .y)
  }) |>
    map_df(~{.x}) |>
    t() |>
    as_tibble(rownames = "name", .name_repair = "minimal") |>
    rename(is_identical = 2) |>
    mutate(type = "parameter")

  bind_rows(d, p)
}

