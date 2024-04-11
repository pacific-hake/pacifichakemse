#' Extract the vectors from a list into a [tibble::tibble()].
#' Used in [fetch_extra_mcmc()]
#'
#' @param reps_lst A list of vectors, all the same length and structure,
#' typically extracted as a portion of a Report.sso file
#' @param header A vector of column names for the new table
#'
#' @return A [tibble::tibble()] representing one row for each of the list
#'  elements found in `reps_lst`. A new column called `Iter` is prepended and
#'  represents the list element number that the data for each row came from.
#'  List elements that are NA will not be included in the table.
extract_rep_table <- function(reps_lst, header){

  lst <- map2(reps_lst, 1:length(reps_lst), ~{
    if(is.na(.x[1])){
      return(NULL)
    }
    vecs <- str_split(.x, " +")
    vec_lengths <- map_int(vecs, ~{length(.x)})
    vec_maxlength <- max(vec_lengths)
    vecs <- map(vecs, ~{
      length(.x) <- vec_maxlength
      .x
    })
    tab <- do.call(rbind, vecs) |>
      as_tibble()
    names(tab) <- header
    tab |>
      add_column(Iter = .y, .before = 1)
  })

  do.call(rbind, lst) |>
    as_tibble(.name_repair = "minimal")
}

