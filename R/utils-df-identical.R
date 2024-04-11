#' Are matrices identical
#'
#' @param wa1 waa matrix 1
#' @param wa2  waa matrix 2
#' @param nm_wa1 name of waa matrix 1
#' @param nm_wa2 name of waa matrix 2
#' @param diff_tol Tolerance in the numbers for identical
#'
#' @return `TRUE` or `FALSE`
df_identical <- function(wa1, wa2, nm_wa1, nm_wa2, diff_tol = 1e-20){

  if(class(wa1) != "matrix" || class(wa2) != "matrix"){
    stop("Both ", nm_wa1, " and ", nm_wa2, " must be class matrix",
         call. = FALSE)
  }
  if(!identical(dim(wa1), dim(wa2))){
    stop(nm_wa1, " not identical to ", nm_wa2, ". They have different ",
         "dimensions",
         call. = FALSE)
  }
  #if(identical(wa1, wa2)){
  #  message(nm_wa1, " is identical to ", nm_wa2, "\n")
  #}
  wa1 <- as.data.frame(wa1)
  wa2 <- as.data.frame(wa2)
  wa_cols_ident <- map_lgl(seq_len(ncol(wa1)), ~{
    if(all(is.na(wa1[,.x]))){
      if(all(is.na(wa2[,.x]))){
        return(TRUE)
      }
      return(FALSE)
    }
    diff <- wa1[,.x] |>
      as_tibble() |>
      add_column(wa2[, .x]) %>%
      mutate(diff = .[[1]] - .[[2]]) |>
      mutate(in_tol = diff < diff_tol)
    if(all(diff$in_tol)){
      return(TRUE)
    }
    FALSE
  })

  if(!all(wa_cols_ident)){
    stop(nm_wa1, " not identical to ", nm_wa2, ". These columns are not ",
         "identical: ", paste(which(!wa_cols_ident), collapse = " "),
         call. = FALSE)
  }
}


