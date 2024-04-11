#' Append objects to list
#'
#' @param lst The list to append to
#' @param ... The objects to append
#'
#' @return The modified list
append_objs_to_list <- function(lst = NULL,
                                ...){
  ellipsis <- list(...)
  arg_names <- get_args()[-1]
  nms <- map_chr(arg_names, ~{
    nm <- .x |> as.character() |> tail(1)
  })
  names(ellipsis) <- nms
  lst <- c(lst, ellipsis)
  new_lst_names <- names(lst)[names(lst) != ""]
  if(length(unique(new_lst_names)) != length(new_lst_names)){
    stop("List contains multiple elements with the same name. The ",
         "duplicated names are:\n",
         paste(new_lst_names[duplicated(new_lst_names)], collapse = " "),
         call. = FALSE)
  }

  lst
}

