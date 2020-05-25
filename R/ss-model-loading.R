#' Create an rds file to hold an SS model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model_dir = NULL){

  verify_argument(model_dir, "character", 1)

  if(!dir.exists(model_dir)){
    stop("Error - the directory ", model_dir, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_dir, paste0(basename(model_dir), ".rds"))
  if(file.exists(rds_file)){
    unlink(rds_file, force = TRUE)
  }

  message("Creating a new RDS file in ", model_dir, "\n")

  # If this point is reached, no RDS file exists so it has to be built from scratch
  model <- load_ss_files(model_dir)

  saveRDS(model, file = rds_file)
  invisible()
}

#' Load models from files created using [create_rds_file()]
#'
#' @details Load an SS3 model from an RDS file and return as a [list]
#'
#' @param model_dirs A [vector] of model directory names
#' @param ret_single_list See details
#'
#' @return A [list] of model objects
#' @export
load_ss_model_from_rds <- function(model_dir = NULL){

  verify_argument(model_dir, "character", 1)

  model_rds_file <- file.path(model_dir, paste0(basename(model_dir), ".rds"))
  if(!file.exists(model_rds_file)){
    create_rds_file(model_dir)
  }
  readRDS(model_rds_file)
}

#' Load all the SS files for output and input, and return the model object
#'
#' @param model_dir Directory the model resides in
#' @param printstats Print info on each model loaded via [r4ss::SS_output()]
#'
#' @return A model object representing the output from the SS model
#' @importFrom r4ss SS_output SS_readdat
#' @export
load_ss_files <- function(model_dir = NULL,
                          printstats = FALSE){

  verify_argument(model_dir, "character", 1)
  verify_argument(printstats, "logical", 1)

  # Load MPD results
  model <- tryCatch({
    SS_output(dir = model_dir,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE)
  }, error = function(e){
    SS_output(dir = model_dir,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              forecast = FALSE)
  })

  ## Load the data file and control file for the model
  ## Get the file whose name contains "_data.ss" and "_control.ss"
  ## If there is not exactly one of each, stop with error.
  model_dir_listing <- dir(model_dir)
  dat_fn_ind <- grep("_data.ss", model_dir_listing)
  ctl_fn_ind <- grep("_control.ss", model_dir_listing)
  par_fn_ind <- grep("ss.par", model_dir_listing)
  if(!length(dat_fn_ind)){
    stop("Error in model ", model_dir,
         ", there is no data file. A data file is any file whose name contains the text _data.ss.\n\n",
         call. = FALSE)
  }
  if(length(dat_fn_ind) > 1){
    stop("Error in model ", model_dir,
         ", there is more than one data file. A data file is any file whose name contains the text _data.ss.\n\n",
         call. = FALSE)
  }
  if(!length(ctl_fn_ind)){
    stop("Error in model ", model_dir,
         ", there is no control file. A control file is any file whose name contains the text _control.ss.\n\n",
         call. = FALSE)
  }
  if(length(ctl_fn_ind) > 1){
    stop("Error in model ", model_dir,
         ", there is more than one control file. A control file is any file whose name contains the text _control.ss.\n\n",
         call. = FALSE)
  }
  dat_fn <- file.path(model_dir, model_dir_listing[dat_fn_ind])
  ctl_fn <- file.path(model_dir, model_dir_listing[ctl_fn_ind])
  par_fn <- file.path(model_dir, model_dir_listing[par_fn_ind])
  model$path <- model_dir
  model$dat_file <- dat_fn
  model$dat <- SS_readdat(dat_fn, verbose = FALSE)
  model$ctl_file <- ctl_fn
  model$ctl <- readLines(ctl_fn)
  model$par_file <- par_fn
  model$par <- readLines(par_fn)

  model
}

