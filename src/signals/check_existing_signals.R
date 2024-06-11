box::use(dplyr)
box::use(stringr)

box::use(cs = ../utils/cloud_storage)
box::use(../utils/gmas_test_run)

#' Check existing signals
#'
#' Checks that the signals data frame for that specific indicator is empty,
#' if it exists. If not empty, then these still need to be triaged into
#' campaigns before generating any new alerts. Thus, an error is raised if it
#' exists or has any rows.
#'
#' If `first_run` is `TRUE`, it also checks that there is no data for the indicator
#' in `output/signals.parquet`, which is the overall dataset. If there is any data
#' for the indicator in there, an error is generated since `first_run` should only
#' be used on the initial pass.
#'
#' @param indicator_id ID of the indicator
#' @param first_run Whether or not this is the first run
#' @param fn_signals File name of the Signals data
#' @param test Whether or not this is for testing signals development. If `TRUE`,
#'     errors are not generated if data is in the overall `output/signals.parquet`
#'     file, so we can test efficiently.
#'
#' @export
check_existing_signals <- function(indicator_id, first_run, fn_signals, test) {
  if (gmas_test_run$gmas_test_run()) {
    return(invisible(NULL))
  }

  check_ind_signals(
    indicator_id = indicator_id,
    fn_signals = fn_signals
  )

  check_overall_signals(
    indicator_id = indicator_id,
    first_run = first_run,
    fn_signals = fn_signals,
    test = test
  )
}

#' Check indicator signals file
#'
#' Checks the indicator signals file, `fn_signals`, and throws error if there
#' are existing rows in `output/{indicator}/signals.parquet` file since that
#' must be triaged before overwriting.
check_ind_signals <- function(indicator_id, fn_signals) {
  if (fn_signals %in% az_files) {
    num_ind_signals <- nrow(cs$read_az_file(fn_signals))
  } else {
    num_ind_signals <- 0
  }


  # generate an error if the indicator signals are non-empty
  if (num_ind_signals > 0) {
    stop(
      stringr$str_wrap(
        paste0(
          "The ",
          indicator_id,
          " signals data ",
          fn_signals,
          " on Azure is non-empty. Please triage this data into `output/signals.parquet` ",
          "prior to generating new signals by running `triage_signals()`. "
        )
      ),
      call. = FALSE
    )
  }
}

#' Check overall signals
#'
#' Check overall signals file, `output/signals.parquet`, and throws an error
#' if there is no rows for `indicator_id` in the file but `first_run` is `FALSE`,
#' or if there are existing signals for `indicator_id` and `first_run` is `TRUE`.
#'
#' These checks are not performed if `test` is `TRUE`.
check_overall_signals <- function(indicator_id, first_run, fn_signals, test) {
  # if testing, don't run these checks
  if (test) {
    return(invisible(NULL))
  }

  # check number of confirmed signals already, doesn't matter for testing
  # calculating at module level
  if ("output/signals.parquet" %in% az_files) {
    num_signals <- df_signals |>
      dplyr$filter(
        indicator_id == !!indicator_id
      ) |>
      nrow()
  } else {
    num_signals <- 0
  }


  # if it's first run, check there are no existing signals for this indicator_id
  # that are in the final `output/signals.parquet` file
  if (first_run && num_signals > 0) {
    stop(
      stringr$str_wrap(
        paste0(
          "There are existing signals for ",
          indicator_id,
          ", so you cannot run `generate_signals()` with `first_run = TRUE`. ",
          "If you want to completely re-create the data, you must first delete all ",
          "existing signals. However, please consider carefully before doing so."
        )
      ),
      call. = FALSE
    )
  }

  # if it's not the first run and no final signals exist yet, generate an error
  if (!first_run && num_signals == 0) {
    stop(
      stringr$str_wrap(
        paste0(
          "There are no existing signals for ",
          indicator_id,
          ", so you cannot run `generate_signals()` with `first_run = FALSE`. ",
          "You must first complete the first run of signals generation before you ",
          "can begin adding existing signals to the dataset."
        )
      ),
      call. = FALSE
    )
  }
}


az_files <- cs$az_file_detect()
if ("output/signals.parquet" %in% az_files) {
  df_signals <- cs$read_az_file("output/signals.parquet")
}
