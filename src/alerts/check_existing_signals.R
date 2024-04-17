box::use(dplyr)
box::use(stringr)

box::use(cs = ../utils/cloud_storage)

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
#' @param overwrite_content Whether or not to overwrite the content of existing rows
#'     in a `signals.parquet` file, rather than generating a new one.
check_existing_signals <- function(indicator_id, first_run, overwrite_content) {
  az_files <- cs$az_file_detect()
  fn_ind_signals <- paste0("output/", indicator_id, "/signals.parquet")
  if (fn_ind_signals %in% az_files) {
    num_ind_signals <- nrow(cs$read_az_file(fn_ind_signals))
  } else {
    num_ind_signals <- 0
  }


  # generate an error if the indicator signals are non-empty
  if (num_ind_signals > 0 && !overwrite_content) {
    stop(
      stringr$str_wrap(
        paste0(
          "The ",
          indicator_id,
          " signals data ",
          fn_ind_signals,
          " on Azure is non-empty. Please triage this data into `output/signals.parquet` ",
          "prior to generating new signals by running `triage_signals()`. ",
          "If you want to re-create content on this ",
          "existing dataset, then you should run ",
          "`generate_signals(..., overwrite_content = TRUE)`."
        )
      ),
      call. = FALSE
    )
  }

  # generate an error if the indicator signals is empty but we said `overwrite_content = TRUE`
  if (num_ind_signals == 0 && overwrite_content) {
    stop(
      stringr$str_wrap(
        paste0(
          "The ",
          indicator_id,
          " signals data ",
          fn_ind_signals,
          " on Azure is empty. You specified `overwrite_content = TRUE` but there ",
          "is no existing alerts or content to overwrite. "
        )
      ),
      call. = FALSE
    )
  }

  if ("output/signals.parquet" %in% az_files) {
    num_signals <- cs$read_az_file("output/signals.parquet") |>
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

  # if it's not the first run and no final signals exist yet, generate an error as well
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

