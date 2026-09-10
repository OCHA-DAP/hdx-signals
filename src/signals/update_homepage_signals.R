box::use(
  dplyr,
  logger,
  readr
)

box::use(
  src/signals/template_data,
  src/utils/hs_local
)

#' Update the signals file displayed on the HDX homepage
#'
#' Maintains the rolling window of the latest signals displayed on the HDX
#' homepage. A single signal is selected from the campaigns approved in the
#' triage with `select_homepage_signal()`, added as the first row after the
#' header, and the oldest signal is dropped so only `n_signals` remain.
#'
#' Unlike the other signals outputs, this is a local repository file and not
#' stored on Azure, so the updated CSV has to be committed for the homepage to
#' display the new signal. Nothing is written if `hs_local()` is `TRUE`, matching
#' the behaviour of `update_az_file()`, because approved signals are not stored
#' in local runs.
#'
#' The signals already in the file are read and written back as character data so
#' they are preserved exactly as they were, and the blank rows that spreadsheet
#' exports pad the file with are dropped.
#'
#' @param df Signals data frame of the campaigns approved in the triage, with the
#'     columns of `signals_template`.
#' @param path Path to the homepage CSV file, relative to the repository root.
#' @param n_signals Number of signals displayed on the homepage.
#'
#' @returns Nothing, but the CSV file at `path` is updated.
#'
#' @export
update_homepage_signals <- function(df,
                                    path = "output/hdx_signals_latest3.csv",
                                    n_signals = 3) {
  if (hs_local$hs_local()) {
    logger$log_debug(
      "`update_homepage_signals()` not saving data as `hs_local()` is `TRUE`. ",
      "Set `HS_LOCAL` env variable to `FALSE` if you want the file to be updated."
    )
    return(invisible(NULL))
  }

  df_signal <- df |>
    select_homepage_signal() |>
    template_data$format_signals_hdx() |>
    as_character_signals()

  df_signal |>
    dplyr$bind_rows(
      read_homepage_signals(path)
    ) |>
    dplyr$slice_head(
      n = n_signals
    ) |>
    readr$write_csv(
      file = path,
      na = ""
    )
}

#' Select the single signal to display on the HDX homepage
#'
#' A campaign can cover multiple locations, so the approved signals are narrowed
#' down by preferring, in order, locations with an HRP and then `"High concern"`
#' alerts. Each preference is only applied if it leaves at least one signal, and
#' one of the remaining signals is picked at random.
#'
#' @param df Signals data frame of the campaigns approved in the triage.
#'
#' @returns Signals data frame of a single signal.
select_homepage_signal <- function(df) {
  df_hrp <- prefer_signals(
    df = df,
    df_preferred = dplyr$filter(df, hrp_location)
  )

  df_high <- prefer_signals(
    df = df_hrp,
    df_preferred = dplyr$filter(df_hrp, alert_level == "High concern")
  )

  dplyr$slice_sample(df_high, n = 1)
}

#' Keep the preferred signals only if there are any
#'
#' @param df Signals data frame the preference was applied to.
#' @param df_preferred Signals data frame left by the preference.
#'
#' @returns `df_preferred`, unless it is empty, in which case the preference
#'     cannot be applied and `df` is returned unchanged.
prefer_signals <- function(df, df_preferred) {
  if (nrow(df_preferred) == 0) {
    return(df)
  }
  df_preferred
}

#' Read the signals currently displayed on the HDX homepage
#'
#' Reads the signals as character data, dropping the blank rows that spreadsheet
#' exports pad the file with. `NULL` is returned if the file does not exist yet.
#' An error is generated if the columns do not match the HDX signals template,
#' since the file would no longer be readable by the homepage.
#'
#' @param path Path to the homepage CSV file.
#'
#' @returns Signals data frame of character columns, or `NULL`.
read_homepage_signals <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  df <- readr$read_csv(
    file = path,
    col_types = readr$cols(.default = readr$col_character())
  )

  hdx_columns <- names(template_data$signals_hdx_template)
  if (!identical(names(df), hdx_columns)) {
    stop(
      paste0(
        "The columns in '",
        path,
        "' do not match the HDX signals template. Fix the file so it has ",
        "exactly these columns before triaging: ",
        paste(hdx_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  dplyr$filter(
    df,
    dplyr$if_any(dplyr$everything(), \(x) !is.na(x))
  )
}

#' Convert all signals columns to character
#'
#' Converts the columns so the new signal can be bound to the signals already in
#' the file, which are read as character. Numeric columns are formatted without
#' scientific notation so large values, such as displacement figures, are written
#' out in full.
#'
#' @param df Signals data frame.
#'
#' @returns Signals data frame of character columns.
as_character_signals <- function(df) {
  dplyr$mutate(
    df,
    dplyr$across(
      .cols = dplyr$everything(),
      .fns = format_signals_column
    )
  )
}

#' Format a single signals column as character
#'
#' @param x Column to format.
#'
#' @returns Character vector, with missing values kept missing.
format_signals_column <- function(x) {
  if (!is.numeric(x)) {
    return(as.character(x))
  }
  x_character <- format(x, scientific = FALSE, trim = TRUE)
  x_character[is.na(x)] <- NA_character_
  x_character
}
