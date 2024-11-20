box::use(
  logger,
  dplyr,
  lubridate,
  cs = src/utils/cloud_storage
)


#' @description
#' Check if file/data is present (on blob). If present append on new rows and overwrite.
#' If file is not on blob write out file
#'
#' @param df data.frame created from mailchimp api call + wrangling
#'
#' @param file_path `character` file path.
#'
#' @export
write_appended_data <- function(
    df,
    storage_account,
    file_path ,
    run_date
    ){

  on_blob <- is_on_blob(file_path, storage_account = storage_account)

  if(!on_blob) {
    logger$log_info("No file detected: writing file")
    cs$update_az_file(
      df = df,
      name =  file_path,
      container = storage_account
    )
  }

  if(on_blob) {
    logger$log_info("File detected: updating new records")

    df_prev <- cs$read_az_file(
      name =  file_path,
      container = storage_account
    )
    df_prev <- df_prev |>
      dplyr$mutate(
        dplyr$across(
          .cols = dplyr$contains("_date"),
          .fns = \(x) lubridate$as_date(x)

        )

        # subscription_date = lubridate$as_date(subscription_date),
        # extraction_date = lubridate$as_date(extraction_date),
      )
    df_diff <- dplyr$anti_join(
      dplyr$select(df,-dplyr$any_of(c("extraction_date"))),
      dplyr$select(df_prev,-dplyr$any_of(c("extraction_date")))
    )

    df_merged_long <- dplyr$bind_rows(
      df_prev,
      df_diff |>
        dplyr$mutate(
          extraction_date = lubridate$as_date(run_date)
        )
    )
    cs$update_az_file(
      df = df_merged_long,
      name =  file_path,
      container = storage_account
    )
  }
}



#' is_on_blob
#'
#' @param file_path `character` filepath
#' @param storage_account `character` storage account name (default: `dev`)
#'
#' @return `logical` whether or not file exists
is_on_blob <-  function(file_path, storage_account){
  blob_detected <- cs$az_file_detect(
    pattern = file_path,
    container = storage_account
  )
  ifelse(length(blob_detected)>0, TRUE, FALSE)
}

