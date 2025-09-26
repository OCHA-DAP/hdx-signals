box::use(
  src/utils/cloud_storage
)

#' Read manually scraped info dataset
#' @returns dataframe if available
#' @export
read_manual_info <- function() {
  name <- "input/manual_context_info.csv"
  container <- "dev"

  # Try to read the file if it exists already
  tryCatch({
    data <- cloud_storage$read_az_file(name, container)
  }, error = function(e) {
    data <- NULL
  })

  return(data)
}
