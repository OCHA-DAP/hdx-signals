#' Script to get ACLED metadata on when country time series are complete. The
#' information is stored on a PDF URL.
box::use(pdftools)
box::use(stringr)
box::use(dplyr)

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/country_codes)

# get pdf text from the URL

pdf_string <- pdftools$pdf_text("https://acleddata.com/acleddatanew/wp-content/uploads/dlm_uploads/2019/01/ACLED_Country-and-Time-Period-coverage_updatedFeb2022.pdf",)

# convert raw string into vector to convert into data frame
string_vec <- pdf_string |>
  stringr$str_remove("\\(Keeling\\) ") |> # drop problematic keeling string first, because next regex would fail
  stringr$str_remove("Cunha\n") |> # drop this before adding it back where it belongs
  stringr$str_replace("Tristan da ", "Tristan da Cunha ") |> # add back Cunha before the confusing new line split
  stringr$str_remove("\\(Bonaire, Sint") |> # delete the Bonaire section
  stringr$str_remove("Eustatius, and Saba\\)") |>
  stringr$str_remove("Islands \\(UK\\)") |> # fix south sandwich islands
  stringr$str_replace("South Sandwich", "South Sandwich Islands") |>
  stringr$str_replace_all("\\([^)]*\\)", "  ") |> # remove extra information between parentheses
  stringr$str_remove_all(
    paste( # remove the region sections
      "East Asia\n",
      "Central Asia & the Caucasus\n",
      "South & Southeast Asia\n",
      "Europe\n",
      "Middle East\n",
      "Africa\n",
      "Latin America & the Caribbean\n",
      "North America\n",
      "Oceania\n",
      "Antarctica\n",
      sep = "|"
    )
  ) |>
  stringr$str_replace_all("\n", "  ") |> # drop all new lines
  stringr$str_split(
    pattern = "(\n)|([\\s]{2,})"
  ) |>
  unlist()

# now turn the vector into a one on one off pattern of country and date so
# we can easily convert to a data frame

string_vec <- string_vec[string_vec != ""] # drop empty values
string_vec <- string_vec[-c(1:3)] # drop headers and keep only the info

country_indices <- seq(from = 1, to = length(string_vec), by = 2)

# turn it into a data frame
df_acled_start <- dplyr$tibble(
  country = string_vec[country_indices],
  iso3 = country_codes$names_to_iso3(country),
  time_period = string_vec[country_indices + 1]
) |>
  dplyr$mutate(
    start_date = as.Date(
      x = paste("1/", time_period),
      format = "%d/%m/%Y-Present"
    )
  )


cs$update_az_file(
  df = df_acled_start,
  name = "input/acled_start.parquet"
)
