library(tidyverse)

walk(
  .x = file.path(
    "src",
    "plots",
    c("plot_idmc.R", "plot_ipc.R")
  ),
  .f = source
)

plot_general <- function(flag_source, iso3, country) {
  match.fun(paste0("plot_", flag_source))(
    iso3 = iso3,
    country = country
  )
}
