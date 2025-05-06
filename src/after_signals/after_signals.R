box::use(
  cs = src / utils / cloud_storage,
  src/indicators/idmc_displacement/utils/raw_displacement,
  src/indicators/idmc_displacement/utils/wrangle_displacement,
  src/indicators/idmc_displacement/utils/plot_displacement,
  src/indicators/acled_conflict/utils/raw_conflict,
  src/indicators/acled_conflict/utils/wrangle_conflict,
  src/indicators/acled_conflict/utils/plot_conflict,
)

box::use(
  dplyr,
  gg = ggplot2
)

df_signals <- cs$read_az_file("output/signals.parquet")
indicator <- "acled_conflict"
displacement_type_filter <- "Disaster"
indicators <- unique(df_signals$indicator_id)
if (indicator %in% c("idmc_displacement_disaster", "idmc_displacement_conflict")){
  data <- raw_displacement$raw() |>
    dplyr$filter(displacement_type == displacement_type_filter)
  data <- wrangle_displacement$wrangle(data)
  alerts <- df_signals |>
    dplyr$filter(
      indicator_id == indicator,
    )
  plot_fn <- plot_displacement$displacement_ts
} else {
  data <- raw_conflict$raw()
  data <- wrangle_conflict$wrangle(data)
  alerts <- df_signals |>
    dplyr$filter(
      indicator_id == indicator,
      Sys.Date()- date <= 365 * 5
    )
  plot_fn <- plot_conflict$conflict_ts
}
alerts <- alerts |>
  dplyr$filter(
    iso3 =="MMR"
  )
for (country in unique(alerts$iso3)){
  country_data <- data |>
    dplyr$filter(iso3 == country)
  alerts_data <- alerts |>
    dplyr$filter(iso3 == country)

  plot <- plot_fn(country_data, data, paste0(country, " - After the Signals"), Sys.Date(), alerts=alerts_data)
  gg$ggsave(
    plot,
    filename = paste0(indicator, "_", country, ".png"),
  )

}
