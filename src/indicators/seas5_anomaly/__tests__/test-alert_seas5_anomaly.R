# one in-season admin 1 unit per row, all qualifying (rp and skill above cut-offs)
make_unit <- function(iso3, date, trimester, season_year, lead, dry_rp = 10,
                      wet_rp = 1) {
  data.frame(
    iso3 = iso3,
    date = as.Date(date),
    trimester = trimester,
    season_year = season_year,
    lead = lead,
    dry_rp = dry_rp,
    wet_rp = wet_rp,
    pearson_r = 0.5,
    in_season_flat = TRUE
  )
}

test_that("a country alerts once per issuance, on the shortest lead", {
  df <- rbind(
    make_unit("ETH", "2026-07-01", "JAS", 2026, 0),
    make_unit("ETH", "2026-07-01", "ASO", 2026, 1),
    make_unit("ETH", "2026-07-01", "SON", 2026, 2)
  )
  alerts <- alert(df)
  expect_equal(nrow(alerts), 1)
  expect_equal(alerts$trimester, "JAS")
})

test_that("a trimester already alerted is not alerted again", {
  df <- rbind(
    make_unit("ETH", "2026-06-01", "JAS", 2026, 1),
    make_unit("ETH", "2026-07-01", "JAS", 2026, 0),
    make_unit("ETH", "2026-07-01", "ASO", 2026, 1)
  )
  alerts <- alert(df)
  expect_equal(alerts$date, as.Date(c("2026-06-01", "2026-07-01")))
  expect_equal(alerts$trimester, c("JAS", "ASO"))
})

test_that("the same trimester of another season year can alert again", {
  df <- rbind(
    make_unit("ETH", "2025-07-01", "JAS", 2025, 0),
    make_unit("ETH", "2026-07-01", "JAS", 2026, 0)
  )
  expect_equal(nrow(alert(df)), 2)
})

test_that("countries are screened independently", {
  df <- rbind(
    make_unit("ETH", "2026-07-01", "JAS", 2026, 0),
    make_unit("SOM", "2026-07-01", "JAS", 2026, 0)
  )
  expect_equal(sort(alert(df)$iso3), c("ETH", "SOM"))
})
