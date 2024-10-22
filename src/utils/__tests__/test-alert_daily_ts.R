test_that("alert_daily_ts errors are caught", {
  expect_error( # val column doesn't exist
    alert_daily_ts(
      df = data.frame(
        iso3 = "A",
        value = 1.0,
        date = as.Date("2020-01-01")
      ),
      val_col = "val",
      min_val = 0
    )
  )

  expect_error( # no date or iso3 column
    alert_daily_ts(
      df = data.frame(
        iso3_whoops = "A",
        value = 1.0,
        date = as.Date("2020-01-01")
      ),
      val_col = "value",
      min_val = 0
    )
  )

  expect_error( # not a daily time series
    alert_daily_ts(
      df = data.frame(
        iso3 = "A",
        value = as.numeric(1:12),
        date = seq.Date(
          from = as.Date("2000-01-01"),
          to = as.Date("2000-12-01"),
          by = "month"
        )
      ),
      val_col = "value",
      min_val = 0
    )
  )

  expect_error( # integer value for `value`
    alert_daily_ts(
      df = data.frame(
        iso3 = "A",
        value = 1:12,
        date = seq.Date(
          from = as.Date("2000-01-01"),
          to = as.Date("2000-12-01"),
          by = "day"
        )
      ),
      val_col = "value",
      min_val = 0
    )
  )

  expect_error( # date class is not date
    alert_daily_ts(
      df = data.frame(
        iso3 = "A",
        value = 1,
        date = "2020-01-01"
      ),
      val_col = "value",
      min_val = 0
    )
  )
})

test_that("alert_daily_ts works properly", {
  expect_equal( # empty if less than 1 year of data (no possible alerts)
    alert_daily_ts(
      df = data.frame(
        iso3 = "A",
        value = as.numeric(1:336),
        date = seq.Date(
          from = as.Date("2000-01-01"),
          to = as.Date("2000-12-01"),
          by = "day"
        )
      ),
      val_col = "value",
      min_val = 10
    ) |> nrow(),
    0
  )

  expect_equal( # should generate two alerts, on the last day
    alert_daily_ts(
      df = data.frame(
        iso3 = "A",
        value = as.numeric(c(1:366, 1:365, 1:365)),
        date = seq.Date(
          from = as.Date("2009-01-01"),
          to = as.Date("2012-01-01"),
          by = "day"
        )
      ),
      val_col = "value",
      min_val = 100
    ) |> nrow(),
    1
  )
})
