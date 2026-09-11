box::use(src/signals/update_homepage_signals)

impl <- attr(update_homepage_signals, "namespace")

#' Create a signals data frame for a single location
#'
#' Built from `signals_template` so all of the signals columns are present.
signals_fixture <- function(iso3, hrp_location, alert_level, value = 100000) {
  bind_rows(
    signals_template,
    tibble(
      iso3 = iso3,
      location = iso3,
      hrp_location = hrp_location,
      alert_level = alert_level,
      value = value,
      date = as.Date("2026-08-01"),
      campaign_date = as.Date("2026-09-08"),
      campaign_url_archive = paste0("http://eepurl.com/campaign#", iso3),
      signals_version = "0.6.0.0"
    )
  )
}

#' Read a homepage signals file as character data
read_fixture <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()))
}

test_that("select_homepage_signal prefers HRP locations over high concern", {
  df <- bind_rows(
    signals_fixture("AAA", TRUE, "Medium concern"),
    signals_fixture("BBB", FALSE, "High concern")
  )
  expect_equal(impl$select_homepage_signal(df)$iso3, "AAA")
})

test_that("select_homepage_signal prefers high concern among HRP locations", {
  df <- bind_rows(
    signals_fixture("AAA", TRUE, "Medium concern"),
    signals_fixture("BBB", TRUE, "High concern"),
    signals_fixture("CCC", FALSE, "High concern")
  )
  expect_equal(impl$select_homepage_signal(df)$iso3, "BBB")
})

test_that("select_homepage_signal falls back to high concern when no HRP location", {
  df <- bind_rows(
    signals_fixture("AAA", FALSE, "Medium concern"),
    signals_fixture("BBB", NA, "High concern")
  )
  expect_equal(impl$select_homepage_signal(df)$iso3, "BBB")
})

test_that("select_homepage_signal picks at random when no preference applies", {
  df <- bind_rows(
    signals_fixture("AAA", FALSE, "Medium concern"),
    signals_fixture("BBB", NA, "Medium concern")
  )
  df_signal <- impl$select_homepage_signal(df)
  expect_equal(nrow(df_signal), 1)
  expect_true(df_signal$iso3 %in% c("AAA", "BBB"))
})

test_that("select_homepage_signal returns the only signal available", {
  df <- signals_fixture("AAA", FALSE, "Medium concern")
  expect_equal(impl$select_homepage_signal(df)$iso3, "AAA")
})

test_that("update_homepage_signals adds the new signal on top and drops the oldest", {
  path <- local_tempfile(fileext = ".csv")
  with_envvar(new = c(HS_LOCAL = FALSE), {
    for (iso3 in c("AAA", "BBB", "CCC", "DDD")) {
      update_homepage_signals$update_homepage_signals(
        df = signals_fixture(iso3, TRUE, "High concern"),
        path = path
      )
    }
  })

  df <- read_fixture(path)
  expect_equal(df$iso3, c("DDD", "CCC", "BBB"))
  expect_equal(names(df), names(signals_hdx_template))
})

test_that("update_homepage_signals writes the HDX columns and full values", {
  path <- local_tempfile(fileext = ".csv")
  with_envvar(new = c(HS_LOCAL = FALSE), {
    update_homepage_signals$update_homepage_signals(
      df = signals_fixture("AAA", TRUE, "High concern", value = 100000),
      path = path
    )
  })

  df <- read_fixture(path)
  expect_equal(nrow(df), 1)
  expect_equal(df$value, "100000")
  expect_equal(df$date, "2026-08-01")
  expect_equal(df$campaign_url, "http://eepurl.com/campaign#AAA")
})

test_that("update_homepage_signals drops the blank rows of spreadsheet exports", {
  path <- local_tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(names(signals_hdx_template), collapse = ","),
      paste(rep("", length(signals_hdx_template)), collapse = ","),
      paste(rep("", length(signals_hdx_template)), collapse = ",")
    ),
    con = path
  )

  with_envvar(new = c(HS_LOCAL = FALSE), {
    update_homepage_signals$update_homepage_signals(
      df = signals_fixture("AAA", TRUE, "High concern"),
      path = path
    )
  })

  expect_equal(read_fixture(path)$iso3, "AAA")
})

test_that("update_homepage_signals does nothing when hs_local() is TRUE", {
  path <- local_tempfile(fileext = ".csv")
  with_envvar(new = c(HS_LOCAL = TRUE), {
    update_homepage_signals$update_homepage_signals(
      df = signals_fixture("AAA", TRUE, "High concern"),
      path = path
    )
  })
  expect_false(file.exists(path))
})

test_that("read_homepage_signals errors when the columns do not match", {
  path <- local_tempfile(fileext = ".csv")
  writeLines(c("iso3,location", "AAA,Aaa"), con = path)
  expect_error(
    impl$read_homepage_signals(path),
    regexp = "do not match the HDX signals template"
  )
})
