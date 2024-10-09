box::use(
  testthat[...],
  mockery[...],
  httptest2[...],
  withr[...]
)

.on_load <- function (ns) {
  test_dir(box::file())
}

box::export()
