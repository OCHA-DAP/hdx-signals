box::use(
  mockery[...],
  testthat[...],
  withr[...]
)

.on_load <- function (ns) {
  test_dir(box::file())
}

box::export()
