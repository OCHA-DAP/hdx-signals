# Testing hdx-signals

This document outlines how testing is setup for the package. The following workflow
generally follows the guidance provided by
[this box vignette](https://cran.rstudio.com/web/packages/box/vignettes/testing.html).

## Structure

Let's assume we have a module stored in `src/utils` that has a function `a()` defined
in `a.R`. We also have the `__init__.R` file at the top level of the module to re-export
`a()` and other functions.

├── __init__.R
├── a.R
├── __tests__
│   ├── __init__.R
|   ├── helper-module.R
│   └── test-a.R

The tests associated with the module are stored within it, in a `__tests__` folder.
In this instance, `src/utils/__tests__`.

### `__init__.R`

The `__init__.R` file for the overall module should have a set of code that will
source the test module when the file is called or sourced NOT as a module. For our
example, 

```r
if (is.null(box::name())) {
  box::use(src/utils/`__tests__`)
}
```

### `__tests__/__init__.R`

This calls the `__tests__/__init__.R` script. It should always have the following
lines of code, which ensures we load `{testthat}` and runs the tests in the `__tests__`
directory. There is also a `box::export()` which simply lets `{box}` know that it is
a module to be sourced by `box::use()`.

```r
box::use(
  testthat[...]
)

.on_load <- function (ns) {
  test_dir(box::file())
}

box::export()
```

### `__tests__/helper-module.R`

Last, the `helper-module.R` script, standard for `{testthat}`, contains code that is
run prior to running the tests for that directory. It should be used to load any
libraries or code you want for testing. For instance, we frequently use `{mockery}`,
`{withr}`, and `{httptest2}` to enhance our ability to mock functions, set environment
variables dynamically, and ensure we don't call APIs during testing.

## Coverage

Currently, the tests cover the `src/email`, `src/signals`, and `src/utils` directories.
The `src/images` and `src/repo` directories need testing, as do the individual
`src/indicators`. Testing coverage could be setup using `{covr}`, but is unclear if it
is possible using `{box}`.
