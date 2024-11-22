test_that("parse_pdf returns NA_character_ on error", {
  stub(parse_pdf, "pdftools$pdf_text", \(x) stop())
  expect_equal(
    parse_pdf("a"),
    NA_character_
  )
})

test_that("parse_pdf tries to read URL", {
  mock_pdf_text <- mock(c("a", "b", "c"))
  stub(parse_pdf, "pdftools$pdf_text", mock_pdf_text)
  expect_equal(
    parse_pdf("url"),
    "a\nb\nc"
  )
  expect_args(mock_pdf_text, 1, "url")
})
