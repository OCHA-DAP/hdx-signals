box::use(src/email/mailchimp[...])

box::use(
  httptest2[...],
  httpuv[...], # not used but necessary for req dry run
  mockery[...],
  withr[...]
)
