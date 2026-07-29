box::use(src/utils/push_google_drive)

impl <- attr(push_google_drive, "namespace")

test_that("push_google_drive skips upload when hs_local() is TRUE", {
  stub(impl$push_google_drive, "hs_local$hs_local", \() TRUE)

  mock_auth <- mock()
  stub(impl$push_google_drive, "gdrive_auth", mock_auth)

  impl$push_google_drive(mtcars, "a.csv")
  expect_called(mock_auth, 0)
})

test_that("push_google_drive updates an existing file in place", {
  stub(impl$push_google_drive, "hs_local$hs_local", \() FALSE)
  stub(impl$push_google_drive, "gdrive_auth", mock())
  stub(impl$push_google_drive, "temp_file$temp_file", \(fileext) paste0("file", fileext))
  stub(impl$push_google_drive, "readr$write_csv", mock())
  stub(impl$push_google_drive, "googledrive$as_id", \(x) x)

  stub(
    impl$push_google_drive, "googledrive$drive_ls",
    \(path, pattern) data.frame(id = "existing_id", name = "a.csv")
  )
  mock_update <- mock()
  mock_upload <- mock()
  stub(impl$push_google_drive, "googledrive$drive_update", mock_update)
  stub(impl$push_google_drive, "googledrive$drive_upload", mock_upload)

  with_envvar(new = c(HS_GDRIVE_FOLDER_ID = "folder123"), {
    impl$push_google_drive(mtcars, "a.csv")
  })

  expect_called(mock_update, 1)
  expect_args(mock_update, 1, file = "existing_id", media = "file.csv")
  expect_called(mock_upload, 0)
})

test_that("push_google_drive uploads a new file when none exists", {
  stub(impl$push_google_drive, "hs_local$hs_local", \() FALSE)
  stub(impl$push_google_drive, "gdrive_auth", mock())
  stub(impl$push_google_drive, "temp_file$temp_file", \(fileext) paste0("file", fileext))
  stub(impl$push_google_drive, "readr$write_csv", mock())
  stub(impl$push_google_drive, "googledrive$as_id", \(x) x)

  stub(
    impl$push_google_drive, "googledrive$drive_ls",
    \(path, pattern) data.frame(id = character(0), name = character(0))
  )
  mock_update <- mock()
  mock_upload <- mock()
  stub(impl$push_google_drive, "googledrive$drive_update", mock_update)
  stub(impl$push_google_drive, "googledrive$drive_upload", mock_upload)

  with_envvar(new = c(HS_GDRIVE_FOLDER_ID = "folder123"), {
    impl$push_google_drive(mtcars, "b.csv")
  })

  expect_called(mock_upload, 1)
  expect_args(mock_upload, 1, media = "file.csv", path = "folder123", name = "b.csv")
  expect_called(mock_update, 0)
})
