box::use(src/utils/cloud_storage)

impl <- attr(cloud_storage, "namespace")

test_that("test that read_az_file works properly", {
  stub(read_az_file, "get_container", mock())
  stub(read_az_file, "az$download_blob", mock())
  mock_read_geojson <- mock()
  mock_read_csv <- mock()
  stub(read_az_file, "sf$st_read", mock_read_geojson)
  stub(read_az_file, "readr$read_csv", mock_read_csv)

  read_az_file("data.geojson")
  expect_called(mock_read_geojson, 1)

  read_az_file("data.csv")
  expect_called(mock_read_csv, 1)
})

test_that("update_az_file works properly", {
  stub(update_az_file, "get_container", \(x) x)
  stub(update_az_file, "tempfile", \(fileext) paste0("file", fileext))

  mock_csv <- mock()
  mock_parquet <- mock()
  mock_json <- mock()
  mock_upload_blob <- mock()

  stub(update_az_file, "readr$write_csv", mock_csv)
  stub(update_az_file, "arrow$write_parquet", mock_parquet)
  stub(update_az_file, "jsonlite$write_json", mock_json)
  stub(update_az_file, "az$upload_blob", mock_upload_blob)

  with_envvar(new = c(HS_LOCAL = TRUE), {
    update_az_file(df = mtcars, name = "a.csv", container = "prod")
    expect_called(mock_upload_blob, 0)
    expect_called(mock_csv, 1)
  })

  with_envvar(new = c(HS_LOCAL = FALSE), {
    update_az_file(df = mtcars, name = "b.parquet", container = "prod")
    expect_args(mock_upload_blob, 1, container = "prod", src = "file.parquet", name = "b.parquet")
    expect_called(mock_parquet, 1)

    update_az_file(df = mtcars, name = "output/c.json", container = "dev")
    expect_args(mock_upload_blob, 2, container = "dev", src = "file.json", name = "output/c.json")
    expect_called(mock_json, 1)
  })
})

test_that("az_file_detect works properly", {
  stub(az_file_detect, "get_container", \(x) x)
  mock_list_blobs <- mock(
    data.frame(
      name = c("input", "a.json", "b.csv", "input/c.json"),
      size = c(NA, 1:3),
      isdir = c(TRUE, FALSE, FALSE, FALSE),
      blobtype = "BlockBlob"
    ),
    cycle = TRUE
  )

  stub(az_file_detect, "az$list_blobs", mock_list_blobs)

  expect_equal(
    az_file_detect(),
    c("a.json", "b.csv", "input/c.json")
  )

  expect_equal(
    az_file_detect("csv$"),
    "b.csv"
  )

  expect_equal(
    az_file_detect("^a", "dev"),
    "a.json"
  )

  expect_args(mock_list_blobs, n = 3, container = "dev")
})


test_that("signals_path is calculated correctly", {
  expect_equal(
    signals_path("id_a", FALSE),
    "output/id_a/signals.parquet"
  )

  expect_equal(
    signals_path("id_b", TRUE),
    "output/id_b/test/signals.parquet"
  )
})

test_that("test that get_container works properly", {
  mock_prod <- mock()
  mock_dev <- mock()
  mock_wfp <- mock()

  stub(impl$get_container, "container_prod", mock_prod)
  stub(impl$get_container, "container_dev", mock_dev)
  stub(impl$get_container, "container_wfp", mock_wfp)

  impl$get_container("prod")
  expect_called(mock_prod, 1)
  expect_called(mock_dev, 0)
  expect_called(mock_wfp, 0)

  impl$get_container("dev")
  expect_called(mock_prod, 1)
  expect_called(mock_dev, 1)
  expect_called(mock_wfp, 0)

  impl$get_container("wfp")
  expect_called(mock_prod, 1)
  expect_called(mock_dev, 1)
  expect_called(mock_wfp, 1)
})

test_that("endpoint_url is returned correctly", {
  expect_equal(
    impl$azure_endpoint_url("file", "dev"),
    "https://imb0chd0dev.file.core.windows.net/"
  )

  expect_equal(
    impl$azure_endpoint_url("blob", "prod"),
    "https://imb0chd0prod.blob.core.windows.net/"
  )
})
