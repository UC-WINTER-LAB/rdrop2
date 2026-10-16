context("testing drop_read_csv")

test_that("drop_read_csv returns a tibble and passes arguments to read_csv", {
  read_dropbox_csv <- drop_read_csv
  api <- new.env(parent = environment(drop_read_csv))
  api$drop_download_raw <- function(file, dtoken) {
    charToRaw("id,value\n001,10\n002,20\n")
  }
  environment(read_dropbox_csv) <- api

  result <- read_dropbox_csv(
    "folder/example.csv",
    dtoken = NULL,
    col_types = "ci"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$id, c("001", "002"))
  expect_equal(result$value, c(10L, 20L))
})

test_that("drop_read_csv does not create a file in the deprecated destination", {
  download_dir <- tempfile()
  dir.create(download_dir)
  on.exit(unlink(download_dir, recursive = TRUE))

  read_dropbox_csv <- drop_read_csv
  api <- new.env(parent = environment(drop_read_csv))
  api$drop_download_raw <- function(file, dtoken) charToRaw("value\n1\n")
  environment(read_dropbox_csv) <- api

  expect_warning(
    result <- read_dropbox_csv(
      "folder/example.csv",
      dest = download_dir,
      dtoken = NULL,
      show_col_types = FALSE
    ),
    "`dest` is deprecated and ignored"
  )

  expect_s3_class(result, "tbl_df")
  expect_length(list.files(download_dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("drop_read_csv requires eager parsing", {
  expect_error(
    drop_read_csv("folder/example.csv", dtoken = NULL, lazy = TRUE),
    "`lazy` must be FALSE"
  )
})
