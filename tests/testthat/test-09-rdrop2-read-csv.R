context("testing drop_read_csv")

test_that("drop_read_csv returns a tibble and passes arguments to read_csv", {
  source_file <- tempfile(fileext = ".csv")
  download_dir <- tempfile()
  dir.create(download_dir)
  writeLines(c("id,value", "001,10", "002,20"), source_file)
  on.exit(unlink(c(source_file, download_dir), recursive = TRUE))

  read_dropbox_csv <- drop_read_csv
  api <- new.env(parent = environment(drop_read_csv))
  api$drop_download <- function(file, local_path, ...) {
    file.copy(source_file, local_path, overwrite = TRUE)
  }
  environment(read_dropbox_csv) <- api

  result <- read_dropbox_csv(
    "folder/example.csv",
    dest = download_dir,
    dtoken = NULL,
    col_types = "ci"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$id, c("001", "002"))
  expect_equal(result$value, c(10L, 20L))
})
