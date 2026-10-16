

#' drop_read_csv
#'
#' Reads a CSV file from Dropbox directly into memory as a tibble. The Dropbox
#' response is passed to [readr::read_csv()] as a raw vector, so this function
#' does not create a local copy of the CSV.
#' @param file Name of file with full path relative to Dropbox root
#' @param dest Deprecated and ignored. Retained for compatibility with earlier
#'   versions that downloaded the CSV to this directory.
#' @param ... Additional arguments passed to [readr::read_csv()]
#' @param lazy Must remain `FALSE` so the returned tibble is fully materialised
#'   in memory and has no dependency on an external file.
#' @template token
#' @return A tibble containing the CSV data.
#' @note The tibble remains in the R session and may still be persisted if the
#'   user saves their workspace or writes the data to disk.
#' @export
#' @examples \dontrun{
#' write.csv(iris, file = "iris.csv")
#' drop_upload("iris.csv")
#' # Now let's read this back into an R session
#' new_iris <- drop_read_csv("iris.csv")
#'}
drop_read_csv <- function(
  file,
  dest = NULL,
  dtoken = get_dropbox_token(),
  ...,
  lazy = FALSE
) {
  if (!is.null(dest)) {
    warning(
      "`dest` is deprecated and ignored because CSV files are now read directly into memory.",
      call. = FALSE
    )
  }

  if (!identical(lazy, FALSE)) {
    stop("`lazy` must be FALSE for memory-only CSV reading.", call. = FALSE)
  }

  csv_raw <- drop_download_raw(file, dtoken = dtoken)
  readr::read_csv(csv_raw, ..., lazy = FALSE)
}
