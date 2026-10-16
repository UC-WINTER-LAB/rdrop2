

#' drop_read_csv
#'
#' A lightweight wrapper around [readr::read_csv()] to read CSV files from
#' Dropbox into memory as a tibble.
#' @param file Name of file with full path relative to Dropbox root
#' @param dest A temporary directory where a CSV file is downloaded before
#'   being read into memory
#' @param ... Additional arguments passed to [readr::read_csv()]
#' @template token
#' @return A tibble containing the CSV data.
#' @export
#' @examples \dontrun{
#' write.csv(iris, file = "iris.csv")
#' drop_upload("iris.csv")
#' # Now let's read this back into an R session
#' new_iris <- drop_read_csv("iris.csv")
#'}
drop_read_csv <- function(file, dest = tempdir(), dtoken = get_dropbox_token(), ...) {
    localfile <- file.path(dest, basename(file))
    drop_download(file, localfile, overwrite = TRUE, dtoken = dtoken)
    readr::read_csv(localfile, ...)
}
