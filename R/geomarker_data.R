#' Path to geomarker data directory
#'
#' By default, the R user's data directory for the geomarker data package is
#' used to download and install geomarker data files as needed when used
#' by geomarker assessment functions (see `?tools::R_user_dir`).
#' Specify an alternative download location by setting the `R_USER_DATA_DIR`
#' environment variable.
#' @param subdir character (length <= 1); optional subdirectory within the
#' geomarker data folder
#' @returns character string of path to directory
#' @export
#' @examples
#' geomarker_data_dir()
#' geomarker_data_dir("greenness")
#' # use environment variable to change location
#' withr::local_envvar(R_USER_DATA_DIR = tempdir())
#' geomarker_data_dir()
#' withr::deferred_run()
geomarker_data_dir <- function(subdir = character(0)) {
  stopifnot(
    "subdir must be a character vector" = inherits(subdir, "character"),
    "subdir must not be longer than one" = length(subdir) <= 1,
    "subdir must not be missing" = !is.na(subdir),
    "subdir must only use alphanumeric characters" = grepl(
      "^[a-z0-9]+$",
      subdir,
      ignore.case = TRUE
    )
  )
  the_path <- tools::R_user_dir(package = "geomarker", which = "data")
  if (length(subdir) > 0) {
    the_path <- file.path(the_path, subdir)
  }
  if (!file.exists(the_path)) {
    dir.create(the_path, recursive = TRUE)
  }
  the_path
}

# TODO list files in geomarker data folder

# really only designed to be used with static URLs ending in filename
# TODO will break with query parameters??
url_to_filename <- function(urls) {
  url_dir_hash <- vapply(
    dirname(urls),
    digest::digest,
    algo = "xxhash32",
    serialize = FALSE,
    FUN.VALUE = character(1)
  )
  f_name <- basename(urls)
  filepath <- paste(url_dir_hash, f_name, sep = "--")
  filepath
}


#' Download a file to the geomarker data directory
#'
#' `geomarker_download_file()` downloads a file at the URL
#' to the geomarker data directory
#' (see `?geomarker_data_dir()`) and named using a short hash of the
#' non-filename portion of the URL in addition to a filename derived
#' from the URL. By default, files that are already downloaded will not
#' be downloaded again.
#' @param url character (length one); URL of file to download
#' @param overwrite logical; overwrite file if already downloaded?
#' @param quiet logical; show download progress messages
#' and print path to downloaded file?
#' @param subdir character (length one); optional subdirectory
#' within the geomarker data folder
#' @returns character string of file path to downloaded file
#' (by default, returned invisibly)
#' @export
#' @examples
#' geomarker_download_file(
#'   paste0(
#'     "https://www2.census.gov/geo/tiger/",
#'     "TIGER2024/INTERNATIONALBOUNDARY/",
#'     "tl_2024_us_internationalboundary.zip"
#'  )
#' )
geomarker_download_file <- function(
  url,
  overwrite = FALSE,
  quiet = FALSE,
  subdir = character(0)
) {
  stopifnot(
    "url must be length one" = length(url) == 1,
    "url must be character" = inherits(url, "character")
  )
  dest <- file.path(geomarker_data_dir(subdir = subdir), url_to_filename(url))
  if (file.exists(dest) && !overwrite) {
    if (quiet) {
      return(invisible(dest))
    }
    return(dest)
  }
  tmp <- tempfile(pattern = "geomarker_dl_")
  on.exit(unlink(tmp), add = TRUE)
  if (requireNamespace("curl", quietly = TRUE)) {
    curl::curl_download(url, destfile = tmp, quiet = quiet, mode = "wb")
  } else {
    message("install the curl package for a better downloading experience!")
    utils::download.file(url, destfile = tmp, mode = "wb", quiet = quiet)
  }
  ok <- file.rename(tmp, dest)
  if (!ok && !file.copy(tmp, dest, overwrite = TRUE)) {
    stop(
      "Failed to move downloaded file: ",
      tmp,
      " into destination: ",
      dest,
      call. = FALSE
    )
  }
  if (quiet) {
    return(invisible(dest))
  }
  dest
}
