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
#'
#' geomarker_data_dir_info()
#'
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

#' @rdname geomarker_data_dir
#' @export
geomarker_data_dir_info <- function(subdir = character(0)) {
  files <- list.files(
    geomarker_data_dir(subdir),
    recursive = TRUE,
    full.names = TRUE
  )
  info <- file.info(files)

  n_files <- sum(!is.na(info$size))
  total_bytes <- sum(info$size, na.rm = TRUE)

  pretty_size <- function(bytes) {
    if (bytes < 1024^2) {
      sprintf("%.1f MB", bytes / 1024^2)
    } else {
      sprintf("%.2f GB", bytes / 1024^3)
    }
  }

  message(
    sprintf(
      "The geomarker data directory has %s files totalling %s:",
      format(n_files, big.mark = ","),
      pretty_size(total_bytes)
    ),
    "\n  ",
    geomarker_data_dir()
  )

  invisible(NULL)
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
  if (!quiet) {
    size_note <- NULL
    if (requireNamespace("curl", quietly = TRUE)) {
      size_note <- tryCatch(
        {
          handle <- curl::new_handle(nobody = TRUE, header = TRUE)
          res <- curl::curl_fetch_memory(url, handle = handle)
          headers <- curl::parse_headers(res$headers)
          content_length <- headers[["content-length"]]
          if (is.null(content_length)) {
            content_length <- headers[["Content-Length"]]
          }
          if (is.null(content_length)) {
            return(NULL)
          }
          bytes <- suppressWarnings(as.numeric(content_length))
          if (is.na(bytes)) {
            return(NULL)
          }
          if (bytes < 1024^2) {
            sprintf(" (%.1f MB)", bytes / 1024^2)
          } else {
            sprintf(" (%.2f GB)", bytes / 1024^3)
          }
        },
        error = function(err) NULL
      )
    }
    message(
      "Downloading ",
      url,
      " -> ",
      dest,
      if (!is.null(size_note)) size_note
    )
  }
  err_context <- paste0(
    "Download failed.\n",
    "URL: ",
    url,
    "\n",
    "Expected file path: ",
    dest,
    "\n",
    "If you can download this file manually, place it at the path above and retry."
  )
  tryCatch(
    {
      if (requireNamespace("curl", quietly = TRUE)) {
        curl::curl_download(url, destfile = tmp, quiet = quiet, mode = "wb")
      } else {
        message("install the curl package for a better downloading experience!")
        utils::download.file(url, destfile = tmp, mode = "wb", quiet = quiet)
      }
    },
    error = function(err) {
      stop(
        err_context,
        "\nOriginal error: ",
        conditionMessage(err),
        call. = FALSE
      )
    }
  )
  ok <- file.rename(tmp, dest)
  if (!ok && !file.copy(tmp, dest, overwrite = TRUE)) {
    stop(
      "Failed to move downloaded file into destination.\n",
      "Temp file: ",
      tmp,
      "\n",
      err_context,
      call. = FALSE
    )
  }
  if (quiet) {
    return(invisible(dest))
  }
  dest
}
