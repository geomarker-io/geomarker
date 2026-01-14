check_installed <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("The package `%s` is required", pkg)
    if (!is.null(reason)) {
      msg <- paste0(msg, " ", reason)
    }
    msg <- paste0(msg, ". Please install it first.")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

is_non_decreasing <- function(x) {
  if (length(x) <= 1) {
    return(TRUE)
  }
  all(diff(x) >= 0)
}

url_to_filename <- function(url, etag = TRUE) {
  stopifnot(
    "etag must be length one" = length(etag) == 1,
    "etag must be logical" = inherits(etag, "logical"),
    "url must be length one" = length(url) == 1,
    "url must be character" = inherits(url, "character")
  )
  if (grepl("\\?", url)) {
    stop(
      "URLs with query parameters are not supported. ",
      "Provide a URL that ends with a filename.",
      call. = FALSE
    )
  }
  url_dir_hash <- vapply(
    dirname(url),
    digest::digest,
    algo = "xxhash32",
    serialize = FALSE,
    FUN.VALUE = character(1)
  )
  url_etag <- url_etag(url)
  if (etag && !is.na(url_etag)) {
    url_dir_hash <- url_etag
  }
  f_name <- basename(url)
  filepath <- paste(url_dir_hash, f_name, sep = "--")
  filepath
}

url_etag <- function(url) {
  stopifnot(
    "url must be length one" = length(url) == 1,
    "url must be character" = inherits(url, "character")
  )
  check_installed("curl", "to check URL headers.")
  headers <- curl::new_handle(nobody = TRUE, header = TRUE) |>
    curl::curl_fetch_memory(url = url, handle = _) |>
    _$headers |>
    curl::parse_headers()

  which_etag_header <- grep("^ETag:", headers, ignore.case = TRUE)
  if (length(which_etag_header) == 0) {
    return(NA_character_)
  }
  etag <- sub(
    "^ETag:\\s*",
    "",
    headers[[which_etag_header]],
    ignore.case = TRUE
  )
  if (!is.null(etag)) {
    etag <- gsub("\\\\\"", "\"", etag)
    etag <- gsub("^\"|\"$", "", etag)
  }

  if (!is.null(etag)) {
    return(etag)
  }
  NA_character_
}
