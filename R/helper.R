s2_cell_to_vect <- function(x) {
  x <- s2::as_s2_cell(x)
  stopifnot(
    "x must be (coercible to) a s2_cell object" = inherits(x, "s2_cell")
  )
  x |>
    s2::s2_cell_to_lnglat() |>
    as.data.frame() |>
    terra::vect(geom = c("x", "y"), crs = "+proj=longlat +datum=WGS84")
}

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
  url_dir_hash <-
    digest::digest(
      dirname(url),
      algo = "xxhash32",
      serialize = FALSE,
    )
  f_name <- basename(url)
  if (etag) {
    url_etag <- url_etag(url)
    if (!is.na(url_etag)) {
      f_name <- paste0(
        tools::file_path_sans_ext(f_name),
        "--",
        url_etag,
        ".",
        tools::file_ext(f_name)
      )
    }
  }
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
