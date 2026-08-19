geomarker_stow <- function(url, .subdir, ..., .etag = NULL) {
  args <- list(...)
  reserved <- intersect(names(args), c("package", "subdir"))
  if (length(reserved) > 0) {
    stop(
      "`package` and `subdir` are fixed by geomarker and cannot be ",
      "supplied through `...`.",
      call. = FALSE
    )
  }
  if (!is.null(.etag)) {
    args$etag <- .etag
  }
  if (geomarker_no_download()) {
    args$offline <- TRUE
  }
  args$url <- url
  args$package <- "geomarker"
  args$subdir <- .subdir
  do.call(stow::stow, args)
}

geomarker_stow_path <- function(subdir) {
  stow::stow_path(package = "geomarker", subdir = subdir)
}

geomarker_stow_filename <- function(url) {
  paste0(
    digest::digest(dirname(url), algo = "xxhash64", serialize = FALSE),
    "--",
    basename(url)
  )
}

geomarker_fixture_stow_dir <- function(output_dir, subdir) {
  path <- file.path(output_dir, "stow", subdir)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}
