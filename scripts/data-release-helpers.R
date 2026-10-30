# Maintainer-only helpers shared by data builders and publication tooling.
data_release_revision <- function(x) {
  if (length(x) != 1L || is.na(x) || !grepl("^[1-9][0-9]*$", as.character(x))) {
    stop("Revision must be a positive integer.", call. = FALSE)
  }
  as.character(x)
}

data_release_args <- function(args) {
  at <- which(args == "--revision")
  revision <- "1"
  if (length(at)) {
    if (length(at) != 1L || at == length(args)) {
      stop(
        "Supply --revision once, followed by a positive integer.",
        call. = FALSE
      )
    }
    revision <- data_release_revision(args[at + 1L])
    args <- args[-c(at, at + 1L)]
  }
  list(args = args, revision = revision)
}

data_release_tag <- function(record, revision = "1") {
  revision <- data_release_revision(revision)
  if (!is.null(record[["MERRA-Year"]])) {
    year <- record[["MERRA-Year"]]
    half <- tolower(record[["MERRA-Half"]])
    if (!grepl("^[0-9]{4}$", year) || !half %in% c("h1", "h2")) {
      stop("Invalid MERRA vintage.", call. = FALSE)
    }
    vintage <- paste("merra2", year, half, sep = "-")
  } else if (!is.null(record[["HPMS-Year"]])) {
    year <- record[["HPMS-Year"]]
    if (!grepl("^[0-9]{4}$", year)) {
      stop("Invalid HPMS vintage.", call. = FALSE)
    }
    vintage <- paste0("hpms-", year)
  } else {
    date <- record[["Source-Service-Data-As-Of"]]
    if (
      is.null(date) ||
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date) ||
        is.na(as.Date(date))
    ) {
      stop("Invalid elevation snapshot date.", call. = FALSE)
    }
    vintage <- paste0("elevation-", date)
  }
  paste0("data-", vintage, "-r", revision)
}

data_release_url <- function(tag, name) {
  paste0(
    "https://github.com/geomarker-io/geomarker/releases/download/",
    tag,
    "/",
    name
  )
}

data_release_pin <- function(record, revision = "1") {
  record[["Package-Release"]] <- NULL
  record[["Data-Release"]] <- data_release_tag(record, revision)
  record[["Asset-URL"]] <- data_release_url(
    record[["Data-Release"]],
    record[["Asset-Name"]]
  )
  record[["Fixture-Name"]] <- paste0(
    digest::digest(
      dirname(record[["Asset-URL"]]),
      algo = "xxhash64",
      serialize = FALSE
    ),
    "--",
    record[["Asset-Name"]]
  )
  record
}

data_release_write <- function(record, path) {
  write.dcf(as.data.frame(record, check.names = FALSE), path, width = 100)
  invisible(path)
}
