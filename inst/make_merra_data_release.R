#!/usr/bin/env Rscript

# build one complete half-year MERRA-2 release asset
#
# usage:
#   Rscript inst/make_merra_data_release.R YEAR HALF WORK_DIR [OUTPUT_FILE]
#
# the work directory retains the monthly and daily caches. this script writes
# an RDS and candidate DCF only; it never uploads or edits the package catalog.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) != 1) {
  stop("Could not determine the script location.", call. = FALSE)
}
repo_dir <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
source(file.path(repo_dir, "R", "helper.R"))
source(file.path(repo_dir, "R", "geomarker_data.R"))
source(file.path(repo_dir, "R", "merra.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3 || length(args) > 4) {
  stop(
    "Usage: Rscript inst/make_merra_data_release.R YEAR HALF WORK_DIR [OUTPUT_FILE]",
    call. = FALSE
  )
}
year <- suppressWarnings(as.integer(args[[1]]))
half <- toupper(args[[2]])
if (is.na(year) || year < 2017 || !half %in% c("H1", "H2")) {
  stop("YEAR must be 2017 or later and HALF must be H1 or H2.", call. = FALSE)
}

work_dir <- normalizePath(args[[3]], mustWork = FALSE)
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(R_USER_DATA_DIR = work_dir)
default_name <- sprintf("merra2_%d_%s_pm25.rds", year, tolower(half))
output_file <- if (length(args) == 4) {
  args[[4]]
} else {
  file.path(work_dir, default_name)
}
output_name <- basename(output_file)
pattern <- sprintf(
  "^merra2_%d_%s_pm25[.]rds$",
  year,
  tolower(half)
)
if (!grepl(pattern, output_name)) {
  stop("OUTPUT_FILE must use the official half-year asset name.", call. = FALSE)
}
dcf_file <- sub("[.]rds$", ".dcf", output_file)

month_numbers <- if (half == "H1") 1:6 else 7:12
months <- sprintf("%d-%02d", year, month_numbers)
message("Building ", months[[1]], " through ", months[[6]], ".")
month_files <- install_merra_data(
  months,
  source = "earthdata",
  overwrite = TRUE,
  quiet = FALSE
)
month_records <- lapply(
  sub("[.]rds$", ".dcf", month_files),
  function(path) as.list(read.dcf(path)[1, ])
)
month_data <- Map(
  function(path, record) merra_read_data(path, record),
  month_files,
  month_records
)
grids <- lapply(month_data, function(x) sort(unique(x$s2)))
if (!all(vapply(grids, identical, logical(1), grids[[1]]))) {
  stop("The six monthly artifacts do not share one MERRA grid.", call. = FALSE)
}
data <- do.call(rbind, month_data)
row.names(data) <- NULL
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
merra_save_data(data, output_file)

field <- function(name) {
  paste(vapply(month_records, `[[`, "", name), collapse = ",")
}
dates <- sort(unique(data$date))
record <- list(
  `MERRA-Year` = as.character(year),
  `MERRA-Half` = half,
  `Coverage-Start` = as.character(min(dates)),
  `Coverage-End` = as.character(max(dates)),
  `Asset-Days` = as.character(length(dates)),
  `Source-Collection-ID` = "C1276812830-GES_DISC",
  `Source-Collection` = "M2T1NXAER 5.12.4",
  `Source-Granule-URs` = field("Source-Granule-URs"),
  `Source-Granule-Concept-Revisions` = field(
    "Source-Granule-Concept-Revisions"
  ),
  `Source-Subset-SHA256` = field("Source-Subset-SHA256"),
  `Source-Variables` = paste(merra_variables, collapse = ","),
  `Source-BBox-West-South-East-North` = paste(merra_bbox, collapse = ","),
  `Transformation-ID` = "m2t1nxaer-daily-pm25-v1",
  `PM25-Formula` = paste(
    "DUSMASS25 + OCSMASS + BCSMASS + SSSMASS25 +",
    "SO4SMASS * 132.14 / 96.06"
  ),
  `Package-Release` = "v0.0.1",
  `Asset-Name` = output_name,
  `Asset-URL` = paste0(
    "https://github.com/geomarker-io/geomarker/releases/download/v0.0.1/",
    output_name
  ),
  `Asset-Bytes` = as.character(file.info(output_file)$size),
  `Asset-SHA256` = digest::digest(output_file, algo = "sha256", file = TRUE),
  `Asset-Rows` = as.character(nrow(data)),
  `Asset-Grid-Cells` = as.character(length(grids[[1]])),
  `Asset-Schema` = merra_schema,
  `Asset-Compression` = "RDS-XZ",
  `Grid-Longitude-Resolution-Degrees` = "0.625",
  `Grid-Latitude-Resolution-Degrees` = "0.5",
  `Fixture-Name` = url_to_filename(
    paste0(
      "https://github.com/geomarker-io/geomarker/releases/download/v0.0.1/",
      output_name
    ),
    etag = FALSE
  ),
  Attribution = "NASA Global Modeling and Assimilation Office MERRA-2",
  `Built-UTC` = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)
write.dcf(as.data.frame(record, check.names = FALSE), dcf_file)
invisible(merra_read_data(output_file, record))
message("Validated MERRA release asset: ", output_file)
message("Candidate DCF record: ", dcf_file)
