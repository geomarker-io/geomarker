#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || length(args) > 2) {
  stop(
    "Usage: Rscript inst/validate_traffic_data_release.R ASSET [MANIFEST]",
    call. = FALSE
  )
}
asset <- normalizePath(args[[1]], mustWork = TRUE)
manifest_path <- if (length(args) == 2) {
  normalizePath(args[[2]], mustWork = TRUE)
} else {
  normalizePath(file.path("inst", "traffic-data.dcf"), mustWork = TRUE)
}
manifest <- as.list(read.dcf(manifest_path)[1, ])
required <- c(
  "HPMS-Year",
  "HPMS-Source-Item-ID",
  "HPMS-Source-SHA256",
  "Transformation-ID",
  "Package-Release",
  "Asset-Name",
  "Asset-Bytes",
  "Asset-SHA256",
  "Asset-Rows",
  "Asset-Negative-Passenger-Rows",
  "Asset-Layer",
  "Asset-CRS",
  "Asset-Schema"
)
if (!all(required %in% names(manifest))) {
  stop("Traffic data manifest is incomplete.", call. = FALSE)
}
if (!identical(manifest[["HPMS-Year"]], "2024")) {
  stop("Traffic data manifest does not describe HPMS 2024.", call. = FALSE)
}
if (!identical(basename(asset), manifest[["Asset-Name"]])) {
  stop("Traffic asset filename does not match the manifest.", call. = FALSE)
}

expected_bytes <- as.numeric(manifest[["Asset-Bytes"]])
actual_bytes <- unname(file.info(asset)$size)
if (is.na(actual_bytes) || actual_bytes != expected_bytes) {
  stop(
    "Traffic asset size mismatch: expected ",
    expected_bytes,
    ", observed ",
    actual_bytes,
    ".",
    call. = FALSE
  )
}
actual_sha256 <- digest::digest(
  file = asset,
  algo = "sha256",
  serialize = FALSE
)
if (!identical(actual_sha256, manifest[["Asset-SHA256"]])) {
  stop(
    "Traffic asset SHA-256 mismatch: expected ",
    manifest[["Asset-SHA256"]],
    ", observed ",
    actual_sha256,
    ".",
    call. = FALSE
  )
}

sqlite <- Sys.which("sqlite3")
if (!nzchar(sqlite)) {
  stop("`sqlite3` is required to validate traffic data.", call. = FALSE)
}
integrity <- system2(
  sqlite,
  c(shQuote(asset), shQuote("PRAGMA integrity_check;")),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(integrity, "ok")) {
  stop("Traffic GeoPackage integrity check failed.", call. = FALSE)
}

layer <- manifest[["Asset-Layer"]]
sql <- paste0(
  "SELECT count(*), ",
  "sum(AADT IS NULL OR AADT_SINGLE_UNIT IS NULL OR AADT_COMBINATION IS NULL), ",
  "sum(AADT < 0 OR AADT > 1000000 OR ",
  "AADT_SINGLE_UNIT < 0 OR AADT_SINGLE_UNIT > 1000000 OR ",
  "AADT_COMBINATION < 0 OR AADT_COMBINATION > 1000000), ",
  "sum(AADT_SINGLE_UNIT + AADT_COMBINATION > AADT) ",
  "FROM ",
  layer,
  ";"
)
values <- system2(
  sqlite,
  c(shQuote(asset), shQuote(sql)),
  stdout = TRUE,
  stderr = TRUE
)
parsed <- as.numeric(strsplit(values[[1]], "|", fixed = TRUE)[[1]])
expected_rows <- as.numeric(manifest[["Asset-Rows"]])
expected_negative_passenger <- as.numeric(
  manifest[["Asset-Negative-Passenger-Rows"]]
)
if (
  !identical(parsed[[1]], expected_rows) ||
    any(parsed[2:3] != 0) ||
    !identical(parsed[[4]], expected_negative_passenger)
) {
  stop("Traffic GeoPackage row or value validation failed.", call. = FALSE)
}
rtree <- system2(
  sqlite,
  c(
    shQuote(asset),
    shQuote(paste0(
      "SELECT count(*) FROM sqlite_master WHERE type='table' AND ",
      "name='rtree_",
      layer,
      "_geom';"
    ))
  ),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(rtree, "1")) {
  stop("Traffic GeoPackage does not contain its spatial index.", call. = FALSE)
}

layers <- sf::st_layers(asset, do_count = TRUE)
layer_index <- match(layer, layers$name)
if (is.na(layer_index) || layers$features[[layer_index]] != expected_rows) {
  stop(
    "Traffic GeoPackage layer count does not match the manifest.",
    call. = FALSE
  )
}
sample <- sf::st_read(
  asset,
  query = paste0(
    "SELECT AADT, AADT_SINGLE_UNIT, AADT_COMBINATION, geom FROM ",
    layer,
    " LIMIT 1"
  ),
  quiet = TRUE
)
if (
  !identical(
    names(sf::st_drop_geometry(sample)),
    c("AADT", "AADT_SINGLE_UNIT", "AADT_COMBINATION")
  )
) {
  stop("Traffic GeoPackage schema does not match the contract.", call. = FALSE)
}
if (sf::st_crs(sample)$epsg != 4326) {
  stop("Traffic GeoPackage is not EPSG:4326.", call. = FALSE)
}
schema <- system2(
  sqlite,
  c(
    shQuote(asset),
    shQuote(paste0(
      "SELECT name || ':' || type FROM pragma_table_info('",
      layer,
      "') WHERE name IN ('AADT', 'AADT_SINGLE_UNIT', ",
      "'AADT_COMBINATION', 'geom') ORDER BY cid;"
    ))
  ),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(paste(schema, collapse = ","), manifest[["Asset-Schema"]])) {
  stop(
    "Traffic GeoPackage storage schema does not match the manifest.",
    call. = FALSE
  )
}

message(
  "Validated ",
  basename(asset),
  ": ",
  format(expected_rows, big.mark = ","),
  " rows; ",
  expected_negative_passenger,
  " source rows with negative derived passenger AADT; SHA-256 ",
  actual_sha256
)
