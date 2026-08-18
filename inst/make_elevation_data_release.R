#!/usr/bin/env Rscript

elevation_release_stop <- function(...) {
  stop(..., call. = FALSE)
}

elevation_release_manifest <- function(path) {
  if (!file.exists(path)) {
    elevation_release_stop("Elevation manifest does not exist: ", path)
  }
  manifest <- as.list(read.dcf(path)[1, ])
  required <- c(
    "Source-Export-URL",
    "Asset-Name",
    "Asset-Bytes",
    "Asset-SHA256",
    "Asset-Layers",
    "Asset-Terra-Datatype",
    "Asset-CRS",
    "Asset-Extent-XMin-XMax-YMin-YMax",
    "Asset-Resolution-X-Y-Meters",
    "Asset-Rows",
    "Asset-Columns"
  )
  if (!all(required %in% names(manifest))) {
    elevation_release_stop("Elevation manifest is incomplete: ", path)
  }
  manifest
}

elevation_release_numbers <- function(manifest, field) {
  as.numeric(strsplit(manifest[[field]], ",", fixed = TRUE)[[1]])
}

elevation_release_equal <- function(actual, expected) {
  isTRUE(all.equal(
    unname(actual),
    unname(expected),
    tolerance = 0,
    check.attributes = FALSE
  ))
}

elevation_release_expect <- function(property, actual, expected) {
  if (!elevation_release_equal(actual, expected)) {
    elevation_release_stop(
      "Elevation asset ",
      property,
      " mismatch: expected ",
      paste(expected, collapse = ","),
      ", got ",
      paste(actual, collapse = ","),
      "."
    )
  }
}

elevation_validate_release_asset <- function(asset, manifest) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    elevation_release_stop("The digest package is required for validation.")
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    elevation_release_stop("The terra package is required for validation.")
  }
  if (!file.exists(asset)) {
    elevation_release_stop("Elevation asset does not exist: ", asset)
  }
  if (!identical(basename(asset), manifest[["Asset-Name"]])) {
    elevation_release_stop(
      "Elevation asset filename mismatch: expected ",
      manifest[["Asset-Name"]],
      ", got ",
      basename(asset),
      "."
    )
  }
  elevation_release_expect(
    "file size",
    unname(file.info(asset)$size),
    as.numeric(manifest[["Asset-Bytes"]])
  )
  elevation_release_expect(
    "SHA-256",
    digest::digest(
      file = asset,
      algo = "sha256",
      serialize = FALSE
    ),
    manifest[["Asset-SHA256"]]
  )

  raster <- tryCatch(
    terra::rast(asset),
    error = function(err) {
      elevation_release_stop(
        "Elevation asset is not a readable raster: ",
        conditionMessage(err)
      )
    }
  )
  elevation_release_expect(
    "layer count",
    terra::nlyr(raster),
    as.integer(manifest[["Asset-Layers"]])
  )
  elevation_release_expect(
    "datatype",
    terra::datatype(raster),
    manifest[["Asset-Terra-Datatype"]]
  )
  crs <- terra::crs(raster, describe = TRUE)
  epsg <- if (
    nrow(crs) == 1 &&
      identical(crs$authority[[1]], "EPSG") &&
      !is.na(crs$code[[1]])
  ) {
    as.integer(crs$code[[1]])
  } else {
    NA_integer_
  }
  elevation_release_expect(
    "EPSG code",
    epsg,
    as.integer(sub("^EPSG:", "", manifest[["Asset-CRS"]]))
  )
  elevation_release_expect(
    "extent",
    as.vector(terra::ext(raster)),
    elevation_release_numbers(
      manifest,
      "Asset-Extent-XMin-XMax-YMin-YMax"
    )
  )
  elevation_release_expect(
    "resolution",
    terra::res(raster),
    elevation_release_numbers(manifest, "Asset-Resolution-X-Y-Meters")
  )
  elevation_release_expect(
    "row count",
    terra::nrow(raster),
    as.integer(manifest[["Asset-Rows"]])
  )
  elevation_release_expect(
    "column count",
    terra::ncol(raster),
    as.integer(manifest[["Asset-Columns"]])
  )
  message(
    "Validated ",
    basename(asset),
    " (",
    manifest[["Asset-SHA256"]],
    ")."
  )
  invisible(asset)
}

script_file <- sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]]
)
script_dir <- dirname(normalizePath(script_file, mustWork = TRUE))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || length(args) > 2) {
  elevation_release_stop(
    "Usage: Rscript inst/make_elevation_data_release.R ",
    "OUTPUT_FILE [MANIFEST]"
  )
}
output_file <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
manifest_file <- if (length(args) == 2) {
  normalizePath(args[[2]], winslash = "/", mustWork = TRUE)
} else {
  file.path(script_dir, "elevation-data.dcf")
}
manifest <- elevation_release_manifest(manifest_file)
if (!identical(basename(output_file), manifest[["Asset-Name"]])) {
  elevation_release_stop(
    "OUTPUT_FILE must end with ",
    manifest[["Asset-Name"]],
    "."
  )
}
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

if (file.exists(output_file)) {
  elevation_validate_release_asset(output_file, manifest)
  message("Existing pinned elevation asset is valid; no network request made.")
  quit(save = "no", status = 0)
}
if (nzchar(Sys.getenv("R_GEOMARKER_NO_DOWNLOAD", unset = ""))) {
  elevation_release_stop(
    "R_GEOMARKER_NO_DOWNLOAD is set; the elevation release asset will not ",
    "be downloaded."
  )
}
if (!requireNamespace("curl", quietly = TRUE)) {
  elevation_release_stop("The curl package is required to export 3DEP data.")
}

partial_file <- paste0(output_file, ".partial")
on.exit(unlink(partial_file), add = TRUE)
message(
  "Exporting the pinned USGS 3DEP GeoTIFF without local conversion:\n  ",
  manifest[["Source-Export-URL"]]
)
tryCatch(
  curl::curl_download(
    manifest[["Source-Export-URL"]],
    destfile = partial_file,
    quiet = FALSE,
    mode = "wb"
  ),
  error = function(err) {
    elevation_release_stop(
      "USGS 3DEP export failed. No release asset was created.\n",
      "URL: ",
      manifest[["Source-Export-URL"]],
      "\nOriginal error: ",
      conditionMessage(err)
    )
  }
)

validation_manifest <- manifest
validation_manifest[["Asset-Name"]] <- basename(partial_file)
elevation_validate_release_asset(partial_file, validation_manifest)
if (!file.rename(partial_file, output_file)) {
  if (!file.copy(partial_file, output_file, overwrite = FALSE)) {
    elevation_release_stop(
      "Validated export could not be moved to ",
      output_file,
      "."
    )
  }
}
message("Pinned elevation release asset: ", output_file)
