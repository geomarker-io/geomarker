#!/usr/bin/env Rscript

# Build geomarker's versioned HPMS 2024 F1/F2 AADT GeoPackage.
#
# Usage:
#   Rscript inst/make_traffic_data_release.R WORK_DIR [OUTPUT_FILE]
#
# WORK_DIR must be a dedicated `geomarker-hpms-2024.*` directory directly
# under /private/tmp. A failed build removes it; a successful build removes
# the source ZIP, extraction, and partial output, leaving only the validated
# GeoPackage and its build manifest when OUTPUT_FILE is inside WORK_DIR.

source_url <- paste0(
  "https://www.arcgis.com/sharing/rest/content/items/",
  "5e6a977c2d7c4ec1bdc82e684d3384f2/data"
)
source_bytes <- 2623940113
source_sha256 <- "129b868c0c6d684f63ea137e6a1658495e5a6b520af2bbfc62785c4f31b8c83f"
source_modified_utc <- "2026-07-15T17:38:09Z"
source_item_id <- "5e6a977c2d7c4ec1bdc82e684d3384f2"
asset_name <- "hpms_2024_f12_aadt.gpkg"
asset_layer <- "hpms_2024_f12_aadt"
package_release <- "v0.0.1"
transformation_id <- "f12-aadt-v1"
required_fields <- c(
  "F_SYSTEM",
  "AADT",
  "AADT_SINGLE_UNIT",
  "AADT_COMBINATION"
)
expected_jurisdictions <- c(
  "AK",
  "AL",
  "AR",
  "AZ",
  "CA",
  "CO",
  "CT",
  "DC",
  "DE",
  "FL",
  "GA",
  "HI",
  "IA",
  "ID",
  "IL",
  "IN",
  "KS",
  "KY",
  "LA",
  "MA",
  "MD",
  "ME",
  "MI",
  "MN",
  "MO",
  "MS",
  "MT",
  "NC",
  "ND",
  "NE",
  "NH",
  "NJ",
  "NM",
  "NV",
  "NY",
  "OH",
  "OK",
  "OR",
  "PA",
  "PR",
  "RI",
  "SC",
  "SD",
  "TN",
  "TX",
  "UT",
  "VA",
  "VT",
  "WA",
  "WI",
  "WV",
  "WY"
)
gib <- 1024^3
minimum_free_bytes <- 10 * gib
extraction_output_reserve <- 2 * gib

stop_build <- function(...) {
  stop(..., call. = FALSE)
}

check_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop_build("Package `", package, "` is required to build traffic data.")
  }
}

free_bytes <- function(path) {
  output <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE, stderr = TRUE)
  if (length(output) < 2) {
    stop_build("Could not determine free disk space for ", path, ".")
  }
  fields <- strsplit(trimws(output[[length(output)]]), "[[:space:]]+")[[1]]
  as.numeric(fields[[4]]) * 1024
}

require_free_space <- function(path, required, context) {
  available <- free_bytes(path)
  if (is.na(available) || available < required) {
    stop_build(
      context,
      " requires ",
      format(required, big.mark = ",", scientific = FALSE),
      " free bytes, but only ",
      format(available, big.mark = ",", scientific = FALSE),
      " are available at ",
      path,
      "."
    )
  }
  invisible(available)
}

download_source <- function(dest) {
  if (file.exists(dest) && file.info(dest)$size == source_bytes) {
    message("Using complete cached source ZIP: ", dest)
    return(invisible(dest))
  }
  require_free_space(
    dirname(dest),
    source_bytes + minimum_free_bytes,
    "Downloading HPMS 2024"
  )
  curl <- Sys.which("curl")
  if (!nzchar(curl)) {
    stop_build("`curl` is required for a resumable HPMS download.")
  }
  status <- system2(
    curl,
    c(
      "--fail",
      "--location",
      "--continue-at",
      "-",
      "--output",
      shQuote(dest),
      shQuote(source_url)
    )
  )
  if (
    status != 0 || !file.exists(dest) || file.info(dest)$size != source_bytes
  ) {
    stop_build("HPMS 2024 download did not complete successfully: ", dest)
  }
  invisible(dest)
}

zip_listing <- function(path) {
  unzip <- Sys.which("unzip")
  if (!nzchar(unzip)) {
    stop_build("`unzip` is required to inspect the HPMS archive.")
  }
  listing <- system2(
    unzip,
    c("-Z1", shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(listing, "status")
  if (!is.null(status) && status != 0) {
    stop_build("Could not list the HPMS source ZIP.")
  }
  listing
}

zip_gdb_path <- function(listing) {
  gdb_entries <- listing[grepl("[.]gdb/", listing, ignore.case = TRUE)]
  gdb_paths <- unique(sub("(^.*[.]gdb)/.*$", "\\1", gdb_entries))
  if (length(gdb_paths) != 1) {
    stop_build(
      "Expected exactly one File Geodatabase in the source ZIP; found ",
      length(gdb_paths),
      "."
    )
  }
  gdb_paths[[1]]
}

zip_uncompressed_bytes <- function(path) {
  unzip <- Sys.which("unzip")
  listing <- system2(
    unzip,
    c("-l", shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  total_line <- grep(
    "^[[:space:]]*[0-9]+[[:space:]]+[0-9]+ files$",
    listing,
    value = TRUE
  )
  if (length(total_line) != 1) {
    stop_build("Could not determine uncompressed HPMS archive size.")
  }
  as.numeric(strsplit(trimws(total_line), "[[:space:]]+")[[1]][[1]])
}

open_source_dsn <- function(source_zip, work_dir, listing) {
  gdb_path <- zip_gdb_path(listing)
  zip_dsn <- paste0(
    "/vsizip/",
    normalizePath(source_zip, winslash = "/", mustWork = TRUE),
    "/",
    gdb_path
  )
  layers <- tryCatch(
    sf::st_layers(zip_dsn, do_count = FALSE),
    error = function(err) NULL
  )
  if (!is.null(layers)) {
    message("Reading the File Geodatabase directly from the ZIP.")
    return(list(dsn = zip_dsn, extracted = NULL, layers = layers))
  }

  uncompressed <- zip_uncompressed_bytes(source_zip)
  required <- file.info(source_zip)$size +
    uncompressed +
    extraction_output_reserve +
    minimum_free_bytes
  require_free_space(
    work_dir,
    required,
    "Fallback extraction of HPMS 2024"
  )
  extract_dir <- file.path(work_dir, "extracted")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  message(
    "Direct ZIP access failed; safely extracting ",
    format(uncompressed, big.mark = ",", scientific = FALSE),
    " bytes."
  )
  utils::unzip(source_zip, exdir = extract_dir)
  extracted_dsn <- file.path(extract_dir, gdb_path)
  if (!dir.exists(extracted_dsn)) {
    stop_build("Extracted File Geodatabase was not found: ", extracted_dsn)
  }
  list(
    dsn = extracted_dsn,
    extracted = extract_dir,
    layers = sf::st_layers(extracted_dsn, do_count = FALSE)
  )
}

layer_fields <- function(dsn, layer) {
  escaped_layer <- gsub('"', '""', layer, fixed = TRUE)
  sample <- sf::st_read(
    dsn,
    query = paste0(
      'SELECT F_SYSTEM, AADT, AADT_SINGLE_UNIT, AADT_COMBINATION FROM "',
      escaped_layer,
      '" LIMIT 1'
    ),
    quiet = TRUE
  )
  names(sf::st_drop_geometry(sample))
}

discover_jurisdiction_layers <- function(dsn, layers) {
  candidates <- character()
  for (layer in layers$name) {
    fields <- tryCatch(
      layer_fields(dsn, layer),
      error = function(err) character()
    )
    if (all(tolower(required_fields) %in% tolower(fields))) {
      candidates <- c(candidates, layer)
    }
  }
  candidate_jurisdictions <- sub(
    "^HPMS_FULL_([A-Z]{2})_2024$",
    "\\1",
    candidates
  )
  if (!setequal(candidate_jurisdictions, expected_jurisdictions)) {
    stop_build(
      "HPMS jurisdiction layers do not match the expected 50 states, ",
      "District of Columbia, and Puerto Rico.\nMissing: ",
      paste(
        setdiff(expected_jurisdictions, candidate_jurisdictions),
        collapse = ", "
      ),
      "\nUnexpected: ",
      paste(
        setdiff(candidate_jurisdictions, expected_jurisdictions),
        collapse = ", "
      )
    )
  }
  sort(candidates)
}

read_jurisdiction <- function(dsn, layer) {
  escaped_layer <- gsub('"', '""', layer, fixed = TRUE)
  query <- paste0(
    'SELECT AADT, AADT_SINGLE_UNIT, AADT_COMBINATION FROM "',
    escaped_layer,
    '" WHERE CAST(F_SYSTEM AS INTEGER) IN (1, 2)'
  )
  out <- sf::st_read(dsn, query = query, quiet = TRUE)
  attributes <- sf::st_drop_geometry(out)
  out <- out[stats::complete.cases(attributes), ]
  out <- out[!sf::st_is_empty(out), ]
  if (nrow(out) == 0) {
    return(out)
  }
  out <- sf::st_zm(out, drop = TRUE, what = "ZM")
  if (is.na(sf::st_crs(out))) {
    stop_build("Layer ", layer, " does not have a CRS.")
  }
  out <- sf::st_transform(out, 4326)
  out <- sf::st_cast(out, "MULTILINESTRING", warn = FALSE)
  valid <- sf::st_is_valid(out)
  if (any(is.na(valid) | !valid)) {
    stop_build("Layer ", layer, " contains invalid roadway geometry.")
  }
  negative <- with(
    sf::st_drop_geometry(out),
    AADT < 0 | AADT_SINGLE_UNIT < 0 | AADT_COMBINATION < 0
  )
  if (any(negative)) {
    stop_build("Layer ", layer, " contains negative AADT values.")
  }
  out
}

validate_output <- function(path, expected_rows) {
  sqlite <- Sys.which("sqlite3")
  if (!nzchar(sqlite)) {
    stop_build("`sqlite3` is required to validate the traffic GeoPackage.")
  }
  integrity <- system2(
    sqlite,
    c(shQuote(path), shQuote("PRAGMA integrity_check;")),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!identical(integrity, "ok")) {
    stop_build(
      "GeoPackage integrity validation failed: ",
      paste(integrity, collapse = "\n")
    )
  }
  sql <- paste0(
    "SELECT count(*), ",
    "sum(AADT IS NULL OR AADT_SINGLE_UNIT IS NULL OR AADT_COMBINATION IS NULL), ",
    "sum(AADT < 0 OR AADT > 1000000 OR ",
    "AADT_SINGLE_UNIT < 0 OR AADT_SINGLE_UNIT > 1000000 OR ",
    "AADT_COMBINATION < 0 OR AADT_COMBINATION > 1000000) ",
    "FROM ",
    asset_layer,
    ";"
  )
  values <- system2(
    sqlite,
    c(shQuote(path), shQuote(sql)),
    stdout = TRUE,
    stderr = TRUE
  )
  parsed <- as.numeric(strsplit(values[[1]], "|", fixed = TRUE)[[1]])
  if (
    !identical(parsed[[1]], as.numeric(expected_rows)) || any(parsed[-1] != 0)
  ) {
    stop_build("GeoPackage row or value validation failed: ", values[[1]])
  }
  rtree <- system2(
    sqlite,
    c(
      shQuote(path),
      shQuote(paste0(
        "SELECT count(*) FROM sqlite_master WHERE type='table' AND ",
        "name='rtree_",
        asset_layer,
        "_geom';"
      ))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!identical(rtree, "1")) {
    stop_build("GeoPackage spatial index validation failed.")
  }
  info <- sf::st_layers(path, do_count = TRUE)
  which_layer <- match(asset_layer, info$name)
  if (is.na(which_layer) || info$features[[which_layer]] != expected_rows) {
    stop_build("GeoPackage layer count does not match the build count.")
  }
  sample <- sf::st_read(
    path,
    query = paste0(
      "SELECT AADT, AADT_SINGLE_UNIT, AADT_COMBINATION, geom FROM ",
      asset_layer,
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
    stop_build("GeoPackage schema validation failed.")
  }
  if (sf::st_crs(sample)$epsg != 4326) {
    stop_build("GeoPackage CRS validation failed.")
  }
  invisible(TRUE)
}

write_build_manifest <- function(path, source_zip, asset_path, rows) {
  sqlite <- Sys.which("sqlite3")
  negative_passenger_rows <- system2(
    sqlite,
    c(
      shQuote(asset_path),
      shQuote(paste0(
        "SELECT count(*) FROM ",
        asset_layer,
        " WHERE AADT_SINGLE_UNIT + AADT_COMBINATION > AADT;"
      ))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  values <- data.frame(
    check.names = FALSE,
    `HPMS-Year` = "2024",
    `HPMS-Source-Item-ID` = source_item_id,
    `HPMS-Source-Modified-UTC` = source_modified_utc,
    `HPMS-Source-URL` = source_url,
    `HPMS-Source-Bytes` = format(
      file.info(source_zip)$size,
      scientific = FALSE
    ),
    `HPMS-Source-SHA256` = source_sha256,
    `Transformation-ID` = transformation_id,
    `Package-Release` = package_release,
    `Asset-Name` = basename(asset_path),
    `Asset-URL` = paste0(
      "https://github.com/geomarker-io/geomarker/releases/download/",
      package_release,
      "/",
      basename(asset_path)
    ),
    `Asset-Bytes` = format(file.info(asset_path)$size, scientific = FALSE),
    `Asset-SHA256` = digest::digest(
      file = asset_path,
      algo = "sha256",
      serialize = FALSE
    ),
    `Asset-Rows` = format(rows, scientific = FALSE),
    `Asset-Negative-Passenger-Rows` = negative_passenger_rows,
    `Asset-Layer` = asset_layer,
    `Asset-CRS` = "EPSG:4326",
    `Asset-Schema` = paste0(
      "geom:MULTILINESTRING,AADT:MEDIUMINT,",
      "AADT_SINGLE_UNIT:MEDIUMINT,AADT_COMBINATION:MEDIUMINT"
    ),
    `Built-UTC` = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  write.dcf(values, file = path)
  invisible(path)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || length(args) > 2) {
  stop_build(
    "Usage: Rscript inst/make_traffic_data_release.R WORK_DIR [OUTPUT_FILE]"
  )
}
check_package("sf")
check_package("digest")

work_dir <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
private_tmp <- normalizePath("/private/tmp", winslash = "/", mustWork = TRUE)
work_parent <- normalizePath(
  dirname(work_dir),
  winslash = "/",
  mustWork = TRUE
)
if (
  !identical(work_parent, private_tmp) ||
    !startsWith(basename(work_dir), "geomarker-hpms-2024.")
) {
  stop_build(
    "WORK_DIR must be a dedicated geomarker-hpms-2024.* directory ",
    "directly under /private/tmp."
  )
}
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
source_zip <- file.path(work_dir, "hpms_2024.gdb.zip")
output_file <- if (length(args) == 2) {
  normalizePath(args[[2]], winslash = "/", mustWork = FALSE)
} else {
  file.path(work_dir, asset_name)
}
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
partial_file <- sub("[.]gpkg$", ".partial.gpkg", output_file)
manifest_file <- sub("[.]gpkg$", "-manifest.dcf", output_file)
output_was_present <- file.exists(output_file)
extracted_dir <- NULL
build_error <- tryCatch(
  {
    download_source(source_zip)
    if (file.info(source_zip)$size != source_bytes) {
      stop_build("The HPMS source ZIP size does not match its source metadata.")
    }
    observed_source_sha256 <- digest::digest(
      file = source_zip,
      algo = "sha256",
      serialize = FALSE
    )
    if (!identical(observed_source_sha256, source_sha256)) {
      stop_build(
        "The HPMS source ZIP SHA-256 does not match the pinned source metadata."
      )
    }

    if (file.exists(output_file)) {
      if (file.exists(manifest_file)) {
        existing_manifest <- as.list(read.dcf(manifest_file)[1, ])
        output_layers <- sf::st_layers(output_file, do_count = TRUE)
        output_index <- match(asset_layer, output_layers$name)
        if (is.na(output_index)) {
          stop_build("Existing output does not contain ", asset_layer, ".")
        }
        rows_written <- output_layers$features[[output_index]]
        validate_output(output_file, rows_written)
        observed_asset_sha256 <- digest::digest(
          file = output_file,
          algo = "sha256",
          serialize = FALSE
        )
        if (
          !identical(
            observed_asset_sha256,
            existing_manifest[["Asset-SHA256"]]
          ) ||
            file.info(output_file)$size !=
              as.numeric(
                existing_manifest[["Asset-Bytes"]]
              )
        ) {
          stop_build("Existing output does not match its build manifest.")
        }
      } else {
        message(
          "Validating existing output and completing its missing manifest."
        )
        output_layers <- sf::st_layers(output_file, do_count = TRUE)
        output_index <- match(asset_layer, output_layers$name)
        if (is.na(output_index)) {
          stop_build("Existing output does not contain ", asset_layer, ".")
        }
        rows_written <- output_layers$features[[output_index]]
        validate_output(output_file, rows_written)
        write_build_manifest(
          manifest_file,
          source_zip,
          output_file,
          rows_written
        )
      }
    } else if (file.exists(partial_file)) {
      message(
        "Validating existing partial output before resuming the source build."
      )
      partial_layers <- sf::st_layers(partial_file, do_count = TRUE)
      partial_index <- match(asset_layer, partial_layers$name)
      if (is.na(partial_index)) {
        stop_build(
          "Existing partial output does not contain ",
          asset_layer,
          "."
        )
      }
      rows_written <- partial_layers$features[[partial_index]]
      validate_output(partial_file, rows_written)
      if (!file.rename(partial_file, output_file)) {
        stop_build("Could not move validated output into place: ", output_file)
      }
      write_build_manifest(manifest_file, source_zip, output_file, rows_written)
    } else {
      require_free_space(work_dir, minimum_free_bytes, "Inspecting HPMS 2024")
      listing <- zip_listing(source_zip)
      source <- open_source_dsn(source_zip, work_dir, listing)
      extracted_dir <- source$extracted
      layers <- discover_jurisdiction_layers(source$dsn, source$layers)
      message("Found ", length(layers), " HPMS jurisdiction layers.")

      rows_written <- 0
      for (i in seq_along(layers)) {
        require_free_space(
          work_dir,
          minimum_free_bytes,
          paste0("Processing jurisdiction ", i, " of ", length(layers))
        )
        message("Processing ", i, " of ", length(layers), ": ", layers[[i]])
        jurisdiction <- read_jurisdiction(source$dsn, layers[[i]])
        if (nrow(jurisdiction) == 0) {
          next
        }
        sf::st_write(
          jurisdiction,
          partial_file,
          layer = asset_layer,
          append = rows_written > 0,
          delete_dsn = rows_written == 0,
          layer_options = if (rows_written == 0) {
            "SPATIAL_INDEX=YES"
          } else {
            character()
          },
          quiet = TRUE
        )
        rows_written <- rows_written + nrow(jurisdiction)
        rm(jurisdiction)
        gc(verbose = FALSE)
      }

      require_free_space(work_dir, minimum_free_bytes, "Validating HPMS 2024")
      validate_output(partial_file, rows_written)
      if (!file.rename(partial_file, output_file)) {
        stop_build("Could not move validated output into place: ", output_file)
      }
      write_build_manifest(manifest_file, source_zip, output_file, rows_written)
    }
    NULL
  },
  error = identity,
  interrupt = identity
)

if (!is.null(build_error)) {
  if (!output_was_present && file.exists(output_file)) {
    unlink(output_file)
  }
  if (output_was_present && identical(dirname(output_file), work_dir)) {
    unlink(source_zip)
    unlink(partial_file)
    if (!is.null(extracted_dir)) {
      unlink(extracted_dir, recursive = TRUE, force = TRUE)
    }
  } else {
    unlink(work_dir, recursive = TRUE, force = TRUE)
  }
  stop_build(
    "HPMS 2024 build failed; cleaned dedicated temporary files.\n",
    conditionMessage(build_error)
  )
}

unlink(source_zip)
unlink(partial_file)
if (!is.null(extracted_dir)) {
  unlink(extracted_dir, recursive = TRUE, force = TRUE)
}
if (!identical(dirname(output_file), work_dir)) {
  unlink(work_dir, recursive = TRUE, force = TRUE)
}
message("Validated traffic asset: ", output_file)
message("Build manifest: ", manifest_file)
