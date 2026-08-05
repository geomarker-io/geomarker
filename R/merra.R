merra_variables <- c(
  "DUSMASS25",
  "OCSMASS",
  "BCSMASS",
  "SSSMASS25",
  "SO4SMASS"
)
merra_columns <- c(
  "merra_dust",
  "merra_oc",
  "merra_bc",
  "merra_ss",
  "merra_so4",
  "merra_pm25"
)
merra_data_columns <- c("date", merra_columns, "s2")
merra_bbox <- c(-126.474609, 24.766785, -66.445313, 49.894634)
merra_schema <- paste0(
  "date:Date,",
  paste0(merra_columns, ":double", collapse = ","),
  ",s2:character"
)

merra_months <- function(x) {
  stopifnot(
    "merra_month must be a character vector" = is.character(x),
    "merra_month must not contain missing values" = !anyNA(x)
  )
  if (
    any(!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", x)) ||
      any(as.integer(substr(x, 1, 4)) < 2017)
  ) {
    stop(
      "merra_month must contain calendar months in YYYY-MM format from 2017 onward.",
      call. = FALSE
    )
  }
  x
}

merra_month_dates <- function(month) {
  month <- merra_months(month)
  stopifnot("month must have length one" = length(month) == 1)
  first <- as.Date(paste0(month, "-01"))
  seq(first, seq(first, by = "month", length.out = 2)[[2]] - 1, by = 1)
}

merra_local_file <- function(month) {
  file.path(
    geomarker_data_dir("merra"),
    "local",
    paste0("merra2_", month, "_pm25.rds")
  )
}

merra_save_data <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  partial <- tempfile(basename(path), dirname(path))
  on.exit(unlink(partial), add = TRUE)
  saveRDS(data, partial, compress = "xz")
  if (
    !file.rename(partial, path) && !file.copy(partial, path, overwrite = TRUE)
  ) {
    stop("Could not write MERRA data: ", path, call. = FALSE)
  }
  path
}

merra_read_data <- function(path, record = NULL) {
  if (!file.exists(path)) {
    stop("MERRA data do not exist: ", path, call. = FALSE)
  }
  if (!is.null(record)) {
    record <- as.list(record)
    required <- c(
      "Coverage-Start",
      "Coverage-End",
      "Asset-Days",
      "Asset-Bytes",
      "Asset-SHA256",
      "Asset-Rows",
      "Asset-Grid-Cells",
      "Asset-Schema"
    )
    if (
      any(!required %in% names(record)) ||
        any(!nzchar(unlist(record[required])))
    ) {
      stop("MERRA DCF metadata are incomplete.", call. = FALSE)
    }
    if (
      !identical(
        as.numeric(file.info(path)$size),
        as.numeric(record[["Asset-Bytes"]])
      ) ||
        !identical(
          digest::digest(path, algo = "sha256", file = TRUE),
          record[["Asset-SHA256"]]
        )
    ) {
      stop("MERRA data failed size or SHA-256 validation.", call. = FALSE)
    }
  }

  data <- tryCatch(readRDS(path), error = function(error) {
    stop(
      "MERRA data could not be read: ",
      conditionMessage(error),
      call. = FALSE
    )
  })
  if (!is.data.frame(data) || !identical(names(data), merra_data_columns)) {
    stop(
      "MERRA data schema must be: ",
      paste(merra_data_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (
    !inherits(data$date, "Date") ||
      !all(vapply(data[merra_columns], is.numeric, logical(1)))
  ) {
    stop("MERRA dates and concentrations have invalid types.", call. = FALSE)
  }
  data$s2 <- as.character(data$s2)
  if (
    anyNA(data) ||
      anyDuplicated(paste(data$date, data$s2)) ||
      any(!s2::s2_cell_is_valid(s2::as_s2_cell(unique(data$s2))))
  ) {
    stop(
      "MERRA data contain missing, duplicate, or invalid grid values.",
      call. = FALSE
    )
  }
  expected_pm25 <- with(
    data,
    merra_dust + merra_oc + merra_bc + merra_ss + merra_so4 * 132.14 / 96.06
  )
  if (!isTRUE(all.equal(data$merra_pm25, expected_pm25, tolerance = 1e-10))) {
    stop(
      "MERRA PM2.5 values do not match the documented formula.",
      call. = FALSE
    )
  }

  if (!is.null(record)) {
    dates <- seq(
      as.Date(record[["Coverage-Start"]]),
      as.Date(record[["Coverage-End"]]),
      by = 1
    )
    cells <- unique(data$s2)
    valid <- identical(sort(unique(data$date)), dates) &&
      nrow(data) == length(dates) * length(cells) &&
      as.numeric(record[["Asset-Days"]]) == length(dates) &&
      as.numeric(record[["Asset-Rows"]]) == nrow(data) &&
      as.numeric(record[["Asset-Grid-Cells"]]) == length(cells) &&
      identical(record[["Asset-Schema"]], merra_schema)
    if (!valid) {
      stop("MERRA data failed coverage or schema validation.", call. = FALSE)
    }
  }
  data
}

merra_release_manifest <- function() {
  path <- system.file("merra-data.dcf", package = "geomarker")
  read.dcf(if (nzchar(path)) path else file.path("inst", "merra-data.dcf"))
}

merra_release_files <- function(months, ..., warn = TRUE) {
  manifest <- merra_release_manifest()
  unique_months <- unique(months)
  records <- lapply(unique_months, function(month) {
    if (nrow(manifest) == 0) {
      return(NULL)
    }
    year <- substr(month, 1, 4)
    half <- if (as.integer(substr(month, 6, 7)) <= 6) "H1" else "H2"
    matches <- which(
      manifest[, "MERRA-Year"] == year & manifest[, "MERRA-Half"] == half
    )
    if (length(matches) == 0) {
      return(NULL)
    }
    if (length(matches) > 1) {
      stop(
        "MERRA release DCF has duplicate records for one half-year.",
        call. = FALSE
      )
    }
    record <- as.list(manifest[matches, ])
    start <- as.Date(sprintf(
      "%s-%s-01",
      year,
      if (half == "H1") "01" else "07"
    ))
    end <- if (half == "H1") {
      as.Date(sprintf("%s-06-30", year))
    } else {
      as.Date(sprintf("%s-12-31", year))
    }
    name <- sprintf("merra2_%s_%s_pm25.rds", year, tolower(half))
    required <- c(
      "Coverage-Start",
      "Coverage-End",
      "Asset-Days",
      "Asset-Name",
      "Asset-URL",
      "Asset-Bytes",
      "Asset-SHA256",
      "Asset-Rows",
      "Asset-Grid-Cells",
      "Asset-Schema"
    )
    valid <- all(required %in% names(record)) &&
      all(nzchar(unlist(record[required]))) &&
      identical(record[["Coverage-Start"]], as.character(start)) &&
      identical(record[["Coverage-End"]], as.character(end)) &&
      as.numeric(record[["Asset-Days"]]) == length(seq(start, end, by = 1)) &&
      identical(record[["Asset-Name"]], name) &&
      endsWith(record[["Asset-URL"]], paste0("/", name)) &&
      identical(record[["Asset-Schema"]], merra_schema)
    if (!isTRUE(valid)) {
      stop(
        "MERRA release DCF does not describe a complete half-year.",
        call. = FALSE
      )
    }
    record
  })
  names(records) <- unique_months

  paths <- new.env(parent = emptyenv())
  out <- vapply(
    unique_months,
    function(month) {
      record <- records[[month]]
      if (is.null(record)) {
        return(NA_character_)
      }
      key <- record[["Asset-SHA256"]]
      if (!exists(key, paths, inherits = FALSE)) {
        args <- list(...)
        args$url <- record[["Asset-URL"]]
        args$etag <- FALSE
        args$subdir <- "merra"
        path <- do.call(geomarker_download_file, args)
        merra_read_data(path, record)
        assign(key, path, paths)
      }
      get(key, paths, inherits = FALSE)
    },
    character(1)
  )
  missing <- names(out)[is.na(out)]
  if (warn && length(missing) > 0) {
    warning(
      "No official complete half-year MERRA release is recorded for ",
      paste(missing, collapse = ", "),
      ". Returning missing paths.",
      call. = FALSE
    )
  }
  stats::setNames(unname(out[match(months, names(out))]), months)
}

#' MERRA-2 aerosol diagnostics data
#'
#' Links daily surface PM2.5 and its aerosol components from NASA's MERRA-2
#' `M2T1NXAER` v5.12.4 product to each location and date in an `s2cd` vector.
#'
#' Each package release records one complete January--June and one complete
#' July--December asset per available year. A fully available calendar month
#' may instead be built locally with `install_merra_data(source =
#' "earthdata")`. Local monthly data take precedence over a released half-year
#' asset.
#'
#' Source concentrations are converted to micrograms per cubic meter and
#' averaged across each day. Total PM2.5 is calculated as `DUSMASS25 +
#' OCSMASS + BCSMASS + SSSMASS25 + SO4SMASS * 132.14 / 96.06`.
#'
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param ... passed to `geomarker_download_file()` when an official release
#'   asset is needed
#' @return A named list with one data frame per input location and one row per
#'   requested date. Columns are `merra_dust`, `merra_oc`, `merra_bc`,
#'   `merra_ss`, `merra_so4`, and `merra_pm25`. Unavailable periods contain
#'   missing values and produce one warning.
#' @references
#' <https://disc.gsfc.nasa.gov/datasets/M2T1NXAER_5.12.4/summary>
#' @references
#' <https://gmao.gsfc.nasa.gov/gmao-products/merra-2/faq_merra-2/>
#' @export
#' @examples
#' withr::local_envvar(
#'   R_USER_DATA_DIR = fs::path_package("geomarker", "gmrkr--8841"),
#'   R_GEOMARKER_NO_DOWNLOAD = "true"
#' )
#' get_merra_data(s2cd_example())
get_merra_data <- function(x, ...) {
  stopifnot("x must be a s2_cell_dates vector" = is_s2cd(x))
  cells <- s2::as_s2_cell(x)
  dates <- s2cd_dates(x)
  out <- lapply(lengths(dates), function(n) {
    as.data.frame(
      matrix(
        NA_real_,
        n,
        length(merra_columns),
        dimnames = list(NULL, merra_columns)
      )
    )
  })
  names(out) <- as.character(cells)
  if (length(cells) == 0 || sum(lengths(dates)) == 0) {
    return(out)
  }

  coordinates <- as.data.frame(s2::s2_cell_to_lnglat(cells))
  inside <- coordinates$x >= merra_bbox[[1]] &
    coordinates$x <= merra_bbox[[3]] &
    coordinates$y >= merra_bbox[[2]] &
    coordinates$y <= merra_bbox[[4]]
  if (any(!inside)) {
    stop(
      "All input locations must be inside the CONUS MERRA data bounds.",
      call. = FALSE
    )
  }

  requests <- do.call(
    rbind,
    lapply(seq_along(dates), function(i) {
      if (length(dates[[i]]) == 0) {
        return(NULL)
      }
      data.frame(location = i, row = seq_along(dates[[i]]), date = dates[[i]])
    })
  )
  requests$month <- format(requests$date, "%Y-%m")
  months <- unique(requests$month)
  files <- stats::setNames(rep(NA_character_, length(months)), months)
  local_data <- list()

  for (month in months[as.integer(substr(months, 1, 4)) >= 2017]) {
    path <- merra_local_file(month)
    if (!file.exists(path)) {
      next
    }
    dcf <- sub("[.]rds$", ".dcf", path)
    if (
      !file.exists(dcf) ||
        nrow(record <- read.dcf(dcf)) != 1 ||
        !"MERRA-Month" %in% colnames(record) ||
        !identical(unname(record[1, "MERRA-Month"]), month)
    ) {
      stop(
        "Local MERRA data need a matching one-record DCF manifest.",
        call. = FALSE
      )
    }
    expected <- merra_month_dates(month)
    if (
      !identical(
        unname(record[1, "Coverage-Start"]),
        as.character(min(expected))
      ) ||
        !identical(
          unname(record[1, "Coverage-End"]),
          as.character(max(expected))
        )
    ) {
      stop(
        "Local MERRA DCF does not describe a complete calendar month.",
        call. = FALSE
      )
    }
    local_data[[month]] <- merra_read_data(path, record[1, ])
    files[[month]] <- path
  }

  release_months <- names(files)[
    is.na(files) & as.integer(substr(names(files), 1, 4)) >= 2017
  ]
  if (length(release_months) > 0) {
    files[release_months] <- suppressWarnings(
      merra_release_files(release_months, ..., warn = FALSE)
    )
  }

  loaded <- new.env(parent = emptyenv())
  unavailable <- character()
  for (month in months) {
    which_requests <- which(requests$month == month)
    if (is.na(files[[month]])) {
      unavailable <- c(unavailable, month)
      next
    }
    data <- local_data[[month]]
    if (is.null(data)) {
      if (!exists(files[[month]], loaded, inherits = FALSE)) {
        assign(files[[month]], merra_read_data(files[[month]]), loaded)
      }
      data <- get(files[[month]], loaded, inherits = FALSE)
    }
    these <- requests[which_requests, , drop = FALSE]
    locations <- unique(these$location)
    grid <- unique(data$s2)
    closest <- s2::s2_closest_feature(
      s2::s2_cell_to_lnglat(cells[locations]),
      s2::s2_cell_center(s2::as_s2_cell(grid))
    )
    selected <- stats::setNames(grid[closest], locations)
    data_rows <- match(
      paste(these$date, selected[as.character(these$location)]),
      paste(data$date, data$s2)
    )
    if (anyNA(data_rows)) {
      stop("MERRA data do not cover every requested date.", call. = FALSE)
    }
    values <- data[data_rows, merra_columns, drop = FALSE]
    for (i in seq_len(nrow(these))) {
      out[[these$location[[i]]]][these$row[[i]], ] <- values[i, ]
    }
  }
  if (length(unavailable) > 0) {
    warning(
      "MERRA data are unavailable for ",
      paste(sort(unique(unavailable)), collapse = ", "),
      ". Returning missing values for those dates. Fully available months ",
      "can be built with install_merra_data(source = \"earthdata\").",
      call. = FALSE
    )
  }
  out
}

#' @param merra_month character vector of calendar months in `YYYY-MM` format,
#'   from 2017 onward
#' @param source either `"release"` to install the matching official half-year
#'   asset or `"earthdata"` to build each complete month from NASA
#' @param overwrite logical; recreate monthly data from the latest CMR listing?
#'   Matching daily source caches are still reused.
#' @param quiet logical; suppress progress messages?
#' @return `install_merra_data()` returns a named character vector of installed
#'   paths. Months in the same released half-year may have the same path.
#' @export
#' @rdname get_merra_data
install_merra_data <- function(
  merra_month,
  source = c("release", "earthdata"),
  overwrite = FALSE,
  quiet = FALSE
) {
  months <- merra_months(merra_month)
  source <- match.arg(source)
  stopifnot(
    "overwrite must be TRUE or FALSE" = is.logical(overwrite) &&
      length(overwrite) == 1 &&
      !is.na(overwrite),
    "quiet must be TRUE or FALSE" = is.logical(quiet) &&
      length(quiet) == 1 &&
      !is.na(quiet)
  )
  if (length(months) == 0) {
    return(stats::setNames(character(), character()))
  }
  if (source == "release") {
    return(merra_release_files(months, overwrite = overwrite, quiet = quiet))
  }
  unique_months <- unique(months)
  paths <- vapply(
    unique_months,
    merra_build_month,
    character(1),
    overwrite = overwrite,
    quiet = quiet
  )
  stats::setNames(unname(paths[match(months, names(paths))]), months)
}

merra_json <- function(url, query = list(), token = NULL) {
  check_installed("curl", "to access NASA Earthdata")
  check_installed("jsonlite", "to read NASA Earthdata metadata")
  if (length(query) > 0) {
    query <- paste0(
      curl::curl_escape(names(query)),
      "=",
      vapply(
        query,
        function(x) curl::curl_escape(paste(x, collapse = ",")),
        character(1)
      )
    )
    url <- paste0(url, "?", paste(query, collapse = "&"))
  }
  headers <- c("User-Agent" = "geomarker R package")
  if (!is.null(token)) {
    headers <- c(
      headers,
      "Echo-Token" = token,
      "Authorization" = paste("Bearer", token),
      "Accept" = "application/vnd.cmr-service-bridge.v3"
    )
  }
  response <- curl::curl_fetch_memory(
    url,
    handle = curl::new_handle(httpheader = headers, failonerror = FALSE)
  )
  body <- rawToChar(response$content)
  if (response$status_code < 200 || response$status_code >= 300) {
    stop("NASA Earthdata request failed: ", body, call. = FALSE)
  }
  jsonlite::fromJSON(body, simplifyVector = FALSE)
}

merra_granules <- function(month) {
  dates <- merra_month_dates(month)
  response <- merra_json(
    "https://cmr.earthdata.nasa.gov/search/granules.umm_json",
    list(
      collection_concept_id = "C1276812830-GES_DISC",
      temporal = paste0(min(dates), "T00:00:00Z,", max(dates), "T23:59:59Z"),
      page_size = 200
    )
  )
  items <- response$items
  if (is.null(items)) {
    items <- list()
  }
  granules <- lapply(items, function(item) {
    urls <- item$umm$RelatedUrls
    keep <- vapply(
      urls,
      function(x) identical(x$Subtype, "OPENDAP DATA"),
      logical(1)
    )
    if (sum(keep) != 1) {
      stop(
        "Each MERRA granule must have exactly one OPeNDAP URL.",
        call. = FALSE
      )
    }
    data.frame(
      date = as.Date(substr(
        item$umm$TemporalExtent$RangeDateTime$BeginningDateTime,
        1,
        10
      )),
      granule_ur = item$umm$GranuleUR,
      concept_id = item$meta[["concept-id"]],
      revision_id = as.integer(item$meta[["revision-id"]]),
      stringsAsFactors = FALSE
    )
  })
  if (length(granules) == 0) {
    granules <- data.frame(
      date = as.Date(character()),
      granule_ur = character(),
      concept_id = character(),
      revision_id = integer()
    )
  } else {
    granules <- do.call(rbind, granules)
  }
  if (
    anyNA(granules) ||
      anyDuplicated(granules$date) ||
      !setequal(granules$date, dates)
  ) {
    stop(
      "MERRA month is not complete in CMR; every calendar date is required.",
      call. = FALSE
    )
  }
  granules[match(dates, granules$date), , drop = FALSE]
}

merra_daily_values <- function(hourly) {
  if (
    !is.list(hourly) ||
      !identical(names(hourly), merra_variables) ||
      !all(vapply(hourly, is.matrix, logical(1))) ||
      !all(vapply(hourly, ncol, integer(1)) == 24) ||
      !all(vapply(hourly, nrow, integer(1)) == nrow(hourly[[1]]))
  ) {
    stop(
      "MERRA variables must share one grid with 24 hourly slices.",
      call. = FALSE
    )
  }
  daily <- lapply(hourly, rowMeans, na.rm = FALSE)
  out <- as.data.frame(lapply(daily, function(x) x * 1e9))
  names(out) <- merra_columns[1:5]
  out$merra_pm25 <- with(
    out,
    merra_dust + merra_oc + merra_bc + merra_ss + merra_so4 * 132.14 / 96.06
  )
  out
}

create_daily_merra_data <- function(
  granule,
  source_dir,
  token,
  overwrite = FALSE
) {
  check_installed("terra", "to process NASA MERRA subsets")
  stem <- paste0(
    gsub("[^A-Za-z0-9._-]", "_", granule[["granule_ur"]]),
    "-cmr-r",
    granule[["revision_id"]]
  )
  subset_file <- file.path(source_dir, "subsets", paste0(stem, "--conus.nc4"))
  daily_file <- file.path(source_dir, "daily", paste0(stem, ".rds"))
  daily_dcf <- paste0(daily_file, ".dcf")
  if (
    file.exists(subset_file) &&
      file.exists(daily_file) &&
      file.exists(daily_dcf) &&
      !overwrite
  ) {
    record <- as.list(read.dcf(daily_dcf)[1, ])
    if (
      identical(record[["Granule-Concept-ID"]], granule[["concept_id"]]) &&
        identical(
          record[["Granule-Revision-ID"]],
          as.character(granule[["revision_id"]])
        ) &&
        identical(
          record[["Subset-SHA256"]],
          digest::digest(
            subset_file,
            algo = "sha256",
            file = TRUE
          )
        )
    ) {
      data <- merra_read_data(daily_file)
      if (identical(unique(data$date), as.Date(granule[["date"]]))) {
        return(list(
          data = data,
          subset_url = record[["Subset-URL"]],
          subset_sha256 = record[["Subset-SHA256"]]
        ))
      }
    }
  }

  response <- merra_json(
    paste0(
      "https://cmr.earthdata.nasa.gov/service-bridge/ous/collection/",
      "C1276812830-GES_DISC"
    ),
    list(
      granules = granule[["concept_id"]],
      variable_aliases = merra_variables,
      `bounding-box` = merra_bbox,
      format = "nc4",
      `dap-version` = 4
    ),
    token
  )
  subset_url <- unlist(response$items, use.names = FALSE)
  if (
    length(response$warnings) > 0 ||
      length(subset_url) != 1 ||
      !startsWith(subset_url, "https://opendap.earthdata.nasa.gov/")
  ) {
    stop("NASA did not return exactly one bounded MERRA subset.", call. = FALSE)
  }

  dir.create(dirname(subset_file), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(subset_file) || overwrite) {
    partial <- paste0(subset_file, ".partial")
    on.exit(unlink(partial), add = TRUE)
    curl::curl_download(
      subset_url,
      partial,
      quiet = TRUE,
      mode = "wb",
      handle = curl::new_handle(
        httpheader = c(
          "Echo-Token" = token,
          "Authorization" = paste("Bearer", token)
        ),
        failonerror = TRUE
      )
    )
    if (
      !file.rename(partial, subset_file) &&
        !file.copy(partial, subset_file, overwrite = TRUE)
    ) {
      stop("Could not move the MERRA subset into the cache.", call. = FALSE)
    }
  }

  rasters <- lapply(merra_variables, function(variable) {
    raster <- tryCatch(
      terra::rast(subset_file, subds = variable),
      error = function(e) NULL
    )
    if (is.null(raster)) {
      raster <- tryCatch(
        terra::rast(paste0(
          'NETCDF:"',
          normalizePath(subset_file),
          '":',
          variable
        )),
        error = function(e) NULL
      )
    }
    if (is.null(raster) || terra::nlyr(raster) != 24) {
      stop(
        "MERRA subset is missing 24 hourly slices for ",
        variable,
        ".",
        call. = FALSE
      )
    }
    raster
  })
  names(rasters) <- merra_variables
  if (
    any(
      !vapply(
        rasters[-1],
        terra::compareGeom,
        logical(1),
        rasters[[1]],
        stopOnError = FALSE
      )
    )
  ) {
    stop("MERRA variables do not share one spatial grid.", call. = FALSE)
  }
  daily <- merra_daily_values(lapply(rasters, terra::values, mat = TRUE))
  coordinates <- terra::xyFromCell(
    rasters[[1]],
    seq_len(terra::ncell(rasters[[1]]))
  )
  keep <- stats::complete.cases(daily) &
    coordinates[, 1] >= merra_bbox[[1]] &
    coordinates[, 1] <= merra_bbox[[3]] &
    coordinates[, 2] >= merra_bbox[[2]] &
    coordinates[, 2] <= merra_bbox[[4]]
  data <- data.frame(
    date = rep(as.Date(granule[["date"]]), sum(keep)),
    daily[keep, , drop = FALSE],
    s2 = as.character(s2::as_s2_cell(s2::s2_lnglat(
      coordinates[keep, 1],
      coordinates[keep, 2]
    ))),
    row.names = NULL
  )[, merra_data_columns]
  if (nrow(data) == 0) {
    stop("MERRA subset does not contain a valid CONUS grid.", call. = FALSE)
  }

  merra_save_data(data, daily_file)
  subset_sha256 <- digest::digest(subset_file, algo = "sha256", file = TRUE)
  write.dcf(
    as.data.frame(
      list(
        `Granule-UR` = granule[["granule_ur"]],
        `Granule-Concept-ID` = granule[["concept_id"]],
        `Granule-Revision-ID` = as.character(granule[["revision_id"]]),
        `Subset-URL` = subset_url,
        `Subset-SHA256` = subset_sha256
      ),
      check.names = FALSE
    ),
    daily_dcf
  )
  list(data = data, subset_url = subset_url, subset_sha256 = subset_sha256)
}

merra_build_month <- function(month, overwrite = FALSE, quiet = FALSE) {
  dates <- merra_month_dates(month)
  if (max(dates) >= Sys.Date()) {
    stop(
      "MERRA source builds require a fully elapsed calendar month.",
      call. = FALSE
    )
  }
  asset <- merra_local_file(month)
  dcf <- sub("[.]rds$", ".dcf", asset)
  if (file.exists(asset) && !overwrite) {
    if (!file.exists(dcf)) {
      stop("Local MERRA data exist without their DCF manifest.", call. = FALSE)
    }
    record <- read.dcf(dcf)
    if (
      nrow(record) != 1 ||
        !"MERRA-Month" %in% colnames(record) ||
        !identical(unname(record[1, "MERRA-Month"]), month)
    ) {
      stop(
        "Local MERRA data need a matching one-record DCF manifest.",
        call. = FALSE
      )
    }
    merra_read_data(asset, record[1, ])
    return(asset)
  }
  token <- Sys.getenv("EARTHDATA_TOKEN")
  if (!nzchar(token)) {
    stop(
      "Set EARTHDATA_TOKEN before building MERRA data from source.",
      call. = FALSE
    )
  }
  if (!quiet) {
    message("Discovering and building complete MERRA month ", month, "...")
  }
  granules <- merra_granules(month)
  source_dir <- file.path(geomarker_data_dir("merra"), "source", month)
  days <- lapply(seq_len(nrow(granules)), function(i) {
    if (!quiet) {
      message("  ", granules$date[[i]], " (", i, "/", nrow(granules), ")")
    }
    create_daily_merra_data(
      granules[i, , drop = FALSE],
      source_dir,
      token,
      overwrite = FALSE
    )
  })
  data <- do.call(rbind, lapply(days, `[[`, "data"))
  row.names(data) <- NULL
  merra_save_data(data, asset)
  record <- list(
    `MERRA-Year` = substr(month, 1, 4),
    `MERRA-Month` = month,
    Distribution = "local-earthdata",
    `Coverage-Start` = as.character(min(dates)),
    `Coverage-End` = as.character(max(dates)),
    `Asset-Days` = as.character(length(dates)),
    `Source-Collection-ID` = "C1276812830-GES_DISC",
    `Source-Collection` = "M2T1NXAER 5.12.4",
    `Source-Granule-URs` = paste(granules$granule_ur, collapse = ","),
    `Source-Granule-Concept-Revisions` = paste0(
      granules$concept_id,
      ":",
      granules$revision_id,
      collapse = ","
    ),
    `Source-Subset-URLs` = paste(
      vapply(days, `[[`, "", "subset_url"),
      collapse = ","
    ),
    `Source-Subset-SHA256` = paste0(
      dates,
      "=",
      vapply(days, `[[`, "", "subset_sha256"),
      collapse = ","
    ),
    `Source-Variables` = paste(merra_variables, collapse = ","),
    `Source-BBox-West-South-East-North` = paste(merra_bbox, collapse = ","),
    `Transformation-ID` = "m2t1nxaer-daily-pm25-v1",
    `PM25-Formula` = paste(
      "DUSMASS25 + OCSMASS + BCSMASS + SSSMASS25 +",
      "SO4SMASS * 132.14 / 96.06"
    ),
    `Asset-Name` = basename(asset),
    `Asset-Bytes` = as.character(file.info(asset)$size),
    `Asset-SHA256` = digest::digest(asset, algo = "sha256", file = TRUE),
    `Asset-Rows` = as.character(nrow(data)),
    `Asset-Grid-Cells` = as.character(length(unique(data$s2))),
    `Asset-Schema` = merra_schema,
    `Asset-Compression` = "RDS-XZ",
    `Grid-Longitude-Resolution-Degrees` = "0.625",
    `Grid-Latitude-Resolution-Degrees` = "0.5",
    Attribution = "NASA Global Modeling and Assimilation Office MERRA-2",
    `Built-UTC` = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  write.dcf(as.data.frame(record, check.names = FALSE), dcf)
  merra_read_data(asset, record)
  asset
}

install_merra_geomarker_fixture <- function(
  cell,
  dates,
  output_dir,
  source_files = NULL
) {
  cell <- geomarker_fixture_cell(cell)
  dates <- geomarker_fixture_dates(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  months <- unique(format(dates, "%Y-%m"))
  if (is.null(source_files)) {
    source_files <- install_merra_data(months, quiet = TRUE)
  }
  if (anyNA(source_files) || !all(file.exists(source_files))) {
    stop("MERRA fixture source data are unavailable.", call. = FALSE)
  }
  source_files <- unique(source_files)
  source_data <- do.call(rbind, lapply(source_files, merra_read_data))
  bbox <- geomarker_fixture_cell_bbox(cell)
  grid <- unique(source_data$s2)
  coordinates <- as.data.frame(s2::s2_cell_to_lnglat(s2::as_s2_cell(grid)))
  keep_grid <- grid[
    coordinates$x >= bbox[[1]] - 0.625 &
      coordinates$x <= bbox[[3]] + 0.625 &
      coordinates$y >= bbox[[2]] - 0.5 &
      coordinates$y <= bbox[[4]] + 0.5
  ]

  for (month in months) {
    data <- source_data[
      format(source_data$date, "%Y-%m") == month &
        source_data$s2 %in% keep_grid,
      ,
      drop = FALSE
    ]
    expected <- merra_month_dates(month)
    if (!identical(sort(unique(data$date)), expected) || nrow(data) == 0) {
      stop(
        "MERRA fixture source does not contain a complete requested month.",
        call. = FALSE
      )
    }
    destination <- file.path(
      output_dir,
      "merra",
      "local",
      paste0("merra2_", month, "_pm25.rds")
    )
    merra_save_data(data, destination)
    record <- list(
      `MERRA-Year` = substr(month, 1, 4),
      `MERRA-Month` = month,
      Distribution = "fixture",
      `Coverage-Start` = as.character(min(expected)),
      `Coverage-End` = as.character(max(expected)),
      `Asset-Days` = as.character(length(expected)),
      `Source-Asset-SHA256` = paste(
        vapply(source_files, digest::digest, "", algo = "sha256", file = TRUE),
        collapse = ","
      ),
      `Source-Variables` = paste(merra_variables, collapse = ","),
      `Transformation-ID` = "m2t1nxaer-daily-pm25-v1",
      `PM25-Formula` = paste(
        "DUSMASS25 + OCSMASS + BCSMASS + SSSMASS25 +",
        "SO4SMASS * 132.14 / 96.06"
      ),
      `Asset-Name` = basename(destination),
      `Asset-Bytes` = as.character(file.info(destination)$size),
      `Asset-SHA256` = digest::digest(
        destination,
        algo = "sha256",
        file = TRUE
      ),
      `Asset-Rows` = as.character(nrow(data)),
      `Asset-Grid-Cells` = as.character(length(unique(data$s2))),
      `Asset-Schema` = merra_schema,
      `Asset-Compression` = "RDS-XZ",
      Attribution = "Fixture derived from the recorded MERRA source asset",
      `Built-UTC` = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    write.dcf(
      as.data.frame(record, check.names = FALSE),
      sub("[.]rds$", ".dcf", destination)
    )
  }
  invisible(output_dir)
}
