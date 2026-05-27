#' MODIS enhanced vegetation index data
#'
#' Links annual enhanced vegetation index (EVI) based on the location of each
#' s2 cell and dates. For each year, EVI is defined as the mean EVI within
#' `buffer` meters of the location, extracted from a 250 m annual MODIS
#' Terra/Aqua EVI composite whose pixels are the median of all good or
#' marginal-quality 16-day EVI observations in that year.
#'
#' Data are downloaded from the
#' [Microsoft Planetary Computer](https://planetarycomputer.microsoft.com/)
#' mirror of NASA LP DAAC MOD13Q1/MYD13Q1 Version 6.1 Cloud Optimized GeoTIFFs.
#' See <https://planetarycomputer.microsoft.com/dataset/modis-13Q1-061>
#' for collection details. Source 16-day EVI and pixel reliability rasters are
#' downloaded to a temporary directory only when an annual composite is missing.
#' EVI source rasters are downloaded by MODIS tile; 14 MODIS tiles intersect the
#' contiguous United States.
#' Annual composite rasters are cached in `geomarker_data_dir(subdir)`. EVI
#' source raster values are scaled by 0.0001 while creating annual composites.
#' EVI is a greenness index designed to emphasize photosynthetically active
#' vegetation while reducing atmospheric and soil background effects. In this
#' MODIS product, scaled EVI values are expected to range from about -0.2 to
#' 1.0; higher values indicate denser or more vigorous green vegetation, values
#' near 0 indicate little green vegetation, and negative values usually indicate
#' water, snow, cloud, or other non-vegetated surfaces or artifacts.
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param overwrite logical; overwrite cached annual EVI rasters?
#' @param quiet logical; show download progress messages?
#' @param subdir character; subdirectory within the geomarker data folder
#' @return a list of numeric vectors of annual EVI values.
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' get_evi_data(s2cd_example_cincy(2L))
#' get_evi_data(s2cd_example_cincy(2L), buffer = 800)
get_evi_data <- function(
  x,
  buffer = 400,
  overwrite = FALSE,
  quiet = FALSE,
  subdir = "evi"
) {
  stopifnot(
    "x must be a s2_cell_dates vector" = is_s2cd(x),
    "all dates in x must be on or after 2000-02-18" = s2cd_within(
      x,
      date_range = c(as.Date("2000-02-17"), as.Date("9999-01-01"))
    ),
    "buffer must be numeric" = is.numeric(buffer),
    "buffer must be length one" = length(buffer) == 1,
    "buffer must be non-missing" = !is.na(buffer),
    "overwrite must be logical" = is.logical(overwrite),
    "overwrite must be length one" = length(overwrite) == 1,
    "quiet must be logical" = is.logical(quiet),
    "quiet must be length one" = length(quiet) == 1
  )
  check_installed("terra", "to read EVI raster data.")

  x_dates <- s2cd_dates(x)
  if (length(evi_all_dates(x_dates)) == 0) {
    out <- rep(list(numeric(0)), length(x))
    return(stats::setNames(out, as.character(x)))
  }

  x_points <- evi_s2cd_points(x)
  years <- evi_requested_years(x_dates)
  if (nzchar(Sys.getenv("R_GEOMARKER_NO_DOWNLOAD"))) {
    annual_files <- evi_cached_annual_composite_files(years, subdir = subdir)
    evi_values <- evi_extract_annual_values(x, annual_files, buffer = buffer)
    return(evi_summarize_annual_values(
      values = evi_values,
      x_dates = x_dates,
      names = as.character(x)
    ))
  }

  date_range <- evi_search_date_range(x_dates)
  evi_items <- evi_planetary_computer_items(
    bbox = evi_points_bbox(x_points),
    date_range = date_range
  )
  evi_items <- evi_filter_items_to_points(evi_items, x_points)
  if (nrow(evi_items) == 0) {
    stop(
      "No EVI rasters were found for the requested locations and dates.",
      call. = FALSE
    )
  }
  evi_items <- evi_items[
    format(evi_items$start_date, "%Y") %in% years,
    ,
    drop = FALSE
  ]
  if (nrow(evi_items) == 0) {
    stop(
      "No EVI rasters were found for the requested years.",
      call. = FALSE
    )
  }

  annual_files <- evi_annual_composite_files(
    items = evi_items,
    years = years,
    overwrite = overwrite,
    quiet = quiet,
    subdir = subdir
  )
  evi_values <- evi_extract_annual_values(x, annual_files, buffer = buffer)
  evi_summarize_annual_values(
    values = evi_values,
    x_dates = x_dates,
    names = as.character(x)
  )
}

evi_planetary_computer_items <- function(
  bbox,
  date_range,
  limit = 100L
) {
  params <- c(
    bbox = paste(bbox, collapse = ","),
    datetime = paste0(
      format(date_range[1], "%Y-%m-%dT00:00:00Z"),
      "/",
      format(date_range[2], "%Y-%m-%dT23:59:59Z")
    ),
    limit = as.character(limit)
  )
  url <- paste0(
    "https://planetarycomputer.microsoft.com/api/stac/v1/",
    "collections/modis-13Q1-061/items?",
    paste(
      names(params),
      vapply(params, utils::URLencode, character(1), reserved = TRUE),
      sep = "=",
      collapse = "&"
    )
  )

  out <- list()
  while (!is.na(url) && length(url) == 1 && nzchar(url)) {
    json <- evi_fetch_url(url)
    parsed <- evi_parse_stac_items(json)
    if (nrow(parsed) > 0) {
      out[[length(out) + 1]] <- parsed
    }
    url <- evi_parse_next_href(json)
  }
  if (length(out) == 0) {
    return(data.frame(
      id = character(0),
      platform = character(0),
      xmin = numeric(0),
      ymin = numeric(0),
      xmax = numeric(0),
      ymax = numeric(0),
      start_date = as.Date(character(0)),
      end_date = as.Date(character(0)),
      tile = character(0),
      href = character(0),
      quality_href = character(0)
    ))
  }
  out <- do.call(rbind, out)
  out <- out[!duplicated(out$id), , drop = FALSE]
  out[order(out$start_date, out$id), , drop = FALSE]
}

evi_download_assets <- function(
  hrefs,
  label = "EVI rasters",
  overwrite = FALSE,
  quiet = FALSE,
  subdir = "evi",
  dest_dir = NULL
) {
  if (is.null(dest_dir)) {
    dest_dir <- geomarker_data_dir(subdir = subdir)
  } else if (!file.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  dests <- file.path(
    dest_dir,
    vapply(hrefs, url_to_filename, character(1), etag = FALSE)
  )
  needs_download <- overwrite | !file.exists(dests)
  n_total <- length(hrefs)
  n_download <- sum(needs_download)
  if (!quiet) {
    message(sprintf(
      "%s: %s found, %s cached, %s to download.",
      label,
      n_total,
      n_total - n_download,
      n_download
    ))
  }
  if (n_download == 0) {
    return(dests)
  }

  download_index <- 0L
  if (!quiet) {
    on.exit(cat("\n", file = stderr()), add = TRUE)
  }
  for (i in which(needs_download)) {
    download_index <- download_index + 1L
    dests[[i]] <- evi_download_asset(
      href = hrefs[[i]],
      dest = dests[[i]],
      download_index = download_index,
      download_total = n_download,
      label = label,
      quiet = quiet
    )
  }
  dests
}

evi_download_asset <- function(
  href,
  dest,
  download_index,
  download_total,
  label = "EVI rasters",
  quiet = FALSE
) {
  if (nzchar(Sys.getenv("R_GEOMARKER_NO_DOWNLOAD"))) {
    stop(
      "The envvar R_GEOMARKER_NO_DOWNLOAD is set, but ",
      dest,
      " does not exist",
      call. = FALSE
    )
  }

  signed_href <- evi_sign_asset(href)
  tmp <- tempfile(pattern = "geomarker_evi_")
  on.exit(unlink(tmp), add = TRUE)
  if (!quiet) {
    evi_write_download_progress(label, download_index, download_total)
  }
  err_context <- paste0(
    "Download failed.\n",
    "URL: ",
    href,
    "\n",
    "Expected file path: ",
    dest,
    "\n",
    "If you can download this file manually, place it at the path above and retry."
  )
  tryCatch(
    {
      if (requireNamespace("curl", quietly = TRUE)) {
        curl::curl_download(
          signed_href,
          destfile = tmp,
          quiet = TRUE,
          mode = "wb"
        )
      } else {
        utils::download.file(
          signed_href,
          destfile = tmp,
          mode = "wb",
          quiet = TRUE
        )
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

evi_write_download_progress <- function(label, download_index, download_total) {
  msg <- evi_download_progress_message(label, download_index, download_total)
  cat("\r", msg, evi_clear_line_padding(msg), sep = "", file = stderr())
  utils::flush.console()
  invisible(msg)
}

evi_download_progress_message <- function(
  label,
  download_index,
  download_total
) {
  sprintf(
    "Downloading %s %s of %s",
    label,
    download_index,
    download_total
  )
}

evi_clear_line_padding <- function(msg) {
  width <- getOption("width", 80)
  strrep(" ", max(0, width - nchar(msg)))
}

evi_sign_asset <- function(href) {
  json <- evi_fetch_url(paste0(
    "https://planetarycomputer.microsoft.com/api/sas/v1/sign?href=",
    utils::URLencode(href, reserved = TRUE)
  ))
  signed_href <- evi_json_match('"href"\\s*:\\s*"([^"]+)"', json)
  if (is.na(signed_href)) {
    stop("Could not sign the Planetary Computer EVI asset.", call. = FALSE)
  }
  evi_json_unescape(signed_href)
}

evi_annual_composite_files <- function(
  items,
  years,
  overwrite = FALSE,
  quiet = FALSE,
  subdir = "evi"
) {
  items$year <- format(items$start_date, "%Y")
  items <- items[items$year %in% years, , drop = FALSE]
  items <- items[
    order(items$year, items$tile, items$start_date, items$id),
    ,
    drop = FALSE
  ]
  groups <- unique(items[, c("year", "tile"), drop = FALSE])
  if (anyNA(groups$tile) || any(!nzchar(groups$tile))) {
    stop(
      "Could not determine the MODIS tile for at least one EVI raster.",
      call. = FALSE
    )
  }

  dests <- file.path(
    geomarker_data_dir(subdir = subdir),
    evi_annual_composite_filename(groups$year, groups$tile)
  )
  needs_build <- overwrite | !file.exists(dests)
  n_total <- length(dests)
  n_build <- sum(needs_build)
  if (!quiet) {
    message(sprintf(
      "Annual EVI composites: %s required, %s cached, %s to build.",
      n_total,
      n_total - n_build,
      n_build
    ))
  }
  if (n_build > 0 && nzchar(Sys.getenv("R_GEOMARKER_NO_DOWNLOAD"))) {
    stop(
      "The envvar R_GEOMARKER_NO_DOWNLOAD is set, but at least one annual ",
      "EVI composite does not exist: ",
      dests[which(needs_build)[[1]]],
      call. = FALSE
    )
  }

  build_index <- 0L
  for (i in which(needs_build)) {
    build_index <- build_index + 1L
    group_items <- items[
      items$year == groups$year[[i]] & items$tile == groups$tile[[i]],
      ,
      drop = FALSE
    ]
    evi_build_annual_composite(
      items = group_items,
      dest = dests[[i]],
      build_index = build_index,
      build_total = n_build,
      quiet = quiet
    )
  }

  records <- lapply(
    seq_len(nrow(groups)),
    function(i) {
      group_items <- items[
        items$year == groups$year[[i]] & items$tile == groups$tile[[i]],
        ,
        drop = FALSE
      ]
      data.frame(
        year = groups$year[[i]],
        tile = groups$tile[[i]],
        xmin = evi_bbox_value(group_items$xmin, min),
        ymin = evi_bbox_value(group_items$ymin, min),
        xmax = evi_bbox_value(group_items$xmax, max),
        ymax = evi_bbox_value(group_items$ymax, max),
        file = dests[[i]]
      )
    }
  )
  do.call(rbind, records)
}

evi_annual_composite_filename <- function(year, tile) {
  sprintf("modis-13Q1-061-%s-%s-annual-evi-median-q01.tif", year, tile)
}

evi_cached_annual_composite_files <- function(years, subdir = "evi") {
  evi_dir <- geomarker_data_dir(subdir = subdir)
  files <- list.files(
    evi_dir,
    pattern = "^modis-13Q1-061-[0-9]{4}-h[0-9]{2}v[0-9]{2}-annual-evi-median-q01[.]tif$",
    full.names = TRUE
  )
  file_info <- evi_parse_annual_composite_filenames(basename(files))
  keep <- file_info$year %in% years
  n_keep <- sum(keep)
  out <- data.frame(
    year = file_info$year[keep],
    tile = file_info$tile[keep],
    xmin = rep(NA_real_, n_keep),
    ymin = rep(NA_real_, n_keep),
    xmax = rep(NA_real_, n_keep),
    ymax = rep(NA_real_, n_keep),
    file = files[keep]
  )
  missing_years <- setdiff(years, unique(out$year))
  if (length(missing_years) > 0) {
    stop(
      "The envvar R_GEOMARKER_NO_DOWNLOAD is set, but no cached annual ",
      "EVI composite exists for year(s): ",
      paste(missing_years, collapse = ", "),
      call. = FALSE
    )
  }
  out[order(out$year, out$tile), , drop = FALSE]
}

evi_parse_annual_composite_filenames <- function(filenames) {
  pattern <- paste0(
    "^modis-13Q1-061-",
    "([0-9]{4})-",
    "(h[0-9]{2}v[0-9]{2})-",
    "annual-evi-median-q01[.]tif$"
  )
  matches <- regexec(pattern, filenames, perl = TRUE)
  parsed <- regmatches(filenames, matches)
  data.frame(
    year = vapply(
      parsed,
      \(x) if (length(x) >= 2) x[[2]] else NA_character_,
      character(1)
    ),
    tile = vapply(
      parsed,
      \(x) if (length(x) >= 3) x[[3]] else NA_character_,
      character(1)
    )
  )
}

evi_build_annual_composite <- function(
  items,
  dest,
  build_index,
  build_total,
  quiet = FALSE
) {
  if (!quiet) {
    message(sprintf(
      "Building annual EVI composite %s of %s: %s %s",
      build_index,
      build_total,
      items$year[[1]],
      items$tile[[1]]
    ))
  }

  tmp_dir <- tempfile(pattern = "geomarker_evi_sources_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  source_files <- evi_download_assets(
    hrefs = c(items$href, items$quality_href),
    label = sprintf(
      "EVI source files for %s %s",
      items$year[[1]],
      items$tile[[1]]
    ),
    overwrite = FALSE,
    quiet = quiet,
    dest_dir = tmp_dir
  )
  n_items <- nrow(items)
  evi_write_annual_composite(
    evi_files = source_files[seq_len(n_items)],
    quality_files = source_files[n_items + seq_len(n_items)],
    dest = dest
  )
}

evi_write_annual_composite <- function(evi_files, quality_files, dest) {
  stopifnot(
    "evi_files and quality_files must have the same length" = length(
      evi_files
    ) ==
      length(quality_files)
  )
  layers <- Map(evi_quality_filtered_raster, evi_files, quality_files)
  evi_write_annual_layers(layers, dest)
}

evi_write_annual_layers <- function(layers, dest) {
  annual <- Reduce(c, layers) |>
    terra::app(fun = "median", na.rm = TRUE)
  names(annual) <- "evi"

  if (!file.exists(dirname(dest))) {
    dir.create(dirname(dest), recursive = TRUE)
  }
  tmp <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(dest)), "-"),
    fileext = ".tif",
    tmpdir = dirname(dest)
  )
  on.exit(unlink(tmp), add = TRUE)
  terra::writeRaster(
    annual,
    tmp,
    overwrite = TRUE,
    datatype = "FLT4S",
    NAflag = -9999,
    gdal = c("COMPRESS=LZW")
  )
  if (file.exists(dest)) {
    unlink(dest)
  }
  ok <- file.rename(tmp, dest)
  if (!ok && !file.copy(tmp, dest, overwrite = TRUE)) {
    stop(
      "Failed to move annual EVI composite into destination: ",
      dest,
      call. = FALSE
    )
  }
  invisible(dest)
}

evi_quality_filtered_fixture_raster <- function(href, quality_href, cell) {
  r <- evi_sign_asset(href) |>
    terra::rast()
  q <- evi_sign_asset(quality_href) |>
    terra::rast()
  # COG tags store a display scale; reset it before applying the EVI scale.
  terra::scoff(r) <- cbind(1, 0)
  terra::scoff(q) <- cbind(1, 0)
  r <- geomarker_fixture_crop_to_cell(r, cell = cell)
  q <- geomarker_fixture_crop_to_cell(q, cell = cell)
  terra::ifel(q <= 1, r * 0.0001, NA)
}

evi_fixture_item_overlaps_cell <- function(href, cell) {
  r <- terra::rast(evi_sign_asset(href))
  cover <-
    cell |>
    s2::as_s2_cell() |>
    s2::s2_cell_polygon() |>
    sf::st_as_sfc() |>
    sf::st_transform(terra::crs(r)) |>
    terra::vect()
  evi_extents_overlap(terra::ext(r), terra::ext(cover))
}

evi_extents_overlap <- function(x, y) {
  terra::xmin(x) <= terra::xmax(y) &&
    terra::xmax(x) >= terra::xmin(y) &&
    terra::ymin(x) <= terra::ymax(y) &&
    terra::ymax(x) >= terra::ymin(y)
}

evi_quality_filtered_raster <- function(evi_file, quality_file) {
  r <- terra::rast(evi_file)
  q <- terra::rast(quality_file)
  # COG tags store a display scale; reset it before applying the EVI scale.
  terra::scoff(r) <- cbind(1, 0)
  terra::scoff(q) <- cbind(1, 0)
  terra::ifel(q <= 1, r * 0.0001, NA)
}

evi_extract_annual_values <- function(x, annual_files, buffer) {
  cell_ids <- as.character(s2::as_s2_cell(x))
  unique_cell_ids <- unique(cell_ids)
  unique_cell_index <- match(cell_ids, unique_cell_ids)

  x_vect <- s2_cell_to_vect(s2::as_s2_cell(unique_cell_ids))
  x_points <- evi_cell_points(unique_cell_ids)
  years <- unique(annual_files$year)
  values <- matrix(
    NA_real_,
    nrow = length(years),
    ncol = length(unique_cell_ids),
    dimnames = list(years, unique_cell_ids)
  )
  for (i in seq_len(nrow(annual_files))) {
    point_index <- evi_points_in_bbox(
      points = x_points,
      bbox = as.numeric(annual_files[i, c("xmin", "ymin", "xmax", "ymax")])
    )
    if (length(point_index) == 0) {
      next
    }
    r <- terra::rast(annual_files$file[[i]])
    x_buffer <-
      terra::project(x_vect[point_index, ], r) |>
      terra::buffer(width = buffer)
    extracted <- suppressWarnings(terra::extract(
      r,
      x_buffer,
      fun = evi_mean,
      ID = FALSE
    ))[[1]]
    extracted <- as.numeric(extracted)
    year_index <- match(annual_files$year[[i]], years)
    current <- values[year_index, point_index]
    fill <- is.na(current)
    current[fill] <- extracted[fill]
    values[year_index, point_index] <- current
  }
  values[, unique_cell_index, drop = FALSE]
}

evi_mean <- function(x, ...) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

evi_summarize_annual_values <- function(
  values,
  x_dates,
  names
) {
  out <- vector("list", length(x_dates))
  for (i in seq_along(x_dates)) {
    years <- unique(format(x_dates[[i]], "%Y"))
    out[[i]] <- stats::setNames(as.numeric(values[years, i]), years)
  }
  stats::setNames(out, names)
}

evi_search_date_range <- function(x_dates) {
  dates <- evi_all_dates(x_dates)
  date_range <- range(dates)
  c(
    as.Date(sprintf("%s-01-01", format(date_range[1], "%Y"))),
    as.Date(sprintf("%s-12-31", format(date_range[2], "%Y")))
  )
}

evi_all_dates <- function(x_dates) {
  dates <- do.call(c, x_dates)
  if (is.null(dates)) {
    return(as.Date(character(0)))
  }
  dates
}

evi_requested_years <- function(x_dates) {
  unique(format(evi_all_dates(x_dates), "%Y"))
}

evi_s2cd_points <- function(x) {
  evi_cell_points(s2::as_s2_cell(x))
}

evi_cell_points <- function(x) {
  lnglat <- s2::s2_cell_to_lnglat(s2::as_s2_cell(x)) |>
    as.data.frame()
  data.frame(x = lnglat$x, y = lnglat$y)
}

evi_points_bbox <- function(points, expand = 1e-6) {
  c(
    min(points$x) - expand,
    min(points$y) - expand,
    max(points$x) + expand,
    max(points$y) + expand
  )
}

evi_s2cd_bbox <- function(x, expand = 1e-6) {
  evi_points_bbox(evi_s2cd_points(x), expand = expand)
}

evi_points_in_bbox <- function(points, bbox) {
  if (anyNA(bbox)) {
    return(seq_len(nrow(points)))
  }
  which(
    points$x >= bbox[[1]] &
      points$y >= bbox[[2]] &
      points$x <= bbox[[3]] &
      points$y <= bbox[[4]]
  )
}

evi_bbox_value <- function(x, fun) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  fun(x, na.rm = TRUE)
}

evi_filter_items_to_points <- function(items, points) {
  if (nrow(items) == 0) {
    return(items)
  }
  keep <- vapply(
    seq_len(nrow(items)),
    function(i) {
      item_bbox <- as.numeric(items[i, c("xmin", "ymin", "xmax", "ymax")])
      if (anyNA(item_bbox)) {
        return(TRUE)
      }
      any(
        points$x >= item_bbox[[1]] &
          points$y >= item_bbox[[2]] &
          points$x <= item_bbox[[3]] &
          points$y <= item_bbox[[4]]
      )
    },
    FUN.VALUE = logical(1)
  )
  items[keep, , drop = FALSE]
}

evi_item_tile <- function(id, href) {
  text <- paste(id, href)
  match <- regexpr(
    "h[0-9]{2}v[0-9]{2}",
    text,
    perl = TRUE,
    ignore.case = TRUE
  )
  if (match[[1]] == -1) {
    return(NA_character_)
  }
  tolower(regmatches(text, match))
}

install_evi_geomarker_fixture <- function(
  cell,
  dates,
  output_dir,
  overwrite = FALSE,
  quiet = FALSE,
  subdir = "evi"
) {
  check_installed("terra", "to create EVI fixture data.")
  cell <- geomarker_fixture_cell(cell)
  dates <- geomarker_fixture_dates(dates)
  years <- evi_requested_years(list(dates))
  output_dir <- geomarker_fixture_output_dir(output_dir)
  evi_dir <- file.path(output_dir, subdir)
  dir.create(evi_dir, showWarnings = FALSE, recursive = TRUE)

  items <- evi_planetary_computer_items(
    bbox = geomarker_fixture_cell_bbox(cell),
    date_range = evi_search_date_range(list(dates))
  )
  items$year <- format(items$start_date, "%Y")
  items <- items[items$year %in% years, , drop = FALSE]
  items <- items[
    order(items$year, items$tile, items$start_date, items$id),
    ,
    drop = FALSE
  ]
  groups <- unique(items[, c("year", "tile"), drop = FALSE])
  if (nrow(groups) == 0) {
    stop(
      "No EVI rasters were found for the requested fixture dates.",
      call. = FALSE
    )
  }
  keep_group <- vapply(
    seq_len(nrow(groups)),
    \(i) {
      group_items <- items[
        items$year == groups$year[[i]] & items$tile == groups$tile[[i]],
        ,
        drop = FALSE
      ]
      evi_fixture_item_overlaps_cell(group_items$href[[1]], cell)
    },
    FUN.VALUE = logical(1)
  )
  groups <- groups[keep_group, , drop = FALSE]
  if (nrow(groups) == 0) {
    stop("No EVI rasters overlap the requested fixture cell.", call. = FALSE)
  }

  dests <- file.path(
    evi_dir,
    evi_annual_composite_filename(groups$year, groups$tile)
  )
  needs_build <- overwrite | !file.exists(dests)
  if (!quiet) {
    message(sprintf(
      "Annual EVI fixture composites: %s required, %s cached, %s to build.",
      length(dests),
      length(dests) - sum(needs_build),
      sum(needs_build)
    ))
  }
  build_index <- 0L
  for (i in which(needs_build)) {
    build_index <- build_index + 1L
    group_items <- items[
      items$year == groups$year[[i]] & items$tile == groups$tile[[i]],
      ,
      drop = FALSE
    ]
    if (!quiet) {
      message(sprintf(
        "Building annual EVI fixture composite %s of %s: %s %s",
        build_index,
        sum(needs_build),
        group_items$year[[1]],
        group_items$tile[[1]]
      ))
    }
    source_index <- 0L
    layers <- lapply(seq_len(nrow(group_items)), \(j) {
      source_index <<- source_index + 1L
      if (!quiet) {
        evi_write_download_progress(
          sprintf(
            "EVI fixture sources for %s %s",
            group_items$year[[1]],
            group_items$tile[[1]]
          ),
          source_index,
          nrow(group_items)
        )
      }
      evi_quality_filtered_fixture_raster(
        href = group_items$href[[j]],
        quality_href = group_items$quality_href[[j]],
        cell = cell
      )
    })
    if (!quiet) {
      cat("\n", file = stderr())
    }
    evi_write_annual_layers(layers, dests[[i]])
  }
  invisible(dests)
}

evi_fetch_url <- function(url) {
  if (requireNamespace("curl", quietly = TRUE)) {
    handle <- curl::new_handle(followlocation = TRUE)
    res <- curl::curl_fetch_memory(url, handle = handle)
    return(rawToChar(res$content))
  }
  paste(readLines(url, warn = FALSE), collapse = "\n")
}

evi_parse_stac_items <- function(json) {
  features <- evi_json_features(json)
  if (length(features) == 0) {
    return(data.frame(
      id = character(0),
      platform = character(0),
      xmin = numeric(0),
      ymin = numeric(0),
      xmax = numeric(0),
      ymax = numeric(0),
      start_date = as.Date(character(0)),
      end_date = as.Date(character(0)),
      tile = character(0),
      href = character(0),
      quality_href = character(0)
    ))
  }
  out <- lapply(
    features,
    function(feature) {
      feature_platform <- evi_json_match(
        '"platform"\\s*:\\s*"([^"]+)"',
        feature
      )
      href <- evi_json_match(
        '"250m_16_days_EVI"\\s*:\\s*\\{[^{}]*"href"\\s*:\\s*"([^"]+)"',
        feature
      )
      quality_href <- evi_json_match(
        '"250m_16_days_pixel_reliability"\\s*:\\s*\\{[^{}]*"href"\\s*:\\s*"([^"]+)"',
        feature
      )
      if (is.na(href) || is.na(quality_href)) {
        return(NULL)
      }
      bbox <- evi_parse_stac_bbox(feature)
      data.frame(
        id = evi_json_match('"id"\\s*:\\s*"([^"]+)"', feature),
        platform = feature_platform,
        xmin = bbox[[1]],
        ymin = bbox[[2]],
        xmax = bbox[[3]],
        ymax = bbox[[4]],
        start_date = as.Date(substr(
          evi_json_match('"start_datetime"\\s*:\\s*"([^"]+)"', feature),
          1,
          10
        )),
        end_date = as.Date(substr(
          evi_json_match('"end_datetime"\\s*:\\s*"([^"]+)"', feature),
          1,
          10
        )),
        tile = evi_item_tile(
          id = evi_json_match('"id"\\s*:\\s*"([^"]+)"', feature),
          href = href
        ),
        href = evi_json_unescape(href),
        quality_href = evi_json_unescape(quality_href)
      )
    }
  )
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    return(data.frame(
      id = character(0),
      platform = character(0),
      xmin = numeric(0),
      ymin = numeric(0),
      xmax = numeric(0),
      ymax = numeric(0),
      start_date = as.Date(character(0)),
      end_date = as.Date(character(0)),
      tile = character(0),
      href = character(0),
      quality_href = character(0)
    ))
  }
  do.call(rbind, out)
}

evi_parse_stac_bbox <- function(feature) {
  bbox <- evi_json_match('"bbox"\\s*:\\s*\\[([^\\]]+)\\]', feature)
  if (is.na(bbox)) {
    return(rep(NA_real_, 4))
  }
  bbox <- as.numeric(strsplit(bbox, "\\s*,\\s*")[[1]])
  if (length(bbox) != 4 || anyNA(bbox)) {
    return(rep(NA_real_, 4))
  }
  bbox
}

evi_parse_next_href <- function(json) {
  href <- evi_json_match(
    '"rel"\\s*:\\s*"next"[^}]*"href"\\s*:\\s*"([^"]+)"',
    json
  )
  if (is.na(href)) {
    return(NA_character_)
  }
  evi_json_unescape(href)
}

evi_json_features <- function(json) {
  match <- regexpr('"features"\\s*:\\s*\\[', json, perl = TRUE)
  if (match[[1]] == -1) {
    return(character(0))
  }
  pos <- match[[1]] + attr(match, "match.length")
  depth <- 0L
  in_string <- FALSE
  escaped <- FALSE
  feature_start <- NA_integer_
  features <- character()
  json_length <- nchar(json)
  while (pos <= json_length) {
    char <- substr(json, pos, pos)
    if (in_string) {
      if (escaped) {
        escaped <- FALSE
      } else if (char == "\\") {
        escaped <- TRUE
      } else if (char == '"') {
        in_string <- FALSE
      }
    } else if (char == '"') {
      in_string <- TRUE
    } else if (char == "{") {
      if (depth == 0L) {
        feature_start <- pos
      }
      depth <- depth + 1L
    } else if (char == "}") {
      depth <- depth - 1L
      if (depth == 0L) {
        features <- c(features, substr(json, feature_start, pos))
      }
    } else if (char == "]" && depth == 0L) {
      break
    }
    pos <- pos + 1L
  }
  features
}

evi_json_match <- function(pattern, text) {
  match <- regexpr(pattern, text, perl = TRUE)
  if (match[[1]] == -1) {
    return(NA_character_)
  }
  sub(pattern, "\\1", regmatches(text, match), perl = TRUE)
}

evi_json_unescape <- function(x) {
  x <- gsub("\\\\/", "/", x)
  gsub("\\\\u0026", "&", x, ignore.case = TRUE)
}
