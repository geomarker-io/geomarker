#' Summarize nearby National Emissions Inventory point emissions
#'
#' Summarizes annual facility-level point emissions within `buffer` meters of
#' each s2 cell as tons per year. Each date is mapped by
#' calendar year to the nearest scheduled triennial National Emissions
#' Inventory (NEI) cycle
#' (2016-2018 to 2017 NEI, 2019-2021 to 2020 NEI, 2022-2024 to 2023 NEI).
#' Dates mapping to an unavailable or
#' unsupported cycle, including 2025 through 2027 mapping to the not-yet-
#' available 2026 NEI, return `NA_real_`.
#'
#' Facility-summary ZIP files for each requested pollutant are downloaded
#' from the Environmental Protection Agency and read directly on each call.
#' Rows are retained only when the pollutant code, latitude, longitude, and
#' total emissions are complete. Latitude, longitude, and total emissions must
#' also be finite, with latitude between -90 and 90 degrees and longitude
#' between -180 and 180 degrees.
#'
#' `fun` receives the numeric vector of annual emissions from facilities within
#' the buffer and must return one numeric value. With the default `sum`, a
#' location with no nearby facilities returns zero.
#'
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param pollutant_code one of `PM25-PRI`, `EC`, `OC`, `SO4`, `NO3`, or
#' `PMFINE`
#' @param fun function used to summarize nearby annual emissions
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param ... passed to `geomarker_download_file()`
#' @return a list of numeric vectors of point-source emissions summaries, one
#' vector per input location and one value per associated date. Values passed
#' to `fun` are in tons per year. Dates mapping to unavailable inventory
#' cycles return `NA_real_`.
#' @references
#' <https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei>
#' @references
#' <https://www.epa.gov/air-emissions-inventories/what-are-units-nei-emissions-data>
#' @export
#' @examples
#' withr::local_envvar(
#'   R_USER_DATA_DIR = fs::path_package(
#'     "geomarker",
#'     "gmrkr--8841"
#'   ),
#'   R_GEOMARKER_NO_DOWNLOAD = "true"
#' )
#' get_nei_point_summary(s2cd_example())
#' get_nei_point_summary(s2cd_example(), "PMFINE", buffer = 1600)
#' get_nei_point_summary(s2cd_example_cincy(5L), "PMFINE", buffer = 10000)
get_nei_point_summary <- function(
  x,
  pollutant_code = c("PM25-PRI", "EC", "OC", "SO4", "NO3", "PMFINE"),
  fun = sum,
  buffer = 1000,
  ...
) {
  stopifnot(
    "x must be a s2_cell_dates vector" = is_s2cd(x),
    "fun must be a function" = is.function(fun),
    "buffer must be numeric" = is.numeric(buffer),
    "buffer must be length one" = length(buffer) == 1,
    "buffer must be finite" = is.finite(buffer),
    "buffer must not be negative" = buffer >= 0
  )
  pollutant_code <- match.arg(
    pollutant_code,
    c("PM25-PRI", "EC", "OC", "SO4", "NO3", "PMFINE")
  )

  source_urls <- c(
    `2017` = paste0(
      "https://gaftp.epa.gov/Air/nei/nei_facility_summaries/",
      "2017_NEI_Facility_summary.zip"
    ),
    `2020` = paste0(
      "https://gaftp.epa.gov/Air/nei/nei_facility_summaries/",
      "2020_NEI_Facility_summary.zip"
    ),
    `2023` = paste0(
      "https://gaftp.epa.gov/Air/nei/2023/data_summaries/",
      "eis_report_38234_2023NEI_facility_summary_21jul2026.zip"
    )
  )

  mapped_years <- lapply(s2cd_dates(x), \(dates) {
    years <- as.integer(format(dates, "%Y"))
    as.character(2017L + 3L * round((years - 2017L) / 3))
  })
  out <- lapply(s2cd_dates(x), \(dates) rep(NA_real_, length(dates)))
  requested_years <- intersect(
    unique(unlist(mapped_years, use.names = FALSE)),
    names(source_urls)
  )
  if (length(requested_years) == 0) {
    return(out)
  }

  check_installed("readr", "to read NEI facility-summary CSV files")
  quiet <- isTRUE(list(...)$quiet)
  cells <- s2::as_s2_cell(x)
  cell_keys <- as.character(cells)

  for (year in requested_years) {
    location_index <- which(vapply(
      mapped_years,
      \(years) year %in% years,
      logical(1)
    ))
    unique_keys <- unique(cell_keys[location_index])
    unique_cells <- cells[match(unique_keys, cell_keys)]
    fixture_file <- file.path(
      geomarker_data_dir(),
      paste0("nei-", year, "-facility-summary.zip")
    )
    source_file <- if (
      nzchar(Sys.getenv("R_GEOMARKER_NO_DOWNLOAD")) &&
        file.exists(fixture_file)
    ) {
      fixture_file
    } else {
      geomarker_download_file(source_urls[[year]], ...)
    }
    members <- utils::unzip(source_file, list = TRUE)$Name
    csv_members <- members[grepl("[.]csv$", members, ignore.case = TRUE)]
    if (length(csv_members) != 1) {
      stop(
        "The NEI facility-summary ZIP must contain exactly one CSV file; ",
        "found ",
        length(csv_members),
        ".",
        call. = FALSE
      )
    }

    required_columns <- c(
      "site latitude",
      "site longitude",
      "pollutant code",
      "total emissions"
    )
    header_connection <- unz(source_file, csv_members[[1]], open = "rb")
    header <- tryCatch(
      suppressMessages(
        readr::read_csv(
          header_connection,
          n_max = 0,
          progress = FALSE,
          name_repair = "minimal"
        )
      ),
      finally = close(header_connection)
    )
    missing_columns <- setdiff(required_columns, names(header))
    if (length(missing_columns) > 0) {
      stop(
        "The NEI facility-summary CSV is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    connection <- unz(source_file, csv_members[[1]], open = "rb")
    point_data <- tryCatch(
      readr::read_csv(
        connection,
        col_types = readr::cols_only(
          `site latitude` = readr::col_double(),
          `site longitude` = readr::col_double(),
          `pollutant code` = readr::col_character(),
          `total emissions` = readr::col_double()
        ),
        progress = !quiet,
        name_repair = "minimal"
      ),
      finally = close(connection)
    )
    point_data <- as.data.frame(point_data, stringsAsFactors = FALSE)
    point_data <- point_data[
      point_data[["pollutant code"]] %in% pollutant_code,
      required_columns,
      drop = FALSE
    ]
    valid <- stats::complete.cases(point_data) &
      is.finite(point_data[["site latitude"]]) &
      is.finite(point_data[["site longitude"]]) &
      is.finite(point_data[["total emissions"]]) &
      point_data[["site latitude"]] >= -90 &
      point_data[["site latitude"]] <= 90 &
      point_data[["site longitude"]] >= -180 &
      point_data[["site longitude"]] <= 180
    point_data <- point_data[valid, , drop = FALSE]
    names(point_data) <- c(
      "latitude",
      "longitude",
      "pollutant_code",
      "emissions_tpy"
    )

    point_geographies <- s2::s2_geog_point(
      point_data$longitude,
      point_data$latitude
    )
    nearby <- s2::s2_dwithin_matrix(
      s2::s2_cell_to_lnglat(unique_cells),
      point_geographies,
      distance = buffer
    )
    summaries <- vapply(
      nearby,
      \(indices) {
        summary <- fun(point_data$emissions_tpy[indices])
        if (!is.numeric(summary) || length(summary) != 1) {
          stop("fun must return one numeric value", call. = FALSE)
        }
        as.numeric(summary)
      },
      numeric(1)
    )
    names(summaries) <- unique_keys

    for (index in location_index) {
      out[[index]][mapped_years[[index]] == year] <-
        summaries[[cell_keys[[index]]]]
    }
  }

  out
}

install_nei_point_geomarker_fixture <- function(
  cell,
  dates,
  output_dir,
  source_files = NULL
) {
  check_installed("readr", "to create NEI point-source fixture data")
  source_urls <- c(
    `2017` = paste0(
      "https://gaftp.epa.gov/Air/nei/nei_facility_summaries/",
      "2017_NEI_Facility_summary.zip"
    ),
    `2020` = paste0(
      "https://gaftp.epa.gov/Air/nei/nei_facility_summaries/",
      "2020_NEI_Facility_summary.zip"
    ),
    `2023` = paste0(
      "https://gaftp.epa.gov/Air/nei/2023/data_summaries/",
      "eis_report_38234_2023NEI_facility_summary_21jul2026.zip"
    )
  )
  cell <- geomarker_fixture_cell(cell)
  date_years <- as.integer(format(geomarker_fixture_dates(dates), "%Y"))
  years <- intersect(
    unique(as.character(2017L + 3L * round((date_years - 2017L) / 3))),
    names(source_urls)
  )
  output_dir <- geomarker_fixture_output_dir(output_dir)
  if (!is.null(source_files)) {
    stopifnot(
      "source_files must be a named character vector" = is.character(
        source_files
      ) &&
        !is.null(names(source_files)),
      "source_files must contain all requested NEI years" = all(
        years %in% names(source_files)
      ),
      "source_files must exist" = all(file.exists(source_files[years]))
    )
  }

  lapply(years, \(year) {
    source_file <- if (is.null(source_files)) {
      geomarker_download_file(source_urls[[year]], quiet = TRUE)
    } else {
      source_files[[year]]
    }
    members <- utils::unzip(source_file, list = TRUE)$Name
    csv_members <- members[grepl("[.]csv$", members, ignore.case = TRUE)]
    if (length(csv_members) != 1) {
      stop(
        "The NEI facility-summary ZIP must contain exactly one CSV file; ",
        "found ",
        length(csv_members),
        ".",
        call. = FALSE
      )
    }

    required_columns <- c(
      "site latitude",
      "site longitude",
      "pollutant code",
      "total emissions"
    )
    header_connection <- unz(source_file, csv_members[[1]], open = "rb")
    header <- tryCatch(
      suppressMessages(
        readr::read_csv(
          header_connection,
          n_max = 0,
          progress = FALSE,
          name_repair = "minimal"
        )
      ),
      finally = close(header_connection)
    )
    missing_columns <- setdiff(required_columns, names(header))
    if (length(missing_columns) > 0) {
      stop(
        "The NEI facility-summary CSV is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    connection <- unz(source_file, csv_members[[1]], open = "rb")
    point_data <- tryCatch(
      readr::read_csv(
        connection,
        col_types = readr::cols_only(
          `site latitude` = readr::col_double(),
          `site longitude` = readr::col_double(),
          `pollutant code` = readr::col_character(),
          `total emissions` = readr::col_double()
        ),
        progress = FALSE,
        name_repair = "minimal"
      ),
      finally = close(connection)
    )
    point_data <- as.data.frame(point_data, stringsAsFactors = FALSE)
    point_data <- point_data[, required_columns, drop = FALSE]
    valid <- stats::complete.cases(point_data) &
      is.finite(point_data[["site latitude"]]) &
      is.finite(point_data[["site longitude"]]) &
      is.finite(point_data[["total emissions"]]) &
      point_data[["site latitude"]] >= -90 &
      point_data[["site latitude"]] <= 90 &
      point_data[["site longitude"]] >= -180 &
      point_data[["site longitude"]] <= 180
    point_data <- point_data[valid, , drop = FALSE]
    names(point_data) <- c(
      "latitude",
      "longitude",
      "pollutant_code",
      "emissions_tpy"
    )

    point_geographies <- s2::s2_geog_point(
      point_data$longitude,
      point_data$latitude
    )
    within <- s2::s2_contains_matrix(
      s2::s2_cell_polygon(cell),
      point_geographies
    )[[1]]
    point_data <- point_data[within, , drop = FALSE]

    fixture_data <- data.frame(
      `site latitude` = point_data$latitude,
      `site longitude` = point_data$longitude,
      `pollutant code` = point_data$pollutant_code,
      `total emissions` = point_data$emissions_tpy,
      check.names = FALSE
    )
    write_dir <- tempfile("nei-fixture-")
    dir.create(write_dir)
    csv_file <- file.path(
      write_dir,
      paste0(year, "_NEI_Facility_summary.csv")
    )
    zip_file <- tempfile(fileext = ".zip")
    on.exit(
      unlink(c(write_dir, zip_file), recursive = TRUE, force = TRUE),
      add = TRUE
    )
    readr::write_csv(fixture_data, csv_file)
    utils::zip(zipfile = zip_file, files = csv_file, flags = "-j")
    file.copy(
      zip_file,
      file.path(
        output_dir,
        paste0("nei-", year, "-facility-summary.zip")
      ),
      overwrite = TRUE
    )
  })

  invisible(output_dir)
}
