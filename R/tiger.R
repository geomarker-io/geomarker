tiger_block_group_url <- function(state, year) {
  sprintf(
    "https://www2.census.gov/geo/tiger/TIGER%s/BG/tl_%s_%s_bg.zip",
    year,
    year,
    state
  )
}

tiger_state_url <- function(year) {
  sprintf(
    "https://www2.census.gov/geo/tiger/TIGER%s/STATE/tl_%s_us_state.zip",
    year,
    year
  )
}

tiger_block_groups <- function(state, year, ...) {
  dest <- geomarker_download_file(
    tiger_block_group_url(state, year),
    ...
  )
  out <-
    sf::st_read(
      paste0("/vsizip/", dest),
      as_tibble = TRUE,
      quiet = TRUE,
      query = sprintf("SELECT GEOID FROM tl_%s_%s_bg", year, state)
    )
  out$s2_geography <- s2::as_s2_geography(out$geometry)
  out <- sf::st_drop_geometry(out)
  return(out)
}

tiger_states <- function(year, ...) {
  dest <- geomarker_download_file(
    tiger_state_url(year),
    ...
  )
  out <-
    sf::st_read(
      paste0("/vsizip/", dest),
      as_tibble = TRUE,
      quiet = TRUE,
      query = sprintf("SELECT GEOID FROM tl_%s_us_state", year)
    )
  out$s2_geography <- s2::as_s2_geography(out$geometry)
  out <- sf::st_drop_geometry(out)
  return(out)
}

#' Census Block Group Linkage
#'
#' Link an object coercible to an s2_cell vector with the closest census block
#' group using the US Census TIGER/Line shapefiles from a specific year.
#' @param x object coercible to an s2_cell vector; non-missing cells must be
#' full-resolution (level 30) cells
#' @param year vintage of TIGER/Line block group geography files
#' @param ... passed to `geomarker_download_file()`
#' @returns character vector of matched census block group identifiers
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' set.seed(123)
#' s2_join_tiger_bg(s2cd_example_cincy(8L))
s2_join_tiger_bg <- function(x, year = as.character(2024:2013), ...) {
  check_installed("sf", "read TIGER/Line census block group geographies")
  check_installed("s2", "s2 geometry calculations")
  x <- tryCatch(
    s2::as_s2_cell(x),
    error = function(err) {
      stop("x must be coercible to a s2_cell vector", call. = FALSE)
    }
  )
  non_missing_x <- stats::na.omit(x)
  if (length(non_missing_x) > 0L) {
    if (!all(s2::s2_cell_is_valid(non_missing_x))) {
      stop("x must contain valid s2 cells", call. = FALSE)
    }
    if (!all(s2::s2_cell_level(non_missing_x) == 30)) {
      stop("all non-missing s2 cells must be full-resolution level 30 cells",
        call. = FALSE
      )
    }
  }
  year <- match.arg(year)
  x_s2_geo <-
    unique(stats::na.omit(x)) |>
    s2::s2_cell_center()
  names(x_s2_geo) <- as.character(unique(stats::na.omit(x)))
  states <- tiger_states(year, ...)
  the_states <- states[
    s2::s2_closest_feature(x_s2_geo, states$s2_geography),
    "GEOID",
    drop = TRUE
  ]
  state_bgs <-
    lapply(unique(the_states), tiger_block_groups, year = year, ...) |>
    stats::setNames(unique(the_states))
  the_s2s <- split(x_s2_geo, the_states)

  bg_lookup <-
    lapply(names(the_s2s), \(stt) {
      sbg <- state_bgs[[stt]]
      sbg[
        s2::s2_closest_feature(the_s2s[[stt]], sbg$s2_geography),
        "GEOID",
        drop = TRUE
      ]
    }) |>
    unlist() |>
    stats::setNames(names(x_s2_geo))

  return(stats::setNames(bg_lookup[as.character(x)], NULL))
}

install_tiger_geomarker_fixture <- function(cell, dates, output_dir) {
  check_installed("sf", "to create TIGER fixture data.")
  cell <- geomarker_fixture_cell(cell)
  years <- geomarker_fixture_years(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  cell_geometry <- sf::st_as_sfc(s2::s2_cell_polygon(cell))

  lapply(years, \(year) {
    tgr_st_url <- tiger_state_url(year)
    tgr_st <- sf::st_read(paste0(
      "/vsizip/",
      geomarker_download_file(tgr_st_url, etag = FALSE)
    ), quiet = TRUE)
    tgr_st_cropped <- sf::st_crop(
      tgr_st,
      sf::st_transform(cell_geometry, sf::st_crs(tgr_st))
    )
    tgr_st_write_dir <- tempfile("tiger_state")
    dir.create(tgr_st_write_dir)
    on.exit(unlink(tgr_st_write_dir, recursive = TRUE, force = TRUE), add = TRUE)
    sf::st_write(
      tgr_st_cropped,
      file.path(tgr_st_write_dir, sprintf("tl_%s_us_state.shp", year)),
      quiet = TRUE
    )
    state_dest <- file.path(output_dir, url_to_filename(tgr_st_url, etag = FALSE))
    unlink(state_dest)
    utils::zip(
      state_dest,
      files = list.files(tgr_st_write_dir, full.names = TRUE),
      flags = "-j"
    )

    lapply(tgr_st_cropped$GEOID, \(st) {
      tgr_bg_url <- tiger_block_group_url(st, year)
      tgr_bg <- sf::st_read(paste0(
        "/vsizip/",
        geomarker_download_file(tgr_bg_url, etag = FALSE)
      ), quiet = TRUE)
      tgr_bg_cropped <- sf::st_crop(
        tgr_bg,
        sf::st_transform(cell_geometry, sf::st_crs(tgr_bg))
      )
      tgr_bg_write_dir <- tempfile(st)
      dir.create(tgr_bg_write_dir)
      on.exit(unlink(tgr_bg_write_dir, recursive = TRUE, force = TRUE), add = TRUE)
      sf::st_write(
        tgr_bg_cropped,
        file.path(tgr_bg_write_dir, sprintf("tl_%s_%s_bg.shp", year, st)),
        quiet = TRUE
      )
      bg_dest <- file.path(output_dir, url_to_filename(tgr_bg_url, etag = FALSE))
      unlink(bg_dest)
      utils::zip(
        bg_dest,
        files = list.files(tgr_bg_write_dir, full.names = TRUE),
        flags = "-j"
      )
      NULL
    }) |>
      invisible()
    NULL
  }) |>
    invisible()
  invisible(output_dir)
}
