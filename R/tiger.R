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
