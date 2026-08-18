#' geomarker: Geomarker assessment using S2 cells and dates
#'
#' The `s2cd` vector class stores S2 cells together with the dates associated
#' with each location. Geomarker assessment functions use this format to link
#' environmental and sociodemographic data to locations and dates.
#'
#' @section Cache-only operation:
#' Set `R_GEOMARKER_NO_DOWNLOAD=true` to prevent geomarker assessment functions
#' from automatically downloading missing source or release assets. Files
#' already available in the selected geomarker cache continue to be used; a
#' required file that is not cached produces an error rather than an automatic
#' download.
#'
#' To use a prepared fixture, set `R_USER_DATA_DIR` to the fixture directory
#' before running an assessment. For example, select the fixture included with
#' geomarker for the current R session with:
#'
#' ```r
#' Sys.setenv(
#'   R_USER_DATA_DIR = system.file("gmrkr--8841", package = "geomarker"),
#'   R_GEOMARKER_NO_DOWNLOAD = "true"
#' )
#' ```
#'
#' Any nonempty value enables cache-only operation, including `false`. Disable
#' it with `Sys.unsetenv("R_GEOMARKER_NO_DOWNLOAD")`.
#'
#' This setting is checked by geomarker's cache layer and by its known direct
#' network paths, including Planetary Computer requests and remote raster reads,
#' NASA Earthdata source builds, and release-preparation downloads. It is a
#' best-effort application control, not a network security boundary; use
#' operating-system or container network controls when network isolation must
#' be guaranteed.
#'
#' @keywords internal
"_PACKAGE"
