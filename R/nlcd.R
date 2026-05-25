#' Summarize nearby imperviousness
#'
#' Summarizes fraction imperviousness (fractional surface area
#' covered with artificial substrate or structures)
#' within the buffer distance of each s2 cell;
#' the year of each date is used to link to the corresponding
#' annual snapshot.
#'
#' Annual NLCD data (v1) is downloaded from a copy of v1 of the Annual National
#' Landcover Database (Annual NLCD) hosted on Harvard Dataverse in GeoTiff format
#' at a 30 m grid.
#' See <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KXETFC&version=1.0>
#' for more details on the hosted data and see
#' <https://www.mrlc.gov/data/project/annual-nlcd> for more details
#' on the NLCD.
#' @param x a s2_cell_dates object (see `?s2cd`)
#' @param fun function to summarize extracted data
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param ... passed to `geomarker_download_file()`
#' @return a list of numeric vectors of NLCD fraction imperviousness summaries
#' @export
#' @examples
#' get_nlcd_fct_imp_data(s2cd_example_cincy(2L))
get_nlcd_fct_imp_data <- function(
  x,
  fun = mean,
  buffer = 800,
  ...
) {
  stopifnot(
    "x must be a s2_cell_dates object" = is_s2cd(x),
    "dates must be between 2017 and 2024" = s2cd_within(
      x,
      date_range = c(as.Date("2017-01-01"), as.Date("2024-12-31"))
    )
  )
  if ("2024" %in% s2cd_years(x)) {
    warning("using 2023 NLCD product for dates in 2024")
  }

  fid <- c(
    "2024" = "10980930",
    "2023" = "10980930",
    "2022" = "10980932",
    "2021" = "10980929",
    "2020" = "10980933",
    "2019" = "10980934",
    "2018" = "10980931",
    "2017" = "10980935"
  )

  nlcd_urls <-
    paste0(
      "https://dataverse.harvard.edu/api/access/datafile/",
      fid[s2cd_years(x)]
    )

  # if (cloud) {
  #   nlcd_raster <-
  #     lapply(nlcd_urls, terra::rast, vsi = TRUE) |>
  #     Reduce(c, x = _)
  # } else {
  nlcd_files <- vapply(
    nlcd_urls,
    geomarker_download_file,
    FUN.VALUE = character(1),
    ...
  )
  nlcd_raster <-
    lapply(nlcd_files, terra::rast) |>
    Reduce(c, x = _)
  # }
  names(nlcd_raster) <- s2cd_years(x)

  x_vect <-
    s2_cell_to_vect(x) |>
    terra::project(nlcd_raster) |>
    terra::buffer(width = buffer)

  xtract <- terra::extract(nlcd_raster, x_vect, fun = fun, ID = FALSE)

  out <- mapply(
    \(yrs, xts) as.numeric(xts[yrs]),
    yrs = lapply(s2cd_dates(x), format, "%Y"),
    xts = split(xtract, seq_len(nrow(xtract))),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

  return(out)
}
