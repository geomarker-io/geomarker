# geomarker

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/geomarker)](https://CRAN.R-project.org/package=geomarker)
[![R-CMD-check](https://github.com/geomarker-io/geomarker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/geomarker/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of geomarker is to facilitate geomarker assessment, or the linkage of environmental exposures and community characteristics with people according to their locations and dates, in R.

## Installation

You can install the development version of geomarker from GitHub:

```r
devtools::install_github("geomarker-io/geomarker")
```

## Using

### s2_cell_dates (`s2cd`)

The `s2cd` vector class extends `s2::s2_cell` to include a list of Date vectors, one for each s2 cell.
This vector is meant to store locations and dates of observations and is used as the input to all geomarker assessment functions.

[S2](https://s2geometry.io/devguide/) is a spherical hierarchical cell index: specifically, a quad-based discrete global grid system (DGGS) on the sphere, while the S2 cell ID system is the 64-bit naming scheme that identifies each cell and its resolution within that grid. BigQuery exposes this system through its [S2 geography functions](https://cloud.google.com/bigquery/docs/reference/standard-sql/geography_functions#s2_functions).
For geomarker, a full-resolution (level 30) `s2_cell` can be treated as a point with an approximate spatial resolution of 1 centimeter, although coarser cells can represent regions—for example, when installing a fixture for a level-6 S2 cell.
Traditional longitude and latitude coordinates can be converted to full-resolution S2 cells with `s2::as_s2_cell(s2::s2_lnglat(lon, lat))`.
Use the [S2 Cell Viewer](https://vesoyu.github.io/s2cell) to inspect individual cells and the [S2 Region Coverer](https://igorgatis.github.io/ws2/) to explore coarser cells and regional coverings.

### Available geomarkers

| geomarker | geomarker function |
|---|---|
| 🌦️ weather and atmospheric conditions | `get_gridmet_data()`, `get_narr_data()` |
| 🔥 wildfire smoke | `get_daily_smoke_data()`, `get_smoke_summary()` |
| 🌿 satellite-based vegetation and greenness | `get_evi_data()` |
| ⛰️ elevation | `get_elevation_summary()` |
| 🏙️ land cover and imperviousness | `get_nlcd_fct_imp_data()` |
| 🚦 traffic | `get_traffic_summary()` |
| 🏭 point-source emissions | `get_nei_point_summary()` |
| 🌫️ MERRA-2 surface PM2.5 | `get_merra_data()` |
| 🗺️ Census block group linkage | `get_tiger_bg()` |

### Geomarker Data

Most data required for geomarker assessment does not come with the package, but is installed on first use by downloading (and pre-processing) source data to the geomarker package's user data directory (see [`tools::R_user_dir()`](https://search.r-project.org/R/refmans/tools/html/userdir.html)).
This means it is available to use again across other R sessions and projects by the same user and will not need to be downloaded more than once.

The [`stow`](https://github.com/cole-brokamp/stow) package provides the common download and cache layer for ordinary source files.
Every cached file is stored with `package = "geomarker"` in a subdirectory named for the function that consumes it, such as `get_elevation_summary` or `get_tiger_bg`.
EVI and authenticated MERRA source builds use the same function-named directory layout while retaining their specialized signed or query-based download transport.
Use `stow::stow_path(package = "geomarker")` to locate the cache and `stow::stow_info(package = "geomarker")` to inspect its contents.
This layout is a clean cutover: files in the previous flat, `hms`, `evi`, or `merra` locations are not moved or searched automatically.
The same cache contract makes it possible to prepare data in advance for a specific level-6 S2 cell and date range.
The source-specific `install_*_geomarker_fixture()` helpers download and spatially subset the required assets into an `inst/gmrkr--<cell>/R/geomarker` directory; see `inst/install_cincy_geomarker_fixture.R` for a complete example.
For assessments that summarize within a buffer, the corresponding fixture installer accepts a `buffer` in meters and includes that halo around the level-6 fixture cell.
Set it to the largest buffer that the prepared fixture must support; each installer defaults to the assessment function's default buffer.
A fixture for the Cincinnati-area cell `8841` is included with geomarker.
This pattern can also be used to package data for another study region first and then run geomarker assessments from those prepared assets when source downloads are unavailable, restricted, slow, or otherwise not preferable.

### Cache-only operation

To use a prepared fixture without downloading from the original sources, set `R_USER_DATA_DIR` to the fixture directory and set `R_GEOMARKER_NO_DOWNLOAD=true`.
For example, select the fixture included with geomarker for the current R session with:

```r
Sys.setenv(
  R_USER_DATA_DIR = system.file("gmrkr--8841", package = "geomarker"),
  R_GEOMARKER_NO_DOWNLOAD = "true"
)
```

When `R_GEOMARKER_NO_DOWNLOAD` is set to any nonempty value, including `false`, geomarker assessment functions use only files already available in the selected cache; a required file that is not cached produces an error rather than an automatic download.
Unset cache-only operation with `Sys.unsetenv("R_GEOMARKER_NO_DOWNLOAD")`.

`R_GEOMARKER_NO_DOWNLOAD` is checked by geomarker's cache layer and by its known direct network paths, including Planetary Computer requests and remote raster reads, NASA Earthdata source builds, and release-preparation downloads.
It is a best-effort application control, not a network security boundary; use operating-system or container network controls when network isolation must be guaranteed.

### MERRA-2 data cadence

MERRA-2 PM2.5 release data are cataloged in `inst/merra-data.dcf` as complete half-year assets: January through June (`H1`) and July through December (`H2`).
Assets are added to the package's provisional `v0.0.1` GitHub release only after every expected daily granule has been discovered and validated.
There is one fixed asset name for each year and half-year. It may be recreated from the latest NASA data while preparing a package release, but the checked-in DCF records exactly one published file and checksum. A later data correction is distributed with a new package and GitHub release.

`get_merra_data()` first uses a complete monthly artifact built under the `get_merra_data` directory in the user's geomarker cache and then tries the matching official half-year release.
When neither is present, it returns aligned missing values with one warning rather than substituting an older period.

A user with an [Earthdata Login](https://urs.earthdata.nasa.gov/) can build any fully elapsed and fully available month directly from NASA. Set the account username and password:

```r
Sys.setenv(EARTHDATA_USER = "...", EARTHDATA_PASSWORD = "...")
install_merra_data("2026-07", source = "earthdata")
```

The source build discovers the exact daily `M2T1NXAER` v5.12.4 granules through NASA CMR, downloads authenticated variable-and-CONUS subsets, caches daily work for resumption, and writes an adjacent DCF with exact granule revisions and subset hashes.
Set `overwrite = TRUE` to recreate a month from the latest CMR listing; daily caches whose source granule revision and hash still match are reused.
This is a manual, potentially large build intended for an HPC or similarly provisioned environment; ordinary package checks do not contact NASA.

The bundled Cincinnati MERRA fixtures use deterministic synthetic concentrations solely to exercise the offline API and both half-year branches. They are not scientific data and must not be used for analysis.
