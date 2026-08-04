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
| 🗺️ Census block group linkage | `s2_join_tiger_bg()` |

### Geomarker Data

Most data required for geomarker assessment does not come with the package, but is installed on first use by downloading (and pre-processing) source data to the geomarker package's user data directory (see [`tools::R_user_dir()`](https://search.r-project.org/R/refmans/tools/html/userdir.html)).
This means it is available to use again across other R sessions and projects by the same user and will not need to be downloaded more than once.

`geomarker_download_file()` is the common download and cache layer that powers most of the package's data assets: it gives each source URL a stable local filename (using an ETag when available), stores the file in `geomarker_data_dir()`, and reuses the cached copy in later assessments.
The same cache contract makes it possible to prepare data in advance for a specific level-6 S2 cell and date range.
The source-specific `install_*_geomarker_fixture()` helpers download and spatially subset the required assets into an `inst/gmrkr--<cell>/R/geomarker` directory; see `inst/install_cincy_geomarker_fixture.R` for a complete example.
A fixture for the Cincinnati-area cell `8841` is included with geomarker.
To use it without downloading from the original sources, set `R_USER_DATA_DIR` to `system.file("gmrkr--8841", package = "geomarker")` and set `R_GEOMARKER_NO_DOWNLOAD=true`.
This pattern can also be used to package data for another study region first and then run geomarker assessments from those prepared assets when source downloads are unavailable, restricted, slow, or otherwise not preferable.
