# geomarker

<!-- badges: start -->

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![geomarker status badge](https://geomarker-io.r-universe.dev/geomarker/badges/version)](https://geomarker-io.r-universe.dev/geomarker)
[![CRAN status](https://www.r-pkg.org/badges/version/geomarker)](https://CRAN.R-project.org/package=geomarker)
[![R-CMD-check](https://github.com/geomarker-io/geomarker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/geomarker/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of geomarker is to facilitate geomarker assessment, or the linkage of environmental exposures and community characteristics with people according to their locations and dates, in R.

## Installation

You can install the latest release of geomarker from R-universe:

```r
install.packages(
  "geomarker",
  repos = c(
    "https://geomarker-io.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

You can install the development version of geomarker from GitHub with pak:

```r
pak::pak("geomarker-io/geomarker")
```

## Using

### s2_cell_dates (`s2cd`)

The `s2cd` vector class extends `s2::s2_cell` to include a list of Date vectors, one for each s2 cell.
This vector is meant to store locations and dates of observations and is used as the input to all geomarker assessment functions.

[S2](https://s2geometry.io/devguide/) is a spherical hierarchical cell index: specifically, a quad-based discrete global grid system (DGGS) on the sphere, while the S2 cell ID system is the 64-bit naming scheme that identifies each cell and its resolution within that grid. BigQuery exposes this system through its [S2 geography functions](https://docs.cloud.google.com/bigquery/docs/reference/standard-sql/geography_functions#s2_functions).
For geomarker, a full-resolution (level 30) `s2_cell` can be treated as a point with an approximate spatial resolution of 1 centimeter, although coarser cells can represent regions—for example, when installing a fixture for a level-6 S2 cell.
Traditional longitude and latitude coordinates can be converted to full-resolution S2 cells with `s2::as_s2_cell(s2::s2_lnglat(lon, lat))`.
Use the [S2 Cell Viewer](https://vesoyu.github.io/s2cell/) to inspect individual cells and the [S2 Region Coverer](https://igorgatis.github.io/ws2/) to explore coarser cells and regional coverings.

### Available geomarkers

| geomarker | geomarker function
|---|---|
| 🗺️ census block group linkage | `get_tiger_bg()`|
| ⛰️ elevation | `get_elevation_summary()` |
| 🏙️ land cover and imperviousness | `get_nlcd_fct_imp_summary()` |
| 🌫️ MERRA-2 surface PM2.5 | `get_merra_data()` |
| 🏭 point-source emissions | `get_nei_point_summary()` |
| 🌿 satellite-based enhanced vegetation index| `get_evi_summary()` |
| 🚦 traffic | `get_traffic_summary()` |
| 🌦️ weather and atmospheric conditions | `get_gridmet_data()`, `get_narr_data()` |
| 🔥 wildfire smoke | `get_smoke_summary()`  |

### Geomarker Data

Most data required for geomarker assessment does not come with the package. On first use, geomarker downloads and, when needed, preprocesses source data into durable managed local copies that remain available across R sessions and projects for the same user.

The [`stow`](https://github.com/cole-brokamp/stow) package manages ordinary source-file downloads. Geomarker supplies `package = "geomarker"`, so its durable managed local copies live beneath the fixed `stow` directory inside the geomarker package's user data directory (see [`tools::R_user_dir()`](https://search.r-project.org/R/refmans/tools/html/userdir.html)):

```r
file.path(tools::R_user_dir("geomarker", "data"), "stow")
```

Within that directory, each durable managed local copy is organized under the name of the function that consumes it, such as `get_elevation_summary` or `get_tiger_bg`. EVI and authenticated MERRA source builds use the same function-named organization while retaining their specialized signed or query-based download transport.
Use `stow::stow_path(package = "geomarker")` to locate the durable managed local copy directory and `stow::stow_info(package = "geomarker")` to inspect its contents.

### Geomarker fixtures

The same organization makes it possible to prepare data in advance for a specific level-6 S2 cell and date range.
The source-specific `install_*_geomarker_fixture()` helpers download and spatially subset the required assets into an `inst/gmrkr--<cell>/R/geomarker/stow` directory; see `inst/install_cincy_geomarker_fixture.R` for a complete example.
For assessments that summarize within a buffer, the corresponding fixture installer accepts a `buffer` in meters and includes that halo around the level-6 fixture cell.
Set it to the largest buffer that the prepared fixture must support; each installer defaults to the assessment function's default buffer.
A fixture for the Cincinnati-area cell `8841` is included with geomarker.
This pattern can also be used to package data for another study region first and then run geomarker assessments from those prepared assets when source downloads are unavailable, restricted, slow, or otherwise not preferable.

### Offline operation

To use a prepared fixture without downloading from the original sources, set `R_USER_DATA_DIR` to the fixture directory and set `R_GEOMARKER_NO_DOWNLOAD=true`.
For example, select the fixture included with geomarker for the current R session with:

```r
Sys.setenv(
  R_USER_DATA_DIR = system.file("gmrkr--8841", package = "geomarker"),
  R_GEOMARKER_NO_DOWNLOAD = "true"
)
```

When `R_GEOMARKER_NO_DOWNLOAD` is set to any nonempty value, including `false`, geomarker assessment functions use only durable managed local copies already available in the selected directory. When a required copy is unavailable, geomarker produces an error rather than downloading it.
Unset offline operation with `Sys.unsetenv("R_GEOMARKER_NO_DOWNLOAD")`.

`R_GEOMARKER_NO_DOWNLOAD` is honored when geomarker requests durable managed local copies through stow and by its known direct network paths, including Planetary Computer requests and remote raster reads, NASA Earthdata source builds, and release-preparation downloads.
It is a best-effort application control, not a network security boundary; use operating-system or container network controls when network isolation must be guaranteed.

### Versioned data releases

Elevation, HPMS traffic, and processed MERRA-2 data are hosted as separate GitHub
data releases in this repository. R package releases use `vMAJOR.MINOR.PATCH`;
data releases use a dataset vintage and a positive revision:

| Data | Release tag | Current coverage |
|---|---|---|
| Elevation | `data-elevation-2026-07-20-r1` | USGS 3DEP snapshot dated July 20, 2026 |
| Traffic | `data-hpms-2024-r1` | HPMS 2024 F1/F2 roadways |
| MERRA-2 | `data-merra2-YYYY-h1-r1` or `data-merra2-YYYY-h2-r1` | 2017 H1 through 2026 H1 |

Each vintage starts at `r1`. Changed data bytes require a new revision such as
`data-merra2-2024-h1-r2`; a new vintage starts again at `r1`. Unchanged data are
reused across package releases. Published data files and tags are immutable.
Each data release includes its data file and a one-record `manifest.dcf` with
the source provenance, coverage, file size, and SHA-256 hash.

The catalogs shipped with each package version pin exact data-release URLs.
Publishing a correction does not change data used by an already installed
package version. Files attached to the legacy `v0.0.1` release remain available
for older versions. The new URLs have different stow cache keys, so upgrading
to this layout requires one fresh download of each national data file used;
existing caches are not moved automatically.

For maintainers, run these commands from the checkout:

```sh
Rscript scripts/release-data.R preview
Rscript scripts/release-data.R stage --work-dir /private/tmp/geomarker-data --source-release v0.0.1
Rscript scripts/release-data.R publish --work-dir /private/tmp/geomarker-data --commit FULL_COMMIT_SHA
Rscript scripts/release-data.R verify --work-dir /private/tmp/geomarker-data --commit FULL_COMMIT_SHA
```

The source-release option is for copying the existing published files during
this migration. For a new revision, use the corresponding builder with
`--revision N`, review and update the checked-in catalog, and put the file at
`WORK_DIR/stage/DATA_TAG/ASSET_NAME` before running `stage` without
`--source-release`. The builders write candidate DCF metadata alongside their
output; they do not upload data or edit the package catalogs. Existing builder
positional arguments are unchanged; `--revision` defaults to `1`.

All four modes accept `--tag DATA_TAG` to select one catalog record. Preview
does not contact GitHub or create files. Stage verifies complete local files.
Publish requires a clean checkout at the supplied commit, that commit on
GitHub, and repository release immutability enabled. It resumes matching drafts
and refuses conflicting files or tags. Data releases are published with
`--latest=false`, preserving the latest R package release used by R-universe.
Verify downloads all assets afresh under `WORK_DIR/verify`; use a new work
directory for a repeated verification. Ordinary package tests remain offline.

The migration creates 21 independent releases using the existing files without
rebuilding them. Publish and verify every data release before merging the
package catalog cutover. Keep older releases available. If validation fails,
leave any published data releases in place and postpone the package cutover.

Run the maintainer-tool tests separately from the package tests:

```sh
Rscript -e 'testthat::test_file("scripts/tests/test-release-data.R", stop_on_failure = TRUE)'
```

### EVI annual composites

`get_evi_summary()` supports completed calendar years. A new tile-year composite
requires every expected Terra and Aqua acquisition, rather than treating a year
as complete merely because December has passed. Missing source periods produce
an error before an annual raster is written. For the instruments' first years,
the expected schedule begins when the product became available.

An adjacent DCF manifest records the selected source item IDs, dates, unsigned
asset URLs, processing version, spatial scope, and raster SHA-256. If the catalog
lists multiple processing revisions of one acquisition, a new build selects the
latest revision. Validated completed composites remain fixed until
`overwrite = TRUE`, which validates the latest source listing and downloads the
source files again. No provisional annual composites are built.

Older annual composites without valid manifests are rebuilt on online use.
Offline use gives an error explaining that they must first be rebuilt online;
it does not infer completeness from the old filename. Fixture installers write
the same provenance plus their S2 cell and supported buffer.

The bundled Cincinnati EVI fixture is deterministic synthetic data for offline
API examples only and must not be used for analysis. Its manifest explicitly
identifies it as synthetic; it is never treated as a validated scientific cache
when building from source. Recreate it with
`Rscript scripts/make-evi-example-fixture.R`.

### MERRA-2 data cadence

MERRA-2 PM2.5 release data are cataloged in `inst/merra-data.dcf` as complete half-year assets: January through June (`H1`) and July through December (`H2`).
The currently cataloged assets were originally published with the package's
`v0.0.1` release after every expected daily granule had been discovered and
validated. They are now pinned through the independent data releases described
above. Each year and half-year has one fixed asset name. Rebuilding it from
revised NASA data requires a new data revision; the checked-in DCF selects
exactly one published revision and checksum for each half-year.

`get_merra_data()` first uses a complete monthly artifact retained under the `get_merra_data` directory as a durable managed local copy and then tries the matching official half-year release.
When neither is present, it returns aligned missing values with one warning rather than substituting an older period.

A user with an [Earthdata Login](https://urs.earthdata.nasa.gov/) can build any fully elapsed and fully available month directly from NASA. Set the account username and password:

```r
Sys.setenv(EARTHDATA_USER = "...", EARTHDATA_PASSWORD = "...")
install_merra_data("2026-07", source = "earthdata")
```

The source build discovers the exact daily `M2T1NXAER` v5.12.4 granules through NASA CMR, downloads authenticated variable-and-CONUS subsets, stages daily source files for resumption, and writes an adjacent DCF with exact granule revisions and subset hashes.
Set `overwrite = TRUE` to recreate a month from the latest CMR listing; staged daily source files whose granule revision and hash still match are reused.
This is a manual, potentially large build intended for an HPC or similarly provisioned environment; ordinary package checks do not contact NASA.

The bundled Cincinnati MERRA fixtures use deterministic synthetic concentrations solely to exercise the offline API and both half-year branches. They are not scientific data and must not be used for analysis.
