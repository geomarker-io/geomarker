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

The `s2cd` class extends `s2::s2_cell` to include a list of Date vectors, one for each s2 cell.
This data structure is meant to store locations and dates of observations and is used as the input to all geomarker assessment functions.

### Geomarker Data

Data required for geomarker assessment does not come with the package, but is installed on first use by downloading (and pre-processing) source data to the R user's data directory for the geomarker package.
This means it available to use again across other R sessions and projects by the same user and will not need to be downloaded more than once.
