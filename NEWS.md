# geomarker (development version)

- gridMET and NARR return aligned missing values for locations outside the
  raster extent instead of attempting to extract invalid raster cells.

- `s2cd_within()` includes both date-range endpoints as documented. gridMET
  and NARR now accept January 1, 1979; EVI retains its February 18, 2000 cutoff.

- EVI summaries include pixels from all MODIS tiles crossed by the requested
  buffer, using one mean across the combined annual rasters.

# geomarker 0.2.0

- `get_smoke_summary()` now supports `summary = "sum"` to add density scores
  across overlapping plumes and `summary = "none"` to return every intersecting
  plume density. Scores are 0 for None, 1 for Light, 2 for Medium, and 3 for
  Heavy. The default, `summary = "max"`, continues to return the maximum density.

- Elevation, traffic, and MERRA-2 assets now have independent, immutable data
  releases. Package versions pin exact data revisions. The files are unchanged,
  but their new URLs require fresh downloads instead of reusing legacy cache
  entries. Existing assets remain available for older package versions.

- NARR downloads now use NOAA PSL's anonymous FTP server. Existing downloads
  from the previous URL are not reused because cache filenames include the URL.

- Installation now uses `stow` (>= 0.3.0) from CRAN.

- Package documentation is available at <https://geomarker.io/geomarker/> and
  is deployed for published package releases.

# geomarker 0.1.1

- `get_tiger_bg()` now preserves input order across state-grouped lookups,
  including duplicate and missing cells.

# geomarker 0.1.0

- Initial release.
