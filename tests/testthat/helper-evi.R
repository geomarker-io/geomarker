evi_test_items <- function(year = "2024", tile = "h11v05") {
  dates <- as.Date(paste0(year, "-01-01")) + c(16 * (0:22), 8 + 16 * (0:22))
  product <- rep(c("MOD13Q1", "MYD13Q1"), each = 23)
  ids <- paste0(product, ".A", format(dates, "%Y%j"), ".", tile, ".061.2025001000000")
  data.frame(
    id = ids, year = year, tile = tile,
    platform = rep(c("terra", "aqua"), each = 23),
    start_date = dates, end_date = dates + 15,
    xmin = 0, ymin = 0, xmax = 1, ymax = 1,
    href = paste0("https://example.com/", ids, "/evi.tif"),
    quality_href = paste0("https://example.com/", ids, "/quality.tif")
  )
}
