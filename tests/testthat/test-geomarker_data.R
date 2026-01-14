test_that("geomarker_data_dir works", {
  geomarker_data_dir() |>
    expect_type("character") |>
    expect_length(1) |>
    grepl("geomarker", x = _) |>
    expect_true()
  geomarker_data_dir("foofy") |>
    expect_type("character") |>
    expect_length(1) |>
    grepl("foofy", x = _) |>
    expect_true()
})

test_that("geomarker_download_file works", {
  the_url <- "https://www2.census.gov/geo/tiger/TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  geomarker_download_file(the_url) |>
    expect_visible() |>
    expect_identical(
      file.path(
        geomarker_data_dir(),
        "d274c988--tl_2024_us_internationalboundary.zip"
      )
    )
  geomarker_download_file(the_url, quiet = TRUE) |>
    expect_invisible()
})
