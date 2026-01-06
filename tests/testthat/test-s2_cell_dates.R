test_that("s2_cell_dates behaves as expected", {
  d <- s2_cell_dates(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )

  d |>
    expect_s3_class(c("geomarker::s2_cell_dates", "s2_cell", "S7_object"))

  d |>
    tibble::as_tibble() |>
    expect_s3_class(c("tbl_df", "tbl", "data.frame"))

  d |>
    as.data.frame() |>
    expect_s3_class("data.frame")

  d |>
    as.numeric() |>
    expect_type("double")

  d |>
    as.character() |>
    expect_type("character")

  # use external method to extract s2
  d |>
    as_s2_cell() |>
    expect_s3_class("s2_cell")

  # convert to character to extract s2
  d |>
    as.character() |>
    s2::as_s2_cell() |>
    expect_s3_class("s2_cell")

  # ensure underlying data is double
  d |>
    S7::S7_data() |>
    expect_type("double")

  # get dates list
  expect_type(d@dates, "list")
  vapply(d@dates, class, character(1)) |>
    expect_identical(rep("Date", length(d)))
})
