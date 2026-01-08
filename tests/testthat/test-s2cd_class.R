test_that("s2cd behaves as expected", {
  d <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )

  d |>
    expect_s3_class(c("s2cd", "s2_cell", "S7_object"))

  d |>
    is_s2cd() |>
    expect_true()

  s2cd_example() |>
    is_s2cd() |>
    expect_true()

  letters |>
    is_s2cd() |>
    expect_false()

  s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")) |>
    is_s2cd() |>
    expect_false()

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  ) |>
    expect_error("all s2 cells must be level 30")

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      TRUE,
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  ) |>
    expect_error("all elements in the dates list must be Date vectors")

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2022-09-13"), as.Date(NA), as.Date("2023-09-20"))
    )
  ) |>
    expect_error("dates must not contain missing values")

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2023-09-20"))
    )
  ) |>
    expect_error("each Date vector must be in chronological order")

  d |>
    tibble::as_tibble() |>
    expect_s3_class(c("tbl_df", "tbl", "data.frame")) |>
    names() |>
    expect_equal(c("s2_cell", "dates"))

  d |>
    as.data.frame() |>
    expect_s3_class("data.frame") |>
    names() |>
    expect_equal(c("s2_cell", "dates"))

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

  # as_s2cd
  my_d <-
    data.frame(
      s2_cell = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
      dates = I(list(
        as.Date("2026-01-01"),
        c(as.Date("2024-09-13"), as.Date("2024-09-20"))
      ))
    )

  expect_identical(d, as_s2cd(d))

  as_s2cd(my_d) |>
    expect_s3_class("s2cd")

  my_d |>
    tibble::as_tibble() |>
    as_s2cd() |>
    expect_s3_class("s2cd")
})
