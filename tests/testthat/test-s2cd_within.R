test_that("can check s2cd vector for a date interval", {
  d <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )
  s2cd_within_dates(
    d,
    date_range = c(as.Date("2020-01-01"), as.Date("2026-12-31"))
  ) |>
    expect_true()
  s2cd_within_dates(
    d,
    date_range = c(as.Date("2026-01-01"), as.Date("2026-12-31"))
  ) |>
    expect_false()
  s2cd_within_dates(
    d,
    date_range = c(as.Date("2020-01-01"), as.Date("2025-12-31"))
  ) |>
    expect_false()
  s2cd_within_dates(
    d,
    date_range = c(as.Date("2024-01-01"), as.Date("2025-12-31"))
  ) |>
    expect_false()
  s2cd_within_dates(
    d,
    date_range = c(as.Date("2026-01-01"), as.Date("2025-12-31"))
  ) |>
    expect_error("date_range must be chronologically sorted")
})

test_that("can check s2cd vector for s2 cell union", {
  d <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )

  cincy_s2_rough_cover <- s2::s2_cell(c(
    "88404b",
    "88404d",
    "884052c",
    "8841ad",
    "8841af",
    "8841b4",
    "8841c9",
    "8841cb",
    "8841cc4",
    "8841ceb"
  ))

  s2cd_within_s2_cells(d, cincy_s2_rough_cover) |>
    expect_false()

  s2cd_within_s2_cells(d[1], cincy_s2_rough_cover) |>
    expect_true()

  s2cd_within_s2_cells(d[2], cincy_s2_rough_cover) |>
    expect_false()
})

test_that("can check s2cd for both dates and s2_cell", {
  d <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )
  cincy_s2_rough_cover <- s2::s2_cell(c(
    "88404b",
    "88404d",
    "884052c",
    "8841ad",
    "8841af",
    "8841b4",
    "8841c9",
    "8841cb",
    "8841cc4",
    "8841ceb"
  ))

  s2cd_within(
    d,
    date_range = c(as.Date("2020-01-01"), as.Date("2026-12-31")),
    s2_cell = cincy_s2_rough_cover
  ) |>
    expect_false()

  s2cd_within(
    d[1],
    date_range = c(as.Date("2020-01-01"), as.Date("2026-12-31")),
    s2_cell = cincy_s2_rough_cover
  ) |>
    expect_true()

  s2cd_within(
    d[1],
    date_range = c(as.Date("2020-01-01"), as.Date("2025-12-31")),
    s2_cell = cincy_s2_rough_cover
  ) |>
    expect_false()
})
