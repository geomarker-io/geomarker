test_that("can check s2cd object for a date interval", {
  d <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )
  s2cd_within(
    d,
    date_range = c(as.Date("2020-01-01"), as.Date("2026-12-31"))
  ) |>
    expect_true()
  s2cd_within(
    d,
    date_range = c(as.Date("2026-01-01"), as.Date("2026-12-31"))
  ) |>
    expect_false()
  s2cd_within(
    d,
    date_range = c(as.Date("2020-01-01"), as.Date("2025-12-31"))
  ) |>
    expect_false()
  s2cd_within(
    d,
    date_range = c(as.Date("2024-01-01"), as.Date("2025-12-31"))
  ) |>
    expect_false()
  s2cd_within(
    d,
    date_range = c(as.Date("2026-01-01"), as.Date("2025-12-31"))
  ) |>
    expect_error("date_range must be chronologically sorted")
})
