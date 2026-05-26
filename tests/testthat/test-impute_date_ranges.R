test_that("imputing date ranges works", {
  impute_date_ranges(as.Date(c("2024-01-01", "2024-03-17", "2024-09-21"))) |>
    expect_equal(
      list(
        start = as.Date(c("2024-01-01", "2024-02-08", "2024-06-19")),
        end = as.Date(c("2024-02-08", "2024-06-19", "2024-09-21"))
      )
    )

  impute_date_ranges(
    as.Date(c("2024-01-01", "2024-03-17", "2024-09-21")),
    start_early = 30L,
    end_late = 60L
  ) |>
    expect_equal(
      list(
        start = as.Date(c("2023-12-02", "2024-02-08", "2024-06-19")),
        end = as.Date(c("2024-02-08", "2024-06-19", "2024-11-20"))
      )
    )

  impute_date_ranges(as.Date("2024-06-02")) |>
    expect_equal(list(
      start = as.Date("2024-06-02"),
      end = as.Date("2024-06-02")
    ))

  impute_date_ranges(
    as.Date("2024-06-02"),
    start_early = 14L,
    end_late = 22L
  ) |>
    expect_equal(list(
      start = as.Date("2024-06-02") - 14,
      end = as.Date("2024-06-02") + 22
    ))
})

test_that("impute_date_ranges requires typed inputs", {
  expect_error(
    impute_date_ranges("2024-01-01"),
    "`x` must be a Date vector"
  )
  expect_error(
    impute_date_ranges(as.Date(NA)),
    "`x` must not contain missing values"
  )
  expect_error(
    impute_date_ranges(as.Date(character())),
    "`x` must contain at least one date"
  )
  expect_error(
    impute_date_ranges(as.Date("2024-01-01"), start_early = 1),
    "`start_early` must be a length-one integer"
  )
  expect_error(
    impute_date_ranges(as.Date("2024-01-01"), end_late = c(1L, 2L)),
    "`end_late` must be a length-one integer"
  )
  expect_error(
    impute_date_ranges(as.Date("2024-01-01"), expand = NA),
    "`expand` must be TRUE or FALSE"
  )
})

test_that("expanded imputed date ranges can be used with s2cd", {
  dates <- impute_date_ranges(
    as.Date(c("2024-01-01", "2024-01-05")),
    expand = TRUE
  )
  x <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = dates
  )

  expect_s3_class(x, "s2cd")
  expect_equal(s2cd_dates(x), dates)
})

test_that("impute date ranges works with grouped df", {
  d <-
    tibble::tibble(
      id = rep(c("A", "B"), each = 3),
      encounter = rep(1:3, 2),
      date = as.Date(c(
        "2024-01-01",
        "2024-03-17",
        "2024-09-21",
        "2023-11-29",
        "2024-09-22",
        "2024-09-29"
      ))
    ) |>
    dplyr::mutate(
      imputed_start_date = impute_date_ranges(date)$start,
      imputed_end_date = impute_date_ranges(date)$end,
      .by = "id"
    )

  expect_equal(nrow(d), 6)
  expect_equal(ncol(d), 5)
  expect_equal(
    d$imputed_start_date,
    as.Date(c(
      "2024-01-01",
      "2024-02-08",
      "2024-06-19",
      "2023-11-29",
      "2024-04-26",
      "2024-09-25"
    ))
  )
  expect_equal(
    d$imputed_end_date,
    as.Date(c(
      "2024-02-08",
      "2024-06-19",
      "2024-09-21",
      "2024-04-26",
      "2024-09-25",
      "2024-09-29"
    ))
  )
})
