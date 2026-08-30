test_that("daily smoke data can be read in from online", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  xx <- get_daily_smoke_data(as.Date(c("2025-06-18", "2025-06-19")))
  expect_length(xx, 2)
  expect_equal(lengths(xx), c("2025-06-18" = 2, "2025-06-19" = 2))
  expect_equal(nrow(xx[[1]]), 48)
  expect_equal(nrow(xx[[2]]), 226)
  expect_true(inherits(xx[[1]]$density, c("ordered", "factor")))
  expect_true(inherits(xx[[1]]$geometry, c("s2_geography", "wk_vctr")))
  expect_true(inherits(xx[[1]], c("tbl_df", "tbl", "data.frame")))
})

test_that("hms smoke works with fixture data", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(9023)
  xx <- s2cd_example_cincy(n_locations = 20L)
  out <- get_smoke_summary(xx)
  expect_type(out, "list")
  expect_length(out, 20)
  expect_true(inherits(out[[1]], c("ord", "factor")))
  expect_length(out[[1]], 3)
  expect_length(out[[2]], 3)
  expect_identical(levels(out[[1]]), c("None", "Light", "Medium", "Heavy"))
})

smoke_summary_test_x <- function() {
  cells <- s2::as_s2_cell(s2::s2_lnglat(
    c(-84.5, -83.5, -82.5),
    c(39.1, 39.1, 39.1)
  ))
  s2cd(
    cells,
    dates = list(
      as.Date(c("2024-01-01", "2024-01-02")),
      as.Date("2024-01-01"),
      as.Date("2024-01-01")
    )
  )
}

smoke_summary_test_data <- function(dates) {
  geometry <- s2::s2_geog_from_text(c(
    paste0(
      "POLYGON ((-84.75 38.85, -84.25 38.85, -84.25 39.35, ",
      "-84.75 39.35, -84.75 38.85))"
    ),
    paste0(
      "POLYGON ((-84.75 38.85, -84.25 38.85, -84.25 39.35, ",
      "-84.75 39.35, -84.75 38.85))"
    ),
    paste0(
      "POLYGON ((-83.75 38.85, -83.25 38.85, -83.25 39.35, ",
      "-83.75 39.35, -83.75 38.85))"
    ),
    paste0(
      "POLYGON ((-83.75 38.85, -83.25 38.85, -83.25 39.35, ",
      "-83.75 39.35, -83.75 38.85))"
    )
  ))
  smoke <- tibble::tibble(
    geometry = geometry,
    density = factor(
      c("Light", "Light", "Light", "Heavy"),
      levels = c("None", "Light", "Medium", "Heavy"),
      ordered = TRUE
    )
  )
  stats::setNames(rep(list(smoke), length(dates)), as.character(dates))
}

test_that("smoke intersections support raw, maximum, and sum summaries", {
  testthat::local_mocked_bindings(
    get_daily_smoke_data = function(x, ...) smoke_summary_test_data(x),
    .package = "geomarker"
  )
  x <- smoke_summary_test_x()

  raw <- get_smoke_summary(x, summary = "none")
  expect_length(raw, 3)
  expect_length(raw[[1]], 2)
  expect_identical(names(raw[[1]]), c("2024-01-01", "2024-01-02"))
  expect_identical(as.character(raw[[1]][[1]]), c("Light", "Light"))
  expect_identical(as.character(raw[[2]][[1]]), c("Light", "Heavy"))
  expect_identical(length(raw[[3]][[1]]), 0L)
  expect_s3_class(raw[[3]][[1]], "ordered")
  expect_identical(
    levels(raw[[3]][[1]]),
    c("None", "Light", "Medium", "Heavy")
  )

  maximum <- get_smoke_summary(x, summary = "max")
  expect_identical(as.character(maximum[[1]]), c("Light", "Light"))
  expect_identical(as.character(maximum[[2]]), "Heavy")
  expect_identical(as.character(maximum[[3]]), "None")
  expect_identical(names(maximum[[1]]), c("2024-01-01", "2024-01-02"))

  total <- get_smoke_summary(x, summary = "sum")
  expect_identical(unname(total[[1]]), c(2, 2))
  expect_identical(unname(total[[2]]), 4)
  expect_identical(unname(total[[3]]), 0)
  expect_identical(names(total[[1]]), c("2024-01-01", "2024-01-02"))
})

test_that("maximum smoke summary remains the default", {
  testthat::local_mocked_bindings(
    get_daily_smoke_data = function(x, ...) smoke_summary_test_data(x),
    .package = "geomarker"
  )
  x <- smoke_summary_test_x()

  expect_identical(
    get_smoke_summary(x),
    get_smoke_summary(x, summary = "max")
  )
})

test_that("smoke summary validates the summary method", {
  expect_error(
    get_smoke_summary(smoke_summary_test_x(), summary = "mean"),
    "should be one of"
  )
})
