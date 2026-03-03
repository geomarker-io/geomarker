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
