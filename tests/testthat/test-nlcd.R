test_that("get_nlcd_fct_imp_data works with fixture dir", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(11)
  xx <- s2cd_example_cincy(n_locations = 20L)
  out <- get_nlcd_fct_imp_data(xx)
  out
})

test_that("get_nlcd_fct_imp_data works with muliple years", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  set.seed(99)
  xx <- s2cd_example_cincy(10L, 5L, "poisson+1")
  xx@dates[[2]][1] <- xx@dates[[2]][1] - (365 * 2)
  out <- get_nlcd_fct_imp_data(xx, buffer = 400)
  expect_length(out, length(xx))
  expect_type(out, "list")
  expect_type(out[[1]], "double")
  expect_type(out[[2]], "double")
  expect_length(unique(out[[1]]), 1)
  expect_length(unique(out[[2]]), 2)
  expect_length(unique(out[[3]]), 1)
  expect_true(all(lengths(xx@dates) == lengths(out)))
})
