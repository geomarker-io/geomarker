test_that("get_nlcd_fct_imp_data works with fixture dir", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(11)
  x <- s2cd_example_cincy(n_locations = 20L)
  expect_no_warning(out <- get_nlcd_fct_imp_data(x))
  expect_length(out, length(x))
  expect_type(out, "list")
  expect_type(out[[1]], "double")
  expect_type(out[[2]], "double")
  expect_true(all(is.finite(unlist(out, use.names = FALSE))))
  expect_identical(
    out[[2]],
    rep(0, length(s2cd_dates(x)[[2]]))
  )
})

test_that("get_nlcd_fct_imp_data works with multiple years", {
  set.seed(99)
  x <- s2cd_example_cincy(10L, 5L, "poisson+1")
  x_dates <- s2cd_dates(x)
  x_dates[[2]][1] <- x_dates[[2]][1] - (365 * 2)
  x <- s2cd(s2::as_s2_cell(x), x_dates)
  years <- sort(unique(unlist(lapply(s2cd_dates(x), format, "%Y"))))
  urls <- paste0(
    "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/",
    "Annual_NLCD_FctImp_",
    years,
    "_CU_C1V2.zip"
  )
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = "true")
  cached <- vapply(
    urls,
    \(url) {
      !inherits(
        try(
          geomarker_stow(
            url,
            "get_nlcd_fct_imp_data",
            quiet = TRUE
          ),
          silent = TRUE
        ),
        "try-error"
      )
    },
    logical(1)
  )
  skip_if_not(
    all(cached),
    "Annual NLCD durable managed local copies are unavailable"
  )
  expect_no_warning(out <- get_nlcd_fct_imp_data(x, buffer = 400))
  expect_length(out, length(x))
  expect_type(out, "list")
  expect_type(out[[1]], "double")
  expect_type(out[[2]], "double")
  expect_length(unique(out[[1]]), 1)
  expect_length(unique(out[[2]]), 2)
  expect_length(unique(out[[3]]), 1)
  expect_true(all(lengths(s2cd_dates(x)) == lengths(out)))
})

test_that("unavailable Annual NLCD years return missing values", {
  x <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date(c("2026-01-01", "2026-06-01")),
      as.Date("1970-01-01")
    )
  )

  expect_identical(
    get_nlcd_fct_imp_data(x),
    list(c(NA_real_, NA_real_), NA_real_)
  )
})
