test_that("get_tiger_bg() works", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(12)
  out <- get_tiger_bg(s2cd_example_cincy(n_locations = 16L))
  expect_type(out, "character")
  expect_length(out, 16)
  expect_true(all(sapply(out, nchar) == 12))
})

test_that("get_tiger_bg() preserves input order across states", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  x <- s2::as_s2_cell(c(
    "8841b3e2ecf45989",
    "8841d6ea2decb2d1",
    NA_character_,
    "8841b0c64248fe83",
    "8841d6ea2decb2d1",
    "8841b58ec7807bf3"
  ))

  out <- get_tiger_bg(x, quiet = TRUE)

  expect_type(out, "character")
  expect_length(out, length(x))
  expect_null(names(out))
  expect_identical(
    out,
    c(
      "390610011001",
      "180290803011",
      NA_character_,
      "211170614002",
      "180290803011",
      "390610102023"
    )
  )
  expect_identical(out[[5]], out[[2]])
})

test_that("get_tiger_bg() handles inputs without non-missing cells", {
  testthat::local_mocked_bindings(
    tiger_states = function(...) stop("TIGER files should not be read"),
    .package = "geomarker"
  )

  expect_identical(
    get_tiger_bg(s2::as_s2_cell(character())),
    character()
  )
  expect_identical(
    get_tiger_bg(s2::as_s2_cell(c(NA_character_, NA_character_))),
    c(NA_character_, NA_character_)
  )
})

test_that("get_tiger_bg is the only exported TIGER linkage API", {
  exports <- getNamespaceExports("geomarker")
  expect_true("get_tiger_bg" %in% exports)
  expect_false("s2_join_tiger_bg" %in% exports)
})

test_that("TIGER URLs use Census HTTPS source", {
  expect_identical(
    tiger_state_url(2024),
    "https://www2.census.gov/geo/tiger/TIGER2024/STATE/tl_2024_us_state.zip"
  )
  expect_identical(
    tiger_block_group_url("39", 2024),
    "https://www2.census.gov/geo/tiger/TIGER2024/BG/tl_2024_39_bg.zip"
  )
})

test_that("get_tiger_bg() validates full-resolution cells", {
  expect_error(
    get_tiger_bg(s2::s2_cell("8841")),
    "full-resolution level 30"
  )
})
