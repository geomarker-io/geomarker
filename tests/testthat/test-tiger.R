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
