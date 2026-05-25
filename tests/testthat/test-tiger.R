test_that("s2_join_tiger_bg() works", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package(
      "geomarker",
      "gmrkr--8841"
    ),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(12)
  out <- s2_join_tiger_bg(s2cd_example_cincy(n_locations = 16L))
  expect_type(out, "character")
  expect_length(out, 16)
  expect_true(all(sapply(out, nchar) == 12))
})

test_that("s2_join_tiger_bg() validates full-resolution cells", {
  expect_error(
    s2_join_tiger_bg(s2::s2_cell("8841")),
    "full-resolution level 30"
  )
})
