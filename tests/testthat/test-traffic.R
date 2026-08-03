test_that("traffic data manifest is pinned to geomarker HPMS 2024", {
  manifest <- traffic_data_manifest()
  expect_identical(manifest[["HPMS-Year"]], "2024")
  expect_identical(manifest[["Transformation-ID"]], "f12-aadt-v1")
  expect_identical(manifest[["Package-Release"]], "v0.0.1")
  expect_identical(manifest[["Asset-Name"]], "hpms_2024_f12_aadt.gpkg")
  expect_match(manifest[["Asset-URL"]], "geomarker-io/geomarker")
  expect_false(grepl("appc|2020", manifest[["Asset-URL"]]))
})

test_that("get_traffic_summary validates inputs", {
  x <- s2cd_example()
  expect_error(get_traffic_summary(s2::as_s2_cell(x)), "s2_cell_dates")
  expect_error(get_traffic_summary(x, buffer = c(400, 800)), "length one")
  expect_error(get_traffic_summary(x, buffer = -1), "must not be negative")
  expect_error(get_traffic_summary(x, buffer = Inf), "must be finite")
})

test_that("get_traffic_summary preserves fixture results and duplicates", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package("geomarker", "gmrkr--8841"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(923)
  x <- s2cd_example_cincy(n_locations = 20L)
  x <- vctrs::vec_c(x, x[c(3, 7)])
  out <- get_traffic_summary(x, quiet = TRUE)
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c(
      "aadtm_trucks_buses",
      "aadtm_tractor_trailer",
      "aadtm_passenger"
    )
  )
  expect_equal(nrow(out), length(x))
  expect_equal(as.numeric(out[21, ]), as.numeric(out[3, ]))
  expect_equal(as.numeric(out[22, ]), as.numeric(out[7, ]))
  expect_true(all(vapply(out, is.numeric, logical(1))))
})

test_that("regional traffic summaries match a full fixture scan", {
  withr::local_envvar(
    R_USER_DATA_DIR = fs::path_package("geomarker", "gmrkr--8841"),
    R_GEOMARKER_NO_DOWNLOAD = "true"
  )
  set.seed(924)
  x <- s2cd_example_cincy(n_locations = 30L)
  cells <- s2::as_s2_cell(x)
  centers <- s2::s2_cell_center(cells)
  hpms <- sf::read_sf(
    traffic_data_file(quiet = TRUE),
    layer = traffic_data_manifest()[["Asset-Layer"]],
    quiet = TRUE
  )
  roads <- s2::as_s2_geography(sf::st_geometry(hpms))
  hpms <- sf::st_drop_geometry(hpms)
  nearby <- s2::s2_dwithin_matrix(centers, roads, distance = 400)
  reference <- Map(
    function(road_indices, center) {
      if (length(road_indices) == 0) {
        return(traffic_zero_summary())
      }
      lengths <- s2::s2_intersection(
        roads[road_indices],
        s2::s2_buffer_cells(
          center,
          distance = 400,
          max_cells = 1000,
          min_level = -1
        )
      ) |>
        s2::s2_length()
      c(
        aadtm_trucks_buses = sum(
          hpms$AADT_SINGLE_UNIT[road_indices] * lengths
        ),
        aadtm_tractor_trailer = sum(
          hpms$AADT_COMBINATION[road_indices] * lengths
        ),
        aadtm_passenger = sum(
          (hpms$AADT[road_indices] -
            hpms$AADT_SINGLE_UNIT[road_indices] -
            hpms$AADT_COMBINATION[road_indices]) *
            lengths
        )
      )
    },
    nearby,
    centers
  )
  reference <- do.call(rbind, reference)
  expect_equal(
    get_traffic_summary(x, quiet = TRUE),
    as.data.frame(reference),
    tolerance = 1e-8
  )
})
