test_that("s2cd behaves as expected", {
  d <- s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )

  d |>
    expect_s3_class(c("s2cd", "vctrs_rcrd", "vctrs_vctr"))

  d |>
    is_s2cd() |>
    expect_true()

  s2cd_example() |>
    is_s2cd() |>
    expect_true()

  letters |>
    is_s2cd() |>
    expect_false()

  s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")) |>
    is_s2cd() |>
    expect_false()

  expect_error(
    geomarker:::validate_s2cd(
      .data = c("8841b39a7c46e25f", "8841a45555555555"),
      dates = list(as.Date("2026-01-01"), as.Date("2026-01-02"))
    ),
    "`s2cd` must contain an `s2_cell` vector."
  )

  expect_error(
    geomarker:::validate_s2cd(
      .data = s2::as_s2_cell("not-a-cell"),
      dates = list(as.Date("2026-01-01"))
    ),
    "s2 cells must be valid"
  )

  expect_error(
    geomarker:::validate_s2cd(
      .data = s2::as_s2_cell("8841b39a7c46e25f"),
      dates = as.Date("2026-01-01")
    ),
    "`dates` must be a list."
  )

  expect_error(
    geomarker:::validate_s2cd(
      .data = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
      dates = list(as.Date("2026-01-01"))
    ),
    "`dates` must have same length as `cells`."
  )

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  ) |>
    expect_error("all s2 cells must be level 30")

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      TRUE,
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  ) |>
    expect_error("all elements in the dates list must be Date vectors")

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2022-09-13"), as.Date(NA), as.Date("2023-09-20"))
    )
  ) |>
    expect_error("dates must not contain missing values")

  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2023-09-20"))
    )
  ) |>
    expect_error("each Date vector must be in chronological order")

  expect_error(
    s2cd(
      s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
      dates = list(
        as.Date("2026-01-01"),
        c(as.Date("2024-09-13"), as.Date("2023-09-20"))
      ),
      sort_dates = NA
    ),
    "`sort_dates` must be TRUE or FALSE."
  )

  expect_warning(
    d_sorted <- s2cd(
      s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
      dates = list(
        as.Date("2026-01-01"),
        c(as.Date("2024-09-13"), as.Date("2023-09-20"))
      ),
      sort_dates = TRUE
    ),
    "Because `sort_dates = TRUE`"
  )
  expect_identical(
    s2cd_dates(d_sorted)[[2]],
    as.Date(c("2023-09-20", "2024-09-13"))
  )

  d |>
    tibble::as_tibble() |>
    expect_s3_class(c("tbl_df", "tbl", "data.frame")) |>
    names() |>
    expect_equal(c("s2_cell", "dates"))

  d |>
    as.data.frame() |>
    expect_s3_class("data.frame") |>
    names() |>
    expect_equal(c("s2_cell", "dates"))

  d |>
    as.numeric() |>
    expect_type("double")

  d |>
    as.character() |>
    expect_type("character")

  expect_identical(
    as.character(d),
    c("8841b39a7c46e25f", "8841a45555555555")
  )
  expect_identical(format(d), as.character(d))
  expect_identical(as.character(d[2]), "8841a45555555555")

  # use external method to extract s2
  d |>
    as_s2_cell() |>
    expect_s3_class("s2_cell")

  # convert to character to extract s2
  d |>
    as.character() |>
    s2::as_s2_cell() |>
    expect_s3_class("s2_cell")

  # ensure underlying data is double
  d |>
    s2::as_s2_cell() |>
    unclass() |>
    expect_type("double")

  # get dates list
  expect_type(s2cd_dates(d), "list")
  vapply(s2cd_dates(d), class, character(1)) |>
    expect_identical(rep("Date", length(d)))

  # use as a first-class tibble column
  tbl <- tibble::tibble(id = seq_along(d), loc = d)
  expect_s3_class(tbl$loc, "s2cd")

  sliced <- dplyr::slice(tbl, 2)
  expect_s3_class(sliced$loc, "s2cd")
  expect_identical(as.data.frame(sliced$loc), as.data.frame(d[2]))

  filtered <- dplyr::filter(tbl, id == 2)
  expect_s3_class(filtered$loc, "s2cd")
  expect_identical(as.data.frame(filtered$loc), as.data.frame(d[2]))

  arranged <- dplyr::arrange(tbl, dplyr::desc(id))
  expect_s3_class(arranged$loc, "s2cd")
  expect_identical(
    as.data.frame(arranged$loc),
    as.data.frame(d[c(2, 1)])
  )

  # as_s2cd
  my_d <-
    data.frame(
      s2_cell = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
      dates = I(list(
        as.Date("2026-01-01"),
        c(as.Date("2024-09-13"), as.Date("2024-09-20"))
      ))
    )

  expect_identical(d[1], s2cd(s2::as_s2_cell(d)[1], s2cd_dates(d)[1]))

  expect_identical(d, as_s2cd(d))

  as_s2cd(my_d) |>
    expect_s3_class("s2cd")

  my_d |>
    tibble::as_tibble() |>
    as_s2cd() |>
    expect_s3_class("s2cd")

  my_unsorted_d <- my_d
  my_unsorted_d$dates[[2]] <- rev(my_unsorted_d$dates[[2]])
  expect_warning(
    my_unsorted_s2cd <- as_s2cd(my_unsorted_d, sort_dates = TRUE),
    "Because `sort_dates = TRUE`"
  )
  expect_identical(
    s2cd_dates(my_unsorted_s2cd)[[2]],
    my_d$dates[[2]]
  )

  expect_identical(my_d, as.data.frame(as_s2cd(my_d)))
})
