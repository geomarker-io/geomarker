library(testthat)

repo_dir <- normalizePath(test_path("..", ".."))
new_release_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_dir, "scripts", "data-release-helpers.R"), env)
  sys.source(file.path(repo_dir, "scripts", "release-data.R"), env)
  env
}

staged_test_record <- function(env) {
  work <- tempfile("release-tests-")
  record <- env$release_records(repo_dir)[[1]]
  directory <- file.path(work, "stage", record[["Data-Release"]])
  dir.create(directory, recursive = TRUE)
  data <- file.path(directory, record[["Asset-Name"]])
  writeBin(charToRaw("test data\n"), data)
  record[["Asset-Bytes"]] <- as.character(file.info(data)$size)
  record[["Asset-SHA256"]] <- env$release_hash(data)
  manifest <- env$release_manifest(record, directory)
  files <- setNames(
    c(data, manifest),
    c(record[["Asset-Name"]], "manifest.dcf")
  )
  assets <- lapply(names(files), function(name) {
    list(
      name = name,
      size = file.info(files[[name]])$size,
      digest = paste0("sha256:", env$release_hash(files[[name]]))
    )
  })
  list(work = work, record = record, files = files, assets = assets)
}

test_that("tags describe the data vintage and positive revision", {
  env <- new_release_env()
  expect_identical(
    env$data_release_tag(list(`HPMS-Year` = "2024")),
    "data-hpms-2024-r1"
  )
  expect_identical(
    env$data_release_tag(list(`MERRA-Year` = "2024", `MERRA-Half` = "H2"), "2"),
    "data-merra2-2024-h2-r2"
  )
  expect_identical(
    env$data_release_tag(list(`Source-Service-Data-As-Of` = "2026-07-20")),
    "data-elevation-2026-07-20-r1"
  )
  for (value in c("0", "-1", "1.5", "01", "r2", "", NA_character_)) {
    expect_error(env$data_release_revision(value), "positive integer")
  }
  expect_error(
    env$data_release_tag(list(`MERRA-Year` = "2024", `MERRA-Half` = "H3")),
    "vintage"
  )
  expect_identical(
    env$data_release_args(c("file", "--revision", "2")),
    list(args = "file", revision = "2")
  )
  expect_error(
    env$data_release_args(c("--revision", "1", "--revision", "2")),
    "Supply --revision once"
  )
})

test_that("catalogs contain exactly the supported releases and consistent pins", {
  env <- new_release_env()
  records <- env$release_records(repo_dir)
  expect_length(records, 21)
  expect_length(Filter(function(x) !is.null(x[["MERRA-Year"]]), records), 19)
  expect_true("data-merra2-2026-h1-r1" %in% names(records))
  expect_false("data-merra2-2026-h2-r1" %in% names(records))
  expect_error(
    env$release_records(repo_dir, "data-merra2-2026-h2-r1"),
    "Unknown"
  )
  bad_repo <- tempfile()
  dir.create(file.path(bad_repo, "inst"), recursive = TRUE)
  file.copy(
    file.path(
      repo_dir,
      "inst",
      c("elevation-data.dcf", "traffic-data.dcf", "merra-data.dcf")
    ),
    file.path(bad_repo, "inst")
  )
  withr::defer(unlink(bad_repo, recursive = TRUE))
  record <- records[[1]]
  record[["Asset-URL"]] <- "https://example.com/latest.tif"
  env$data_release_write(
    record,
    file.path(bad_repo, "inst", "elevation-data.dcf")
  )
  expect_error(env$release_records(bad_repo), "Inconsistent")
})

test_that("staging checks full content and refuses corruption or missing files", {
  env <- new_release_env()
  x <- staged_test_record(env)
  withr::defer(unlink(x$work, recursive = TRUE))
  env$release_gh <- function(...) stop("Network must not be used")
  expect_message(env$release_stage(x$record, x$work), "Staged and verified")
  writeBin(charToRaw("bad data!\n"), x$files[[1]])
  expect_error(env$release_stage(x$record, x$work), "SHA-256 mismatch")
  unlink(x$files[[1]])
  expect_error(env$release_stage(x$record, x$work), "Place the data file")
  expect_error(
    env$release_validate_file(x$files[[1]], x$record),
    "Missing file"
  )
})

test_that("preview does not call GitHub or create a work directory", {
  env <- new_release_env()
  work <- tempfile()
  env$release_gh <- function(...) stop("GitHub must not be called")
  expect_output(
    env$release_data_main(c("preview", "--work-dir", work), repo_dir),
    "data-hpms-2024-r1"
  )
  expect_false(dir.exists(work))
  expect_error(
    env$release_work_dir(file.path(repo_dir, "staging"), repo_dir),
    "outside"
  )
  link <- tempfile()
  file.symlink(repo_dir, link)
  withr::defer(unlink(link))
  expect_error(
    env$release_work_dir(file.path(link, "staging"), repo_dir),
    "outside"
  )
})

test_that("matching published releases are skipped and conflicting releases fail", {
  env <- new_release_env()
  x <- staged_test_record(env)
  withr::defer(unlink(x$work, recursive = TRUE))
  commit <- strrep("a", 40)
  remote <- list(draft = FALSE, immutable = TRUE, assets = x$assets)
  env$release_tag_sha <- function(tag) commit
  env$release_api <- function(...) remote
  env$release_gh <- function(...) stop("Published assets must not be changed")
  expect_message(env$release_publish(x$record, x$work, commit), "exact match")
  remote$assets[[1]]$digest <- paste0("sha256:", strrep("0", 64))
  expect_error(
    env$release_publish(x$record, x$work, commit),
    "Conflicting remote asset"
  )
  remote$assets <- x$assets
  expect_error(
    env$release_publish(x$record, x$work, strrep("b", 40)),
    "Conflicting tag target"
  )
  remote$immutable <- FALSE
  expect_error(env$release_publish(x$record, x$work, commit), "not immutable")
  remote$assets <- c(x$assets, list(list(name = "unexpected")))
  expect_error(env$release_publish(x$record, x$work, commit), "Unexpected")
})

test_that("an interrupted draft resumes only the missing upload before publication", {
  env <- new_release_env()
  x <- staged_test_record(env)
  withr::defer(unlink(x$work, recursive = TRUE))
  commit <- strrep("a", 40)
  remote <- list(draft = TRUE, immutable = FALSE, assets = x$assets[1])
  calls <- list()
  env$release_api <- function(...) remote
  env$release_tag_sha <- function(tag) commit
  env$release_gh <- function(args, ...) {
    calls[[length(calls) + 1L]] <<- args
    if (args[2] == "upload") {
      remote$assets <<- x$assets
    }
    if (args[2] == "edit") {
      remote$draft <<- FALSE
      remote$immutable <<- TRUE
    }
    ""
  }
  expect_message(
    env$release_publish(x$record, x$work, commit),
    "Published and verified"
  )
  expect_length(calls, 2)
  expect_identical(calls[[1]][2], "upload")
  expect_identical(basename(calls[[1]][4]), "manifest.dcf")
  expect_true("--latest=false" %in% calls[[2]])
  expect_false(any(vapply(calls, function(x) "--clobber" %in% x, logical(1))))
})

test_that("fresh verification checks manifest contents and refuses reused directories", {
  env <- new_release_env()
  x <- staged_test_record(env)
  withr::defer(unlink(x$work, recursive = TRUE))
  remote <- list(draft = FALSE, immutable = TRUE, assets = x$assets)
  env$release_api <- function(...) remote
  env$release_gh <- function(args, ...) {
    directory <- args[match("--dir", args) + 1L]
    file.copy(x$files, directory)
    ""
  }
  expect_message(
    env$release_verify(x$record, x$work),
    "Fresh download verified"
  )
  expect_error(env$release_verify(x$record, x$work), "already exists")
  manifest <- x$files[["manifest.dcf"]]
  bad <- x$record
  bad[["Attribution"]] <- "wrong attribution"
  env$data_release_write(bad, manifest)
  expect_error(
    env$release_manifest_matches(manifest, x$record),
    "does not match"
  )
})
