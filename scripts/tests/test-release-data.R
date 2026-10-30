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

test_that("unpublished tags are found through authenticated draft release IDs", {
  env <- new_release_env()
  remote <- list(id = 123L, draft = TRUE, target_commitish = strrep("a", 40))
  env$release_api <- function(endpoint, optional = FALSE) {
    if (startsWith(endpoint, "releases/tags/")) {
      expect_true(optional)
      return(NULL)
    }
    expect_identical(endpoint, "releases/123")
    remote
  }
  env$release_gh <- function(args, ...) {
    expect_true("--paginate" %in% args)
    "123"
  }
  expect_identical(env$release_get("data-hpms-2024-r1"), remote)
  env$release_gh <- function(...) ""
  expect_null(env$release_get("data-hpms-2024-r1", optional = TRUE))
  expect_error(env$release_get("data-hpms-2024-r1"), "not found")
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
  remote <- list(
    id = 123L,
    draft = TRUE,
    immutable = FALSE,
    assets = x$assets[1]
  )
  calls <- list()
  env$release_api <- function(...) remote
  env$release_tag_sha <- function(tag) commit
  env$release_write <- function(endpoint, method, body = NULL, file = NULL) {
    calls[[length(calls) + 1L]] <<- list(
      endpoint = endpoint,
      method = method,
      body = body,
      file = file
    )
    if (method == "POST") {
      asset <- Filter(function(asset) asset$name == basename(file), x$assets)[[
        1
      ]]
      remote$assets <<- c(remote$assets, list(asset))
      return(asset)
    }
    remote$draft <<- FALSE
    remote$immutable <<- TRUE
    remote
  }
  expect_message(
    env$release_publish(x$record, x$work, commit),
    "Published and verified"
  )
  expect_length(calls, 2)
  expect_identical(calls[[1]]$method, "POST")
  expect_identical(basename(calls[[1]]$file), "manifest.dcf")
  expect_match(
    calls[[1]]$endpoint,
    "/releases/123/assets[?]name=manifest[.]dcf$"
  )
  expect_identical(calls[[2]]$body, list(draft = FALSE, make_latest = "false"))
})

test_that("new publication uses returned release IDs despite stale tag lookups", {
  env <- new_release_env()
  x <- staged_test_record(env)
  withr::defer(unlink(x$work, recursive = TRUE))
  commit <- strrep("a", 40)
  remote <- NULL
  reads <- 0L
  calls <- list()
  env$release_get <- function(...) {
    reads <<- reads + 1L
    NULL
  }
  env$release_api <- function(endpoint, ...) {
    if (startsWith(endpoint, "commits/")) {
      return(list(sha = commit))
    }
    stop("Unexpected follow-up lookup")
  }
  env$release_tag_sha <- function(tag) {
    if (!is.null(remote) && !remote$draft) commit else NULL
  }
  env$release_write <- function(endpoint, method, body = NULL, file = NULL) {
    calls[[length(calls) + 1L]] <<- list(
      endpoint = endpoint,
      method = method,
      body = body,
      file = file
    )
    if (identical(endpoint, "releases")) {
      remote <<- list(
        id = 123L,
        draft = TRUE,
        target_commitish = body$target_commitish,
        assets = list()
      )
    } else if (method == "POST") {
      asset <- Filter(function(asset) asset$name == basename(file), x$assets)[[
        1
      ]]
      remote$assets <<- c(remote$assets, list(asset))
      return(asset)
    } else if (method == "PATCH") {
      expect_length(remote$assets, 2)
      remote$draft <<- FALSE
      remote$immutable <<- TRUE
    }
    remote
  }
  expect_message(
    env$release_publish(x$record, x$work, commit),
    "Published and verified"
  )
  expect_identical(reads, 1L)
  expect_identical(
    vapply(calls, `[[`, "", "method"),
    c("POST", "POST", "POST", "PATCH")
  )
  expect_identical(calls[[1]]$body$target_commitish, commit)
  expect_identical(calls[[1]]$body$make_latest, "false")
  expect_true(calls[[1]]$body$draft)
  expect_identical(calls[[4]]$endpoint, "releases/123")
})

test_that("release writes preserve JSON types and send binary files unchanged", {
  env <- new_release_env()
  captured <- NULL
  payload <- NULL
  env$release_gh <- function(args, ...) {
    captured <<- args
    input <- args[match("--input", args) + 1L]
    if ("Content-Type: application/json" %in% args) {
      payload <<- jsonlite::read_json(input)
    }
    '{"id":123}'
  }
  out <- env$release_write(
    "releases",
    "POST",
    body = list(draft = TRUE, make_latest = "false")
  )
  expect_identical(out$id, 123L)
  expect_identical(payload, list(draft = TRUE, make_latest = "false"))
  expect_true("repos/geomarker-io/geomarker/releases" %in% captured)
  file <- tempfile()
  writeBin(as.raw(c(0, 255, 1)), file)
  withr::defer(unlink(file))
  env$release_write("https://uploads.github.com/test", "POST", file = file)
  expect_true("Content-Type: application/octet-stream" %in% captured)
  expect_identical(captured[match("--input", captured) + 1L], file)
  expect_identical(readBin(file, "raw", n = 3), as.raw(c(0, 255, 1)))
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
