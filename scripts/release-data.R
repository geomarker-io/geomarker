#!/usr/bin/env Rscript

release_repo <- "geomarker-io/geomarker"

release_stop <- function(...) stop(..., call. = FALSE)

release_gh <- function(args, optional = FALSE) {
  error_file <- tempfile("gh-error-")
  on.exit(unlink(error_file))
  out <- suppressWarnings(system2(
    "gh",
    shQuote(args),
    stdout = TRUE,
    stderr = error_file
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    detail <- paste(readLines(error_file, warn = FALSE), collapse = "\n")
    if (optional && grepl("HTTP 404", detail, fixed = TRUE)) {
      return(NULL)
    }
    release_stop("GitHub command failed: ", detail)
  }
  paste(out, collapse = "\n")
}

release_api <- function(endpoint, optional = FALSE) {
  out <- release_gh(
    c(
      "api",
      paste0("repos/", release_repo, "/", endpoint),
      "-H",
      "X-GitHub-Api-Version: 2026-03-10"
    ),
    optional
  )
  if (is.null(out)) {
    return(NULL)
  }
  jsonlite::fromJSON(out, simplifyVector = FALSE)
}

release_hash <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}

release_validate_file <- function(path, record) {
  if (!file.exists(path)) {
    release_stop("Missing file: ", path)
  }
  if (
    file.info(path)$size != as.numeric(record[["Asset-Bytes"]]) ||
      release_hash(path) != record[["Asset-SHA256"]]
  ) {
    release_stop("Size or SHA-256 mismatch: ", path)
  }
  invisible(path)
}

release_records <- function(repo_dir, tag = NULL) {
  catalogs <- file.path(
    repo_dir,
    "inst",
    c("elevation-data.dcf", "traffic-data.dcf", "merra-data.dcf")
  )
  records <- unlist(
    lapply(catalogs, function(path) {
      rows <- read.dcf(path)
      lapply(seq_len(nrow(rows)), function(i) {
        as.list(rows[i, !is.na(rows[i, ])])
      })
    }),
    recursive = FALSE
  )
  for (record in records) {
    required <- c(
      "Data-Release",
      "Asset-Name",
      "Asset-URL",
      "Asset-Bytes",
      "Asset-SHA256"
    )
    if (
      !all(required %in% names(record)) ||
        any(!nzchar(unlist(record[required])))
    ) {
      release_stop("Incomplete data release metadata.")
    }
    name <- record[["Asset-Name"]]
    value <- record[["Data-Release"]]
    if (
      !grepl("-r[1-9][0-9]*$", value) ||
        value != data_release_tag(record, sub("^.*-r", "", value)) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", name) ||
        name == "manifest.dcf" ||
        record[["Asset-URL"]] != data_release_url(value, name) ||
        !grepl("^[0-9a-f]{64}$", record[["Asset-SHA256"]]) ||
        !grepl("^[1-9][0-9]*$", record[["Asset-Bytes"]]) ||
        !is.null(record[["Package-Release"]])
    ) {
      release_stop("Inconsistent data release metadata: ", value)
    }
    if (
      !identical(
        record[["Fixture-Name"]],
        data_release_pin(record, sub("^.*-r", "", value))[["Fixture-Name"]]
      )
    ) {
      release_stop("Inconsistent fixture filename: ", value)
    }
  }
  tags <- vapply(records, `[[`, "", "Data-Release")
  if (anyDuplicated(tags)) {
    release_stop("Duplicate data release tags in catalogs.")
  }
  names(records) <- tags
  if (!is.null(tag)) {
    if (!tag %in% tags) {
      release_stop("Unknown data release tag: ", tag)
    }
    records <- records[tag]
  }
  records
}

release_manifest <- function(record, directory) {
  path <- file.path(directory, "manifest.dcf")
  data_release_write(record, path)
  path
}

release_manifest_matches <- function(path, record) {
  rows <- read.dcf(path)
  if (
    nrow(rows) != 1L ||
      !setequal(colnames(rows), names(record)) ||
      !identical(as.list(rows[1, names(record)]), record)
  ) {
    release_stop("Manifest does not match catalog: ", path)
  }
  invisible(path)
}

release_work_dir <- function(path, repo_dir) {
  if (!startsWith(path, "/")) {
    path <- file.path(getwd(), path)
  }
  path <- normalizePath(path, mustWork = FALSE)
  # Resolve existing parents too, so symlinks cannot put staging inside the checkout.
  parent <- path
  while (!dir.exists(parent)) {
    parent <- dirname(parent)
  }
  path <- file.path(normalizePath(parent), substring(path, nchar(parent) + 2L))
  path <- sub("/$", "", path)
  repo_dir <- normalizePath(repo_dir)
  if (identical(path, repo_dir) || startsWith(path, paste0(repo_dir, "/"))) {
    release_stop("The work directory must be outside the checkout.")
  }
  path
}

release_stage <- function(record, work_dir, source_release = NULL) {
  tag <- record[["Data-Release"]]
  directory <- file.path(work_dir, "stage", tag)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, record[["Asset-Name"]])
  if (!file.exists(path)) {
    if (is.null(source_release)) {
      release_stop(
        "Place the data file at ",
        path,
        " or supply --source-release."
      )
    }
    temporary <- tempfile("download-", directory)
    dir.create(temporary)
    on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
    release_gh(c(
      "release",
      "download",
      source_release,
      "--repo",
      release_repo,
      "--pattern",
      record[["Asset-Name"]],
      "--dir",
      temporary
    ))
    incoming <- file.path(temporary, record[["Asset-Name"]])
    release_validate_file(incoming, record)
    if (!file.rename(incoming, path)) release_stop("Could not stage ", path)
  }
  release_validate_file(path, record)
  manifest <- file.path(directory, "manifest.dcf")
  if (file.exists(manifest)) {
    release_manifest_matches(manifest, record)
  } else {
    release_manifest(record, directory)
  }
  message("Staged and verified ", tag)
}

release_tag_sha <- function(tag) {
  ref <- release_api(paste0("git/ref/tags/", tag), optional = TRUE)
  if (is.null(ref)) {
    return(NULL)
  }
  object <- ref$object
  for (i in seq_len(10L)) {
    if (identical(object$type, "commit")) {
      return(object$sha)
    }
    if (!identical(object$type, "tag")) {
      release_stop("Unexpected tag object: ", tag)
    }
    object <- release_api(paste0("git/tags/", object$sha))$object
  }
  release_stop("Tag nesting too deep: ", tag)
}

release_check_target <- function(release, tag, commit) {
  actual <- release_tag_sha(tag)
  if (is.null(actual)) {
    if (!isTRUE(release$draft)) {
      release_stop("Published release has no tag: ", tag)
    }
    actual <- release_api(paste0("commits/", release$target_commitish))$sha
  }
  if (!identical(actual, commit)) release_stop("Conflicting tag target: ", tag)
}

release_check_assets <- function(release, files, complete = TRUE) {
  assets <- release$assets
  names_found <- vapply(assets, `[[`, "", "name")
  if (
    anyDuplicated(names_found) ||
      any(!names_found %in% names(files)) ||
      (complete && !setequal(names_found, names(files)))
  ) {
    release_stop("Unexpected or missing release assets.")
  }
  for (asset in assets) {
    path <- files[[asset$name]]
    if (
      asset$size != file.info(path)$size ||
        !identical(asset$digest, paste0("sha256:", release_hash(path)))
    ) {
      release_stop("Conflicting remote asset: ", asset$name)
    }
  }
  invisible(names_found)
}

release_publish <- function(record, work_dir, commit) {
  tag <- record[["Data-Release"]]
  directory <- file.path(work_dir, "stage", tag)
  data <- release_validate_file(
    file.path(directory, record[["Asset-Name"]]),
    record
  )
  manifest <- release_manifest_matches(
    file.path(directory, "manifest.dcf"),
    record
  )
  files <- stats::setNames(
    c(data, manifest),
    c(record[["Asset-Name"]], "manifest.dcf")
  )
  release <- release_api(paste0("releases/tags/", tag), optional = TRUE)
  if (is.null(release)) {
    target <- release_tag_sha(tag)
    if (!is.null(target) && !identical(target, commit)) {
      release_stop("Conflicting tag target: ", tag)
    }
    notes <- tempfile("release-notes-")
    on.exit(unlink(notes), add = TRUE)
    writeLines(
      c(
        paste0(
          "- data release `",
          tag,
          "`; independent of the R package version"
        ),
        paste0("- file: `", record[["Asset-Name"]], "`"),
        paste0("- SHA-256: `", record[["Asset-SHA256"]], "`"),
        "- provenance and coverage are recorded in the attached manifest.dcf",
        "- published data are immutable; corrections require a new revision"
      ),
      notes
    )
    release_gh(c(
      "release",
      "create",
      tag,
      "--repo",
      release_repo,
      "--target",
      commit,
      "--draft",
      "--latest=false",
      "--title",
      tag,
      "--notes-file",
      notes
    ))
    release <- release_api(paste0("releases/tags/", tag))
  }
  release_check_target(release, tag, commit)
  present <- release_check_assets(
    release,
    files,
    complete = !isTRUE(release$draft)
  )
  if (!isTRUE(release$draft)) {
    if (!isTRUE(release$immutable)) {
      release_stop("Published release is not immutable: ", tag)
    }
    message("Already published; exact match: ", tag)
    return(invisible(NULL))
  }
  for (name in setdiff(names(files), present)) {
    release_gh(c(
      "release",
      "upload",
      tag,
      files[[name]],
      "--repo",
      release_repo
    ))
  }
  release <- release_api(paste0("releases/tags/", tag))
  release_check_target(release, tag, commit)
  release_check_assets(release, files)
  release_gh(c(
    "release",
    "edit",
    tag,
    "--repo",
    release_repo,
    "--draft=false",
    "--latest=false"
  ))
  release <- release_api(paste0("releases/tags/", tag))
  release_check_target(release, tag, commit)
  release_check_assets(release, files)
  if (isTRUE(release$draft) || !isTRUE(release$immutable)) {
    release_stop("Release was not published immutably: ", tag)
  }
  message("Published and verified ", tag)
}

release_verify <- function(record, work_dir, commit = NULL) {
  tag <- record[["Data-Release"]]
  release <- release_api(paste0("releases/tags/", tag))
  if (isTRUE(release$draft) || !isTRUE(release$immutable)) {
    release_stop("Release is not published immutably: ", tag)
  }
  if (!is.null(commit)) {
    release_check_target(release, tag, commit)
  }
  directory <- file.path(work_dir, "verify", tag)
  if (dir.exists(directory)) {
    release_stop("Fresh verification directory already exists: ", directory)
  }
  dir.create(directory, recursive = TRUE)
  release_gh(c(
    "release",
    "download",
    tag,
    "--repo",
    release_repo,
    "--dir",
    directory
  ))
  data <- release_validate_file(
    file.path(directory, record[["Asset-Name"]]),
    record
  )
  manifest <- release_manifest_matches(
    file.path(directory, "manifest.dcf"),
    record
  )
  files <- stats::setNames(
    c(data, manifest),
    c(record[["Asset-Name"]], "manifest.dcf")
  )
  release_check_assets(release, files)
  message("Fresh download verified ", tag)
}

release_data_main <- function(args, repo_dir) {
  usage <- paste(
    "Usage: Rscript scripts/release-data.R preview|stage|publish|verify",
    "[--work-dir DIR] [--tag TAG] [--source-release TAG] [--commit SHA]"
  )
  if (
    !length(args) || !args[1] %in% c("preview", "stage", "publish", "verify")
  ) {
    release_stop(usage)
  }
  mode <- args[1]
  options <- list()
  rest <- args[-1]
  allowed <- c("--work-dir", "--tag", "--source-release", "--commit")
  while (length(rest)) {
    if (
      length(rest) < 2L || !rest[1] %in% allowed || !is.null(options[[rest[1]]])
    ) {
      release_stop(usage)
    }
    options[[rest[1]]] <- rest[2]
    rest <- rest[-c(1, 2)]
  }
  records <- release_records(repo_dir, options[["--tag"]])
  for (record in records) {
    cat(
      record[["Data-Release"]],
      record[["Asset-Name"]],
      record[["Asset-Bytes"]],
      "bytes\n"
    )
  }
  if (mode == "preview") {
    return(invisible(records))
  }
  if (is.null(options[["--work-dir"]])) {
    release_stop("Supply --work-dir outside the checkout.")
  }
  work_dir <- release_work_dir(options[["--work-dir"]], repo_dir)
  commit <- options[["--commit"]]
  if (!is.null(commit) && !grepl("^[0-9a-f]{40}$", commit)) {
    release_stop("Supply a full 40-character commit SHA.")
  }
  latest <- if (mode %in% c("publish", "verify")) {
    release_api("releases/latest")$tag_name
  } else {
    NULL
  }
  if (!is.null(latest) && !grepl("^v[0-9]+[.][0-9]+[.][0-9]+$", latest)) {
    release_stop("Latest release is not a stable package release.")
  }
  if (mode == "publish") {
    if (is.null(commit)) {
      release_stop("Publishing requires --commit SHA.")
    }
    git <- function(args) {
      paste(
        system2("git", shQuote(c("-C", repo_dir, args)), stdout = TRUE),
        collapse = "\n"
      )
    }
    if (
      !identical(git(c("rev-parse", "HEAD")), commit) ||
        nzchar(git(c("status", "--porcelain")))
    ) {
      release_stop("Publishing requires a clean checkout at --commit.")
    }
    if (!identical(release_api(paste0("commits/", commit))$sha, commit)) {
      release_stop("Commit is not available remotely.")
    }
    if (!isTRUE(release_api("immutable-releases")$enabled)) {
      release_stop("Enable repository release immutability before publishing.")
    }
    # Validate the entire batch before the first write to GitHub.
    for (record in records) {
      directory <- file.path(work_dir, "stage", record[["Data-Release"]])
      release_validate_file(
        file.path(directory, record[["Asset-Name"]]),
        record
      )
      release_manifest_matches(file.path(directory, "manifest.dcf"), record)
    }
  }
  for (record in records) {
    switch(
      mode,
      stage = release_stage(record, work_dir, options[["--source-release"]]),
      publish = release_publish(record, work_dir, commit),
      verify = release_verify(record, work_dir, commit)
    )
    if (
      !is.null(latest) &&
        !identical(release_api("releases/latest")$tag_name, latest)
    ) {
      release_stop(
        "GitHub's latest package release changed during this operation."
      )
    }
  }
  invisible(records)
}

if (sys.nframe() == 0L) {
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
  )
  repo_dir <- dirname(dirname(normalizePath(script)))
  source(file.path(repo_dir, "scripts", "data-release-helpers.R"))
  release_data_main(commandArgs(TRUE), repo_dir)
}
