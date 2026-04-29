# ============================================================================
# PSYC 434 — Lab 9 cache helper
# ----------------------------------------------------------------------------
# this file provides one function, load_lab_09_cache(), that:
#   1. downloads a small zip of pre-fitted causal-forest objects from
#      Google Drive (so students do not wait on training during the lab),
#   2. unzips it into a per-user cache directory,
#   3. loads the .qs artefacts and returns them in a named list.
#
# the lab script (scripts/lab-09.R) sources this file and calls
# load_lab_09_cache() before any analysis.
#
# the cache is built by scripts/fit-lab-09-cache.R (run once on Joseph's
# machine; output saved to Google Drive).
# ============================================================================

# paste the Google Drive share URL of lab-09-cache.zip below after running
# the fit-cache script and marking the file "Anyone with the link can view".
# expected form:
#   https://drive.google.com/file/d/<FILE_ID>/view?usp=sharing
LAB_09_CACHE_URL <- "https://drive.google.com/file/d/1LUcCIiY_w03Y6SInXX-RvYrKQrO2HKJi/view?usp=sharing"
# --- helper: extract the file ID from a Google Drive share URL ------------

.gdrive_file_id <- function(share_url) {
  if (grepl("/drive/folders/", share_url)) {
    stop(
      "the URL points at a Google Drive *folder*, not a *file*:\n  ", share_url,
      "\nopen Drive in a browser, right-click the lab-09-cache.zip file ",
      "(not the parent folder), choose Get link > Anyone with the link > Viewer, ",
      "and paste THAT URL. it should look like\n",
      "  https://drive.google.com/file/d/<FILE_ID>/view?usp=sharing"
    )
  }
  m <- regmatches(share_url, regexec("/d/([^/]+)/", share_url))[[1]]
  if (length(m) < 2 || nchar(m[2]) == 0) {
    stop(
      "could not extract file ID from URL:\n  ", share_url,
      "\nexpected form: https://drive.google.com/file/d/<FILE_ID>/view?usp=sharing"
    )
  }
  m[2]
}

# --- helper: download a Drive file robustly --------------------------------

# the simple `uc?export=download&id=...` URL fails for files large enough to
# trigger Google's virus-scan confirmation page (~25-100 MB depending on
# day). googledrive::drive_download handles the confirmation dance for
# public files when called after drive_deauth().
.gdrive_download <- function(file_id, dest) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    install.packages("googledrive")
  }
  suppressMessages(googledrive::drive_deauth())
  googledrive::drive_download(
    googledrive::as_id(file_id),
    path = dest,
    overwrite = TRUE
  )
  invisible(dest)
}

# --- main entry point -------------------------------------------------------

load_lab_09_cache <- function(
  url = LAB_09_CACHE_URL,
  cache_dir = tools::R_user_dir("psyc434", which = "cache"),
  refresh = FALSE
) {
  if (!requireNamespace("qs", quietly = TRUE)) install.packages("qs")

  expected_files <- c(
    "models_binary.qs",
    "policy_tree_stability.qs",
    "policy_workflow.qs"
  )

  # if Joseph's local Google Drive cache is mounted (i.e., this is the
  # author's machine), prefer those files. avoids the lag while Drive
  # syncs replacement uploads to the cloud.
  local_drive <- path.expand(file.path(
    "~/Library/CloudStorage/GoogleDrive-joseph.bulbulia@gmail.com",
    "My Drive/courses/psyc-434-2026/lab-09-cache"
  ))
  if (all(file.exists(file.path(local_drive, expected_files)))) {
    return(list(
      models_binary = qs::qread(file.path(local_drive, "models_binary.qs")),
      policy_tree_stability = qs::qread(file.path(local_drive, "policy_tree_stability.qs")),
      policy_workflow = qs::qread(file.path(local_drive, "policy_workflow.qs")),
      cache_dir = local_drive
    ))
  }

  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  on_disk <- file.path(cache_dir, expected_files)
  needs_download <- refresh || !all(file.exists(on_disk))

  if (needs_download) {
    if (identical(url, "<paste-share-url-here>") || nchar(url) == 0) {
      stop(
        "no cache URL configured. open scripts/lab-09-cache.R and paste the\n",
        "Google Drive share URL into LAB_09_CACHE_URL near the top of the file."
      )
    }
    file_id <- .gdrive_file_id(url)
    zip_path <- file.path(cache_dir, "lab-09-cache.zip")
    message("downloading lab 9 cache (about 80 MB) ...")
    .gdrive_download(file_id, zip_path)
    message("unzipping ...")
    utils::unzip(zip_path, exdir = cache_dir, overwrite = TRUE)
    file.remove(zip_path)
  }

  missing <- expected_files[!file.exists(on_disk)]
  if (length(missing) > 0) {
    stop(
      "cache download finished but the following files are missing:\n  ",
      paste(missing, collapse = "\n  "),
      "\nthe download may have hit Google Drive's virus-scan page; ",
      "rebuild the cache so the zip stays under 100 MB."
    )
  }

  list(
    models_binary = qs::qread(file.path(cache_dir, "models_binary.qs")),
    policy_tree_stability = qs::qread(file.path(cache_dir, "policy_tree_stability.qs")),
    policy_workflow = qs::qread(file.path(cache_dir, "policy_workflow.qs")),
    cache_dir = cache_dir
  )
}
