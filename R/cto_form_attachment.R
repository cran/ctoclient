#' Download Attachments from a SurveyCTO Form
#'
#' @description
#' Downloads files attached to a deployed SurveyCTO form, such as preloaded
#' CSV files, media assets, or other server-side attachments.
#'
#'
#' @param form_id A string specifying the SurveyCTO form ID.
#' @param filename Optional character vector of specific filenames to download
#'   (e.g., `"prices.csv"`). If `NULL` (default), all available attachments
#'   associated with the form are downloaded.
#' @param dir A string giving the directory where files will be saved.
#'   Defaults to [getwd()].
#' @param overwrite Logical; if `TRUE`, existing files in `dir` will be
#'   overwritten. If `FALSE` (the default), existing files are skipped.
#'
#' @details
#' This function first calls [cto_form_metadata()] to retrieve metadata for
#' the deployed form, including the list of available attachments.
#'
#' * **File types**: Any attached file type can be downloaded (for example,
#'   images, audio, or CSV files).
#' * **Progress reporting**: When `options(scto.verbose = TRUE)` (default),
#'   progress messages are printed using the CLI framework.
#' * **Caching**: Files are not re-downloaded if they already exist in `dir`
#'   unless `overwrite = TRUE`.
#'
#' If all requested files are not available, the function aborts with
#' an informative message suggesting how to inspect the form metadata.
#'
#' @return
#' A character vector of file paths to all available attachments that exist
#' locally after the function completes (invisibly).
#'
#' Returns `invisible(NULL)` if the form has no attachments.
#'
#' @family Form Management Functions
#'
#' @export
#'
#' @examples
#' \dontrun{
# 1. Download all attachments
#' files <- cto_form_attachment("household_survey")
#'
#' # 2. Download specific files to a local directory
#' cto_form_attachment(
#'   form_id  = "household_survey",
#'   filename = c("item_list.csv", "logo.png"),
#'   dir      = "data/raw"
#' )
#'
#' # 3. Force re-download of a file
#' p <- cto_form_attachment(
#'   form_id  = "household_survey",
#'   filename = "prices.csv",
#'   overwrite = TRUE
#' )
#'
#' prices <- read.csv(p)
#' }

cto_form_attachment <- function(
  form_id,
  filename = NULL,
  dir = getwd(),
  overwrite = FALSE
) {
  verbose <- get_verbose()
  session <- get_session()

  assert_directory(dir)
  assert_flag(overwrite)
  assert_character(filename, null.ok = TRUE)

  if (verbose) {
    cli_progress_step("Checking available attachments")
  }

  metadata <- cto_form_metadata(form_id)
  media_files <- purrr::pluck(metadata, "deployedGroupFiles", "mediaFiles")
  if (length(media_files) == 0) {
    cli_warn("No attachments found for {.val {form_id}}")
    return(invisible())
  }

  files <- names(media_files)

  if (!is.null(filename)) {
    files <- files[files %in% filename]
    if (length(files) < length(filename)) {
      cli_warn(
        "These files were not found: {.val {filename[!(filename %in% files)]}}"
      )
    }
  }
  if (length(files) == 0) {
    cli_abort(c(
      "x" = "All requested attachment not found",
      "i" = "Use {.fn cto_form_metadata} to see all available attachments"
    ))
  }

  paths_all <- file.path(dir, files)
  to_download <- if (overwrite) {
    rep(TRUE, length(files))
  } else {
    !file.exists(paths_all)
  }
  urls <- purrr::map_chr(files, ~ purrr::pluck(media_files, .x, "downloadLink"))
  urls_to_fetch <- urls[to_download]
  paths_to_fetch <- paths_all[to_download]

  if (any(i <- grepl("^/forms", urls_to_fetch))) {
    urls_to_fetch[i] <- paste0(httr2::req_get_url(session), urls_to_fetch[i])
  }

  if (sum(to_download) > 0) {
    if (verbose) {
      cli_progress_step("Downloading {.val {sum(to_download)}} attachment{?s}")
    }

    reqs <- purrr::map(urls_to_fetch, ~ req_url(session, .x))
    purrr::walk2(
      reqs,
      paths_to_fetch,
      function(r, p) {
        tryCatch(
          {
            resp <- req_perform(r)
            writeBin(httr2::resp_body_raw(resp), p)
          },
          error = function(e) {
            cli_warn("{.val {basename(p)}}: {conditionMessage(e)}")
          }
        )
      }
    )
  }

  invisible(paths_all[file.exists(paths_all)])
}
