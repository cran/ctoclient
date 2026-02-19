#' Download Attachments from SurveyCTO Form Data
#'
#' @description
#' Extracts attachment URLs (images, audio, video, signatures) from SurveyCTO form data
#' and downloads the files to a local directory. This function handles encrypted forms
#' if a private key is provided.
#'
#' @param form_id A string specifying the SurveyCTO form ID to inspect.
#' @param fields A `tidy-select` expression (e.g., `everything()`, `starts_with("img_")`)
#'   specifying which columns should be scanned for attachment URLs. Defaults to `everything()`.
#' @param private_key Optional. A character string specifying the path to a local
#'   RSA private key file. Required if the form is encrypted.
#' @param dir A character string specifying the local directory where files should be saved.
#'   Defaults to `"media"`. The directory must exist.
#' @param overwrite Logical. If `TRUE`, existing files with the same name in `dir`
#'   will be overwritten. If `FALSE` (the default), existing files are skipped.
#'
#' @details
#' This function performs the following steps:
#' 1. Fetches the form data using `cto_form_data`.
#' 2. Scans the selected `fields` for values matching the standard SurveyCTO API
#'    attachment URL pattern.
#' 3. Downloads the identified files sequentially to the specified `dir`.
#'
#' @return Returns a vector of file paths (invisibly). The function is called for its side effect
#'   of downloading files to the local disk.
#'
#' @family Form Management Functions
#'
#' @examples
#' \dontrun{
#' # 1. Download all attachments from the form submissions
#' cto_form_data_attachment(
#'   form_id = "household_survey_v1",
#'   dir = "downloads/medias"
#' )
#'
#' # 2. Download only specific image fields from an encrypted form
#' cto_form_data_attachment(
#'   form_id = "encrypted_health_survey",
#'   fields = starts_with("image_"),
#'   private_key = "keys/my_priv_key.pem",
#'   overwrite = TRUE
#' )
#' }
cto_form_data_attachment <- function(
  form_id,
  fields = everything(),
  private_key = NULL,
  dir = file.path(getwd(), "media"),
  overwrite = FALSE
) {
  verbose <- get_verbose()
  assert_directory(dir)
  assert_flag(overwrite)
  session <- get_session()

  rgx <- "^https://.*\\.surveycto\\.com/api/v2/forms/.*/submissions/uuid:.*/attachments/.*\\.*$"

  df <- cto_form_data(form_id, private_key = private_key, tidy = FALSE)

  if (length(df) == 0) {
    cli_warn("There is no submission for {col_blue(form_id)} form.")
    return(invisible())
  }

  urls <- df |>
    dplyr::select({{ fields }}) |>
    dplyr::select(dplyr::where(is.character)) |>
    dplyr::select(dplyr::where(~ any(grepl(rgx, .x), na.rm = TRUE))) |>
    unlist(use.names = FALSE)

  urls <- urls[grepl(rgx, urls)]

  if (length(urls) == 0) {
    if (is.null(private_key)) {
      cli_warn(c(
        "No submission attachments found.",
        "Did you forget to provide `private_key`?"
      ))
    } else {
      cli_warn("No submission attachments found.")
    }
    return(invisible())
  }

  file_paths <- file.path(dir, basename(urls))
  to_download <- if (overwrite) {
    rep(TRUE, length(urls))
  } else {
    !file.exists(file_paths)
  }

  urls_to_fetch <- urls[to_download]
  paths_to_fetch <- file_paths[to_download]

  if (length(paths_to_fetch) > 0) {
    if (verbose) {
      cli_progress_step(
        "Downloading {.val {length(paths_to_fetch)}} attachment{?s}"
      )
    }
    if (!is.null(private_key)) {
      session <- httr2::req_body_multipart(
        session,
        private_key = curl::form_file(private_key)
      )
    }
    reqs <- purrr::map(urls_to_fetch, ~ req_url(session, .x))

    purrr::walk2(
      reqs,
      paths_to_fetch,
      function(r, p) {
        tryCatch(
          fetch_api_response(r, NULL, p),
          error = function(e) {
            cli_warn("{col_blue(basename(p))}: {conditionMessage(e)}")
          }
        )
      }
    )
  }

  invisible(file_paths[file.exists(file_paths)])
}
