#' Download SurveyCTO Form Metadata and Definitions
#'
#' @description
#' Functions for interacting with SurveyCTO form definitions.
#'
#' * `cto_form_metadata()` retrieves raw metadata for a form, including
#'   available definition files, version identifiers, and download URLs.
#' * `cto_form_definition()` downloads a specific XLSForm definition (Excel file)
#'   to a local directory.
#'
#' @param form_id A string giving the unique SurveyCTO form ID.
#' @param version Optional string specifying a particular form version to download.
#'   If `NULL` (default), the currently deployed version is used.
#' @param dir Directory where the XLSForm should be saved. Defaults to `getwd()`.
#' @param overwrite Logical; if `TRUE`, an existing file in `dir` will be
#'   overwritten. If `FALSE` (default), the existing file is used.
#'
#' @return
#' * `cto_form_metadata()` returns a list containing the metadata, including
#'   keys for `deployedGroupFiles` and `previousDefinitionFiles`.
#' * `cto_form_definition()` returns a character string with the path to the downloaded Excel file.
#'
#' @details
#' * **Version Handling:** When `version` is supplied, it is validated against
#'   the available versions from `cto_form_metadata()`. An informative error is raised
#'   if the requested version does not exist.
#' * **Caching:** If the file already exists in `dir`, it will not be re-downloaded
#'   unless `overwrite = TRUE`.
#'
#' @export
#'
#' @family Form Management Functions
#'
#' @examples
#' \dontrun{
#' # --- 1. Get raw metadata ---
#' meta <- cto_form_metadata("household_survey")
#'
#' # --- 2. Download the current form definition ---
#' file_path <- cto_form_definition("household_survey")
#'
#' # --- 3. Download a specific historical version ---
#' file_path_v <- cto_form_definition(
#'   "household_survey",
#'   version = "20231001"
#' )
#'
#' # --- 4. Read XLSForm manually with readxl ---
#' library(readxl)
#' survey <- read_excel(file_path, sheet = "survey")
#' choices <- read_excel(file_path, sheet = "choices")
#' settings <- read_excel(file_path, sheet = "settings")
#' }
cto_form_metadata <- function(form_id) {
  confirm_cookies()
  session <- get_session()
  assert_form_id(form_id)

  url_path <- str_glue("forms/{form_id}/files")
  session <- req_url_query(session, t = as.numeric(Sys.time()) * 1000)

  if (get_verbose()) {
    cli_progress_step(
      "Reading {col_blue(form_id)} form metadata",
      "Read {col_blue(form_id)} form metadata"
    )
  }
  fetch_api_response(session, url_path)
}


#' @export
#' @rdname cto_form_metadata
cto_form_definition <- function(
  form_id,
  version = NULL,
  dir = getwd(),
  overwrite = FALSE
) {
  confirm_cookies()
  session <- get_session()

  assert_string(version, null.ok = TRUE)
  assert_directory(dir)
  assert_flag(overwrite)

  metadata <- cto_form_metadata(form_id)
  df_versions <- dplyr::bind_rows(
    purrr::pluck(metadata, "deployedGroupFiles", "definitionFile"),
    purrr::pluck(metadata, "previousDefinitionFiles")
  )

  if (!is.null(version)) {
    df <- dplyr::filter(df_versions, .data$formVersion == version)
    if (nrow(df) == 0 && nrow(df_versions) > 0) {
      cli_abort(c(
        "x" = "{col_blue(form_id)} doesn't have the specified form version: {.val {version}}",
        "i" = "Use {.run ctoclient::cto_form_metadata()} to see available form versions"
      ))
    }
  } else {
    df <- df_versions[1, ]
    version <- df$formVersion[1]
  }

  file_path <- file.path(dir, basename(df$filename[1]))

  if (!file.exists(file_path) || overwrite) {
    url <- df$downloadLink[1]
    if (get_verbose()) {
      cli_progress_step(
        "Downloading form definition version {.val {version}}",
        "Downloaded form definition version {.val {version}}"
      )
    }
    fetch_api_response(req_url(session, url), file_path = file_path)
  } else {
    cli_inform(c(
      v = "{.val {basename(file_path)}} already exist in the directory"
    ))
  }

  invisible(file_path)
}
