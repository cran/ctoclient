#' Download SurveyCTO Form Files and Templates
#'
#' @description
#' These functions retrieve auxiliary files and templates associated with a
#' deployed SurveyCTO form. All these functions require a stateful session to work.
#'
#' * [cto_form_languages()] retrieves the list of languages defined in the form.
#' * [cto_form_stata_template()] downloads a Stata `.do` file template for
#'   importing submitted data.
#' * [cto_form_printable()] downloads a printable (HTML) version of the form
#'   definition.
#' * [cto_form_mail_template()] downloads a mail merge template for the form.
#'
#' All downloads are saved locally and their file paths are returned invisibly.
#'
#' @param form_id A character string giving the unique SurveyCTO form ID.
#' @param dir A character string specifying the directory where downloaded files
#'   will be saved. Defaults to the current working directory.
#' @param lang Optional character string giving the language identifier
#'   (for example, `"English"`). If `NULL`, the form's default language is used.
#' @param csv_dir Optional character string giving the directory where the CSV
#'   dataset will eventually be stored. This value is embedded in the generated
#'   Stata `.do` file to automate data loading.
#' @param dta_dir Optional character string giving the directory where the Stata
#'   `.dta` file should be written by the template.
#' @param relevancies Logical; if `TRUE`, relevance logic (skip patterns) is
#'   included in the printable form. Defaults to `FALSE`.
#' @param constraints Logical; if `TRUE`, constraint logic is included in the
#'   printable form. Defaults to `FALSE`.
#' @param type Integer (0–2) specifying the format of the mail merge template:
#'   \itemize{
#'     \item \code{0}: Field names only.
#'     \item \code{1}: Field labels only.
#'     \item \code{2}: Both field names and labels.
#'   }
#' @param group_names Logical; if `TRUE`, group names are included in variable
#'   headers. Defaults to `FALSE`.
#'
#' @return
#' * **`cto_form_languages()`** returns a list containing the available languages
#'   and the index of the default language (1-based).
#' * All other functions return the local file path of the downloaded file,
#'   invisibly.
#'
#' @family Form Management Functions
#'
#' @examples
#' \dontrun{
#' form <- "household_survey"
#'
#' # 1. List available form languages
#' langs <- cto_form_languages(form)
#' print(langs)
#'
#' # 2. Download a Stata import template
#' # Provide future CSV/DTA locations so the .do file is ready to run
#' cto_form_stata_template(
#'   form_id = form,
#'   dir     = "downloads/",
#'   csv_dir = "C:/Data",
#'   dta_dir = "C:/Data"
#' )
#'
#' # 3. Download a printable form with logic displayed
#' cto_form_printable(
#'   form_id      = form,
#'   dir          = "documentation/",
#'   relevancies  = TRUE,
#'   constraints  = TRUE
#' )
#'
#' # 4. Download a mail-merge template
#' cto_form_mail_template(
#'   form_id = form,
#'   dir     = "templates/",
#'   type    = 2
#' )
#' }

cto_form_languages <- function(form_id) {
  confirm_cookies()
  session <- get_session()
  assert_form_id(form_id)

  url_path <- str_glue("forms/{form_id}/languages")
  session <- req_url_query(session, t = as.double(Sys.time()) * 1000)

  if (get_verbose()) {
    cli_progress_step(
      "Reading {.val {form_id}} form languages",
      "Read {.val {form_id}} form languages"
    )
  }

  resp <- fetch_api_response(session, url_path)
  if (!is.null(resp$defaultIndex)) {
    resp$defaultIndex <- resp$defaultIndex + 1
  }
  class(resp) <- "simple.list"
  resp
}

#' @export
#' @rdname cto_form_languages
cto_form_stata_template <- function(
  form_id,
  dir = getwd(),
  lang = NULL,
  csv_dir = NULL,
  dta_dir = NULL
) {
  confirm_cookies()
  session <- get_session()

  assert_directory(dir)
  assert_string(lang, null.ok = TRUE)
  assert_string(csv_dir, null.ok = TRUE)
  assert_string(dta_dir, null.ok = TRUE)
  assert_form_id(form_id)

  query <- list(
    dateTimeFormat = "MDY",
    repeat_option = 1,
    lang = lang,
    csvPath = csv_dir,
    stataPath = dta_dir,
    t = as.double(Sys.time()) * 1000
  )

  query <- drop_nulls_recursive(query)
  url_path <- str_glue("forms/{form_id}/stata-template")

  if (get_verbose()) {
    cli_progress_step(
      "Downloading {.val {form_id}} Stata template",
      "Downloaded {.val {form_id}} Stata template"
    )
  }
  resp <- fetch_api_response(req_url_query(session, !!!query), url_path)

  url <- resp[["url"]]
  path <- file.path(dir, basename(url))
  fetch_api_response(session, url, path)
  invisible(path)
}


#' @export
#' @rdname cto_form_languages
cto_form_printable <- function(
  form_id,
  dir = getwd(),
  lang = NULL,
  relevancies = FALSE,
  constraints = FALSE
) {
  confirm_cookies()
  session <- get_session()

  assert_directory(dir)
  assert_string(lang, null.ok = TRUE)
  assert_flag(relevancies)
  assert_flag(constraints)

  assert_form_id(form_id)

  query <- list(
    lang = lang,
    relevancies = if (relevancies) "on" else NULL,
    constraints = if (constraints) "on" else NULL,
    download = 1,
    t = as.double(Sys.time()) * 1000
  )

  query <- drop_nulls_recursive(query)
  url_path <- str_glue("forms/{form_id}/printable")

  if (get_verbose()) {
    cli_progress_step(
      "Downloading {.val {form_id}} printable",
      "Downloaded {.val {form_id}} printable"
    )
  }
  resp <- fetch_api_response(req_url_query(session, !!!query), url_path)

  url <- resp[["url"]]
  path <- file.path(dir, basename(url))
  fetch_api_response(session, url, path)
  invisible(path)
}


#' @export
#' @rdname cto_form_languages
cto_form_mail_template <- function(
  form_id,
  dir = getwd(),
  type = 2,
  group_names = FALSE
) {
  confirm_cookies()
  session <- get_session()

  assert_directory(dir)
  assert_flag(group_names)
  checkmate::assert_number(type, lower = 0, upper = 2)
  type <- floor(type)

  assert_form_id(form_id)

  query <- list(
    type = type,
    groupnames = group_names,
    t = as.double(Sys.time()) * 1000
  )

  query <- drop_nulls_recursive(query)
  url_path <- str_glue("forms/{form_id}/mail-merge-template")

  if (get_verbose()) {
    cli_progress_step(
      "Downloading {.val {form_id}} mail merge template",
      "Downloaded {.val {form_id}} mail merge template"
    )
  }
  resp <- fetch_api_response(req_url_query(session, !!!query), url_path)

  url <- resp[["url"]]
  path <- file.path(dir, basename(url))
  fetch_api_response(session, url, path)
  invisible(path)
}
