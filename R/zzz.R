#' @importFrom cli cli_abort cli_warn cli_inform col_blue cli_progress_step
#' @importFrom stringr str_c str_glue str_extract str_squish str_replace_all str_remove_all
#' @importFrom httr2 req_url req_url_path req_url_query req_perform resp_body_json resp_body_raw
#' @importFrom checkmate assert_string assert_flag assert_character assert_directory
#' @importFrom rlang `:=` .data
#' @importFrom dplyr mutate select across
#' @importFrom tidyr matches all_of any_of everything
NULL

# Environment to store ----
.ctoclient_env <- new.env(parent = emptyenv())

# Get session ----
get_session <- function() {
  if (!cto_is_connected()) {
    cli_abort(c(
      x = "No active SurveyCTO session found.",
      i = "Please connect first using {.run ctoclient::cto_connect()}.",
      i = "Or set connection using {.run ctoclient::cto_set_connection()}."
    ))
  } else {
    session <- get(".session", envir = .ctoclient_env)
    if (!inherits(session, "cto_session")) {
      cli_abort(
        "Invalid session found. Please reconnect using {.run ctoclient::cto_connect()}."
      )
    }
    return(session)
  }
}

# Get verbose ----
get_verbose <- function() isTRUE(getOption("ctoclient.verbose", TRUE))

# Confirm Cookies ----
# TODO: Include in all non-API endpoints
confirm_cookies <- function() {
  session <- get_session()
  has_cookie <- !is.null(session$options$cookiefile)
  if (!has_cookie) {
    cli_abort(c(
      x = "For this function to work you need to use cookies.",
      i = "Please restart you connection with cookies {.run cto_connect(cookies = TRUE)}"
    ))
  }
  invisible(has_cookie)
}


# Drop NULL list ----
drop_nulls_recursive <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- lapply(x, drop_nulls_recursive)
  Filter(function(z) !is.null(z) && !(is.list(z) && length(z) == 0), x)
}

# Assert form IDs ----
assert_form_id <- function(form_id) {
  checkmate::assert_string(form_id)
  form_ids <- fetch_api_response(get_session(), "api/v2/forms/ids")
  if (!(form_id %in% form_ids)) {
    cli_abort(c(
      x = "There is no form with {.val {form_id}} ID",
      i = "Use {.run ctoclient::cto_form_ids()} to see available form IDs"
    ))
  }
  invisible(TRUE)
}


# Fetch API response ----
fetch_api_response <- function(req, url_path = NULL, file_path = NULL) {
  if (!is.null(url_path)) {
    req <- req_url_path(req, url_path)
  }

  if (is.null(file_path)) {
    req_perform(req) |>
      resp_body_json(
        simplifyVector = TRUE,
        flatten = TRUE
      )
  } else {
    req_perform(req) |>
      httr2::resp_body_raw() |>
      writeBin(file_path)
  }
}

# Fetch paginated json ----
fetch_paginated_response <- function(req, path, field = "data") {
  resp <- fetch_api_response(req, path)
  out <- purrr::pluck(resp, field)
  cursor <- purrr::pluck(resp, "nextCursor")
  while (!is.null(cursor)) {
    req <- req_url_query(req, cursor = cursor)
    resp <- fetch_api_response(req, path)
    out <- dplyr::bind_rows(out, purrr::pluck(resp, field))
    cursor <- purrr::pluck(resp, "nextCursor")
  }
  return(out)
}

# Center text -----
center_text <- function(text, fill = " ", width = 78) {
  checkmate::assert_string(fill)
  if (nchar(text) < width) {
    padding_total <- width - nchar(text)
    left_pad <- floor(padding_total / 2)
    right_pad <- ceiling(padding_total / 2)
    paste0(strrep(fill, left_pad), text, strrep(fill, right_pad))
  } else {
    text
  }
}

# Generate regex variable name ----
gen_regex_varname <- function(name, rpt_lvl, multi, mp = "_*[0-9]+") {
  if (rpt_lvl == 0) {
    if (multi) {
      return(paste0("^", name, mp, "$"))
    } else {
      return(paste0("^", name, "$"))
    }
  } else {
    rpt <- strrep("_[0-9]+", rpt_lvl)
    if (multi) {
      return(paste0("^", name, mp, rpt, "$"))
    } else {
      return(paste0("^", name, rpt, "$"))
    }
  }
}
