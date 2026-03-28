#' Download and Tidy SurveyCTO Form Data
#'
#' @description
#' Downloads submission data from a SurveyCTO server in wide JSON format.
#' Encrypted forms are supported via a private key. When `tidy = TRUE`
#' (default), the function uses the form's XLSForm definition to convert
#' variables to appropriate R types, drop structural fields, and organize
#' columns for analysis.
#'
#' @param form_id A string specifying the SurveyCTO form ID.
#' @param private_key An optional path to a `.pem` private key file. Required
#'   if the form is encrypted.
#' @param start_date A POSIXct timestamp. Only submissions received after
#'   this date/time are requested. Defaults to `"2000-01-01"`.
#' @param status A character vector of submission statuses to include.
#'   Must be a subset of `"approved"`, `"rejected"`, and `"pending"`.
#'   Defaults to all three.
#' @param tidy Logical; if `TRUE`, attempts to clean and restructure the raw
#'   SurveyCTO output using the XLSForm definition.
#'
#' @details
#' When `tidy = TRUE`, the function performs several common post-processing
#' steps:
#'
#' \itemize{
#'   \item **Type conversion:** Converts numeric, date, and datetime fields
#'   to native R types based on question types in the XLSForm.
#'   \item **Structural cleanup:** Removes layout-only fields such as notes,
#'   group markers, and repeat delimiters.
#'   \item **Column ordering:** Places key submission metadata (for example,
#'   completion and submission dates) first, followed by survey variables
#'   in form order.
#'   \item **Media fields:** Strips URLs from image, audio, and video fields,
#'   leaving only the filename.
#'   \item **Geopoints:** Splits geopoint variables into four columns with
#'   `_latitude`, `_longitude`, `_altitude`, and `_accuracy` suffixes when
#'   not already present.
#' }
#'
#' @return
#' A `data.frame` containing the downloaded submissions.
#'
#' If `tidy = FALSE`, the raw parsed JSON response is returned.
#' If `tidy = TRUE`, a cleaned version with standardized column types and
#' ordering is returned.
#'
#' Returns an empty `data.frame` when no submissions are available.
#'
#' @family Form Management Functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download raw submissions
#' raw <- cto_form_data("my_form_id", tidy = FALSE)
#'
#' # Download and tidy encrypted data
#' clean <- cto_form_data("my_form_id", private_key = "keys/my_key.pem")
#' }

cto_form_data <- function(
  form_id,
  private_key = NULL,
  start_date = as.POSIXct("2000-01-01"),
  status = c("approved", "rejected", "pending"),
  tidy = TRUE
) {
  verbose <- get_verbose()

  assert_form_id(form_id)
  if (!is.null(private_key)) {
    checkmate::assert_file_exists(private_key, "r", "pem")
  }
  checkmate::assert_class(start_date, "POSIXct")
  assert_flag(tidy)
  if (tidy) {
    confirm_cookies()
  }

  status <- match.arg(status, several.ok = TRUE)
  start_date <- as.numeric(start_date)
  session <- get_session()

  url_path <- str_glue("api/v2/forms/data/wide/json/{form_id}")
  session <- req_url_query(
    session,
    date = start_date,
    r = status,
    .multi = "pipe"
  )

  if (!is.null(private_key)) {
    session <- httr2::req_body_multipart(
      session,
      private_key = curl::form_file(private_key)
    )
  }

  if (verbose) {
    cli_progress_step(
      "Fetching {col_blue(form_id)} form data",
      "Fetched {col_blue(form_id)} form data"
    )
  }
  raw_data <- fetch_api_response(session, url_path)

  if (length(raw_data) == 0) {
    cli_warn("No submissions were found for {col_blue(form_id)} form")
    return(raw_data)
  }

  if (!tidy) return(raw_data)

  if (verbose) {
    cli_progress_step(
      "Tidying {col_blue(form_id)} form data",
      "Tidied {col_blue(form_id)} form data"
    )
  }

  # Process the data
  tidy_data <- raw_data

  fp <- tryCatch({
    cto_form_definition(form_id, dir = tempdir(), overwrite = TRUE)
  },
  error = function(e) {
    message(paste("Failed to download XLSForm:", conditionMessage(e)))
  })

  survey <- tryCatch({
    readxl::read_excel(fp, sheet = "survey") |>
      mutate(
        type = str_squish(.data$type),
        name = str_squish(.data$name),
        repeat_level = purrr::accumulate(
          .data$type,
          .init = 0,
          .f = function(i, x) {
            if (grepl("begin repeat", x, TRUE)) {
              i + 1
            } else if (grepl("end repeat", x, TRUE)) {
              i - 1
            } else {
              i
            }
          }
        )[-1],
        is_repeat = .data$repeat_level > 0,
        is_gps = grepl("^geopoint", .data$type, TRUE),
        is_numeric = grepl(
          "^select_one|^integer|^decimal|^sensor_",
          .data$type,
          TRUE
        ),
        is_slt_multi = grepl("^select_multiple", .data$type, TRUE),
        is_date = grepl("^date|^today", .data$type, TRUE),
        is_datetime = grepl("^datetime|^start|^end$", .data$type, TRUE),
        is_null_fields = grepl(
          "^note|^begin group|^end group|^end repeat",
          .data$type,
          TRUE
        ),
        is_media = grepl(
          "^image$|^audio$|^video$|^file|^text audit|^audio audit",
          .data$type,
          TRUE
        ),
        regex_varname = purrr::pmap_chr(
          list(.data$name, .data$repeat_level, .data$is_slt_multi),
          \(n, r, m) gen_regex_varname(n, r, m)
        ),
        regex_varname = ifelse(
          grepl("^begin repeat", .data$type, TRUE),
          stringr::str_replace(.data$regex_varname, r"(\[0-9\]\+\$)", "count"),
          .data$regex_varname
        ),
        multi_select = ifelse(
          # For now handling only non-repeat fields
          .data$is_slt_multi & .data$repeat_level == 0,
          purrr::pmap_chr(
            list(.data$name, .data$repeat_level, .data$is_slt_multi, "_values"),
            \(n, r, m, mp) gen_regex_varname(n, r, m, mp)
          ), NA)
      )
  },
  error = function(e) {
    message(paste("Failed to parse XLSForm:", conditionMessage(e)))
  })

  tryCatch({
    cs_dates <- c("CompletionDate", "SubmissionDate")
    all_fields <- survey$regex_varname[!survey$is_null_fields]
    null_fields <- survey$regex_varname[survey$is_null_fields]
    numeric_fields <- survey$regex_varname[survey$is_numeric]
    multi_field <- survey$regex_varname[survey$is_slt_multi]
    date_fields <- survey$regex_varname[survey$is_date]
    datetime_fields <- c(cs_dates, survey$regex_varname[survey$is_datetime])
    media_fields <- survey$regex_varname[survey$is_media]
    gps_fields <- survey$regex_varname[survey$is_gps]
  }, error = function(e) {
    message(paste("Failed to categorize fields:", conditionMessage(e)))
  })

  multi_choices <- tryCatch({
    readxl::read_excel(fp, sheet = "choices") |>
      mutate(value = suppressWarnings(as.numeric(.data$value))) |>
      dplyr::filter(!is.na(.data$value)) |>
      mutate(value = str_replace_all(.data$value, "-", "_")) |>
      select("list_name", "value") |>
      dplyr::right_join(
        survey |>
          dplyr::filter(!is.na(.data$multi_select)) |>
          mutate(list_name = str_extract(.data$type, "\\S+$")) |>
          select("name", "list_name", "multi_select"),
        by = "list_name", relationship = "many-to-many") |>
      mutate(var_name = str_glue("{name}_{value}")) |>
      select("name", "var_name") |>
      tidyr::nest(.by = "name") |>
      purrr::pluck("data")
  }, error = function(e) {
    message(paste("Failed to prepare missing binary variables:", conditionMessage(e)))
  })

  tidy_data <- tryCatch({
    purrr::reduce(
      multi_choices,
      function(df, fields_tbl) {
        fields <- fields_tbl$var_name
        add_vars <- setdiff(fields, names(df))
        if (length(add_vars) == 0) return(df)
        new_cols <- purrr::set_names(
          replicate(length(add_vars), rep(NA, nrow(df)), simplify = FALSE),
          add_vars
        )
        dplyr::mutate(df, !!!new_cols)
      },
      .init = tidy_data
    )
  }, error = function(e) {
    message(paste("Failed to add missing binary variables:", conditionMessage(e)))
    tidy_data
  })


  columns_order <- tryCatch({
    purrr::map(
      all_fields,
      function(pattern) {
        matches <- grep(pattern, names(tidy_data), value = TRUE)
        if (length(matches) == 0) return(NULL)

        if (pattern %in% multi_field) {
          sorted_matches <- stringr::str_sort(matches, numeric = TRUE)
          standard <- sorted_matches[!grepl("__", sorted_matches)]
          special  <- sorted_matches[grepl("__", sorted_matches)]
          return(c(standard, special))
        } else {
          return(matches)
        }
      }) |>
      purrr::flatten_chr() |>
      unique()
  }, error = function(e) {
    message(paste("Failed to prepare column orders:", conditionMessage(e)))
  })

  tidy_data <- tryCatch({
    select(
      tidy_data,
      any_of(cs_dates),
      any_of(columns_order),
      everything()
    )
  }, error = function(e) {
    message(paste("Failed to order columns:", conditionMessage(e)))
    tidy_data
  })

  tidy_data <- tryCatch({
    if (length(null_fields) > 0) {
      select(tidy_data, !matches(null_fields))
    } else tidy_data
  }, error = function(e) {
    message(paste("Failed to drop null columns:", conditionMessage(e)))
    tidy_data
  })


  tidy_data <- tryCatch({
    if (length(multi_field) > 0) {
      purrr::reduce(
      multi_field,
      function(df, pattern) {
        cols <- grep(pattern, names(df), value = TRUE)
        if (length(cols) == 0) return(df)
        df |>
          dplyr::mutate(
            any_selected = dplyr::if_any(dplyr::all_of(cols), ~ .x == "1"),
            dplyr::across(
              dplyr::all_of(cols),
              ~ dplyr::if_else(.data$any_selected, dplyr::coalesce(.x, "0"), .x)
            )
          ) |>
          dplyr::select(!"any_selected")
      },
      .init = tidy_data
    )
    } else tidy_data
  }, error = function(e) {
    message(paste("Failed to replace 0 for binaries:", conditionMessage(e)))
    tidy_data
  })

  tidy_data <- tryCatch({
    mutate(
      tidy_data,
      across(
        matches(datetime_fields),
        ~ as.POSIXct(.x, format = "%B %d, %Y %I:%M:%S %p")
      )
    )
  }, error = function(e) {
    message(paste("Failed to parse datetime:", conditionMessage(e)))
    tidy_data
  })

  tidy_data <- tryCatch({
    if (length(numeric_fields) > 0) {
      mutate(
        tidy_data,
        across(matches(numeric_fields), as.numeric)
      )
    } else tidy_data
  }, error = function(e) {
    message(paste("Failed to convert numeric columns:", conditionMessage(e)))
    tidy_data
  })

  tidy_data <- tryCatch({
    if (length(date_fields) > 0) {
      mutate(
        tidy_data,
        across(matches(date_fields), ~ as.Date(.x, format = "%B %d, %Y"))
      )
    } else tidy_data
  }, error = function(e) {
    message(paste("Failed to parse date columns:", conditionMessage(e)))
    tidy_data
  })

  tidy_data <- tryCatch({
    if (length(media_fields) > 0) {
      mutate(
        tidy_data,
        across(
          matches(media_fields),
          ~ ifelse(grepl("^https", .x, TRUE), basename(.x), .x)
        )
      )
    } else tidy_data
  }, error = function(e) {
    message(paste("Failed to clean media columns:", conditionMessage(e)))
    tidy_data
  })

  tidy_data <- tryCatch({
    if (length(gps_fields) > 0) {
      nms <- names(tidy_data)
      suffix <- c("lat", "long", "alt", "acc")
      keep_idx <- sapply(gps_fields, function(pattern) {
        actual_col <- grep(pattern, nms, value = TRUE)
        if (length(actual_col) == 0) {
          return(FALSE)
        }
        check_fld <- paste0(actual_col[1], "_", suffix[1])
        !(check_fld %in% nms)
      })
      gps_fields <- gps_fields[keep_idx]
    }

    if (length(gps_fields) > 0) {
      tidyr::separate_wider_delim(
        data = tidy_data,
        cols = matches(gps_fields),
        delim = " ",
        names = c("lat", "long", "alt", "acc"),
        names_sep = "_",
        too_few = "align_start",
        cols_remove = FALSE
      )
    } else tidy_data
  }, error = function(e) {
    message(paste("Failed to split gps columns:", conditionMessage(e)))
    tidy_data
  })

  tidy_data <- tryCatch({
    mutate(
      tidy_data,
      across(is.character, readr::parse_guess)
    )
  }, error = function(e) {
    message(paste("Failed to guess column types:", conditionMessage(e)))
    tidy_data
  })


  return(tidy_data)
}
