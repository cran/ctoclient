#' Generate a Stata Do-File with Variable and Value Labels from a SurveyCTO Form
#'
#' @description
#' Creates a Stata `.do` file that applies variable labels, value labels, and
#' notes to a dataset based on the XLSForm definition of a SurveyCTO form.
#' The function supports multi-language forms, repeat groups, and
#' `select_multiple` questions, and generates Stata-compatible regular
#' expressions so labels are applied to all indexed variables.
#'
#' @param form_id A character string specifying the SurveyCTO form ID.
#' @param path Optional character string giving the output file path for the
#'   generated `.do` file. Must end in `.do`. If `NULL`, the file is not written
#'   to disk and the generated commands are returned invisibly.
#'
#' @details
#' The function performs several processing steps:
#'
#' \itemize{
#'   \item \strong{Language selection:} Automatically chooses the default
#'   language defined in the XLSForm, or falls back to English when multiple
#'   label columns are present.
#'   \item \strong{Value labels:} Generates Stata `label define` commands for
#'   all `select_one` choice lists and a binary label set for
#'   `select_multiple` variables.
#'   \item \strong{Repeat handling:} For variables inside repeat groups,
#'   Stata loops and regex matching are created so labels apply to all indexed
#'   copies (for example, `child_age_1`, `child_age_2`).
#'   \item \strong{Select-multiple expansion:} Produces conditional labeling
#'   logic for binary indicator variables derived from
#'   `select_multiple` questions.
#'   \item \strong{Label cleaning:} Removes HTML markup, escapes Stata-special
#'   characters, normalizes whitespace, and preserves SurveyCTO interpolation
#'   strings such as `${var}`.
#' }
#'
#' @return
#' A character vector containing the lines of the generated Stata `.do` file.
#' The value is returned invisibly.
#'
#' @family Form Management Functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a Stata do-file and write it to disk
#' cto_form_dofile("household_survey", path = "labels.do")
#'
#' # Generate without writing to a file
#' cmds <- cto_form_dofile("household_survey")
#' }

cto_form_dofile <- function(form_id, path = NULL) {
  verbose <- get_verbose()

  if (!is.null(path)) {
    checkmate::assert_path_for_output(path, TRUE, "do")
  }

  fp <- cto_form_definition(form_id, dir = tempdir(), overwrite = TRUE)
  form <- list(
    survey = readxl::read_excel(fp, sheet = "survey"),
    choices = readxl::read_excel(fp, sheet = "choices"),
    settings = readxl::read_excel(fp, sheet = "settings")
  )

  if (verbose) {
    if (!is.null(path)) {
      cli_progress_step(
        "Writing {.val {form_id}} Stata do-file to {.file {path}}"
      )
    } else {
      cli_progress_step("Writing {.val {form_id}} Stata do-file")
    }
  }

  # --- 1. Header Generation ---
  ts <- format(Sys.time(), format = '%b %d, %Y at %H:%M %Z')
  t1 <- center_text(str_glue("{toupper(form_id)} VARIABLE AND VALUE LABELS"))
  t2 <- center_text(str_glue("Generated on {ts} by 'ctoclient' Package in R"))

  header_content <- str_glue(
    strrep("*", 80),
    paste0("*", t1, "*"),
    paste0("*", t2, "*"),
    strrep("*", 80),
    "",
    "",
    .sep = "\n"
  )

  # --- 2. Label Column Detection ---
  find_label_col <- function(cols, choices_cols = FALSE) {
    matches <- cols[grepl("^label", cols, TRUE)]
    if (length(matches) == 0) {
      cli_abort("No column with label name found.")
    }

    if (length(matches) > 1) {
      default_lang <- form$settings$default_language[1]
      dl <- if (
        is.null(default_lang) || is.na(default_lang) || default_lang == ""
      ) {
        "english"
      } else {
        default_lang
      }
      # SurveyCTO writes the default language as "English (en)", so `dl` has
      # to be matched literally rather than as a regular expression.
      matches_lang <- matches[
        stringr::str_detect(matches, stringr::fixed(dl, ignore_case = TRUE))
      ]
      if (length(matches_lang) > 0) {
        return(matches_lang[1])
      }
      return(matches[1])
    }
    return(matches)
  }

  var_label_col <- find_label_col(names(form$survey))

  val_label_col <- if (var_label_col %in% names(form$choices)) {
    var_label_col
  } else {
    find_label_col(names(form$choices))
  }

  # --- 3. Process Survey & Extract List Names ---
  survey <- form$survey
  if (any(grepl("^disabled$", names(survey)))) {
    survey <- dplyr::filter(survey, !grepl("yes", .data$disabled, TRUE))
  }

  # Extract list names and types efficiently
  select_types <- survey |>
    dplyr::filter(grepl("select_", .data$type, TRUE)) |>
    mutate(
      type_clean = str_squish(.data$type),
      list_name = str_extract(.data$type_clean, "(?<= )\\S+"),
      is_multi = grepl("select_multiple", .data$type_clean, TRUE),
      .keep = "none"
    )

  valid_choices_s1 <- unique(select_types$list_name[!select_types$is_multi])
  valid_choices_sm <- unique(select_types$list_name[select_types$is_multi])

  # --- 4. Process Choices ---
  choices_all <- form$choices |>
    mutate(
      value = suppressWarnings(as.numeric(.data$value)),
      list_name = str_squish(.data$list_name),
      label_clean = stata_escape_label(.data[[val_label_col]]) |>
        str_squish()
    ) |>
    dplyr::filter(
      !is.na(.data$value),
      # Stata value labels take integers only.
      .data$value == round(.data$value),
      !is.na(.data$label_clean),
      !grepl("^\\\\\\$\\{.*\\}$", .data$label_clean)
    )

  # Generate 'label define' commands for select_one
  choice_sets_s1 <- choices_all |>
    dplyr::filter(.data$list_name %in% valid_choices_s1) |>
    dplyr::summarise(
      stata_cmd = paste0(
        'cap label define ',
        dplyr::first(.data$list_name),
        ' ',
        paste0(.data$value, ' "', .data$label_clean, '"', collapse = " "),
        ', modify'
      ),
      .by = "list_name"
    )

  # Prepare lookup for select_multiple (using a list for fast access)
  multi_lookup <- choices_all |>
    dplyr::filter(.data$list_name %in% valid_choices_sm) |>
    select("list_name", "value", "label_clean") |>
    mutate(
      value = ifelse(
        .data$value < 0,
        paste0("_", abs(.data$value)),
        as.character(.data$value)
      )
    ) |>
    dplyr::group_split(.data$list_name)

  # Take the names from the groups themselves: group_split() and sort() do
  # not agree once list names mix case, which silently attached one list's
  # labels to another list's variables.
  names(multi_lookup) <- purrr::map_chr(multi_lookup, ~ .x$list_name[1])

  # --- 4b. Date and Time Fields ---
  null_block <- build_null_block(form_null_vars(survey$name, survey$type))

  dt_vars <- form_datetime_vars(survey$name, survey$type)

  datetime_block <- build_datetime_block(
    dt_vars$datetime,
    dt_vars$date,
    format(Sys.time(), "%Y")
  )

  # --- 5. Process Variables ---

  # Pre-clean survey data
  var_labels <- survey |>
    mutate(
      orig_order = dplyr::row_number(),
      type = str_squish(str_replace_all(.data$type, "\\\n", " ")),
      name = str_squish(str_replace_all(.data$name, "\\\n", " ")),

      # Calculate repeat levels
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
      is_slt_multi = grepl("^select_multiple", .data$type, TRUE),
      is_num_type = grepl("^integer$|^decimal$", .data$type, TRUE),
      list_name_raw = str_extract(.data$type, "(?<= )\\S+"),
      list_name = ifelse(
        .data$is_slt_multi,
        "slt_multi_binary",
        .data$list_name_raw
      ),
      list_multi = ifelse(
        .data$is_slt_multi,
        .data$list_name_raw,
        NA_character_
      ),
      is_null_fields = grepl(
        "^note|^begin[ _]group|^end[ _]group|^begin[ _]repeat|^end[ _]repeat",
        .data$type,
        TRUE
      )
    ) |>
    dplyr::filter(!.data$is_null_fields, !is.na(.data[[var_label_col]])) |>
    # Generate regex only for relevant rows later if possible, but structure implies we need it here
    mutate(
      regex_varname = purrr::pmap_chr(
        list(.data$name, .data$repeat_level, .data$is_slt_multi),
        gen_regex_varname
      ),

      # Efficient cleaning of the label column
      cleaned_label = stata_escape_label(.data[[var_label_col]]) |>
        str_replace_all("\\\n", " ") |>
        str_squish(),

      var_label = stringr::str_trunc(
        #stringr::str_remove(.data$cleaned_label, paste0(.data$name, "(\\W+)?")),
        .data$cleaned_label,
        80
      ),
      var_note = .data$cleaned_label,
      has_list = !is.na(.data$list_name) & !.data$is_slt_multi
    ) |>
    dplyr::filter(.data$var_label != "" & !is.na(.data$var_label))

  # --- SPLIT-APPLY-COMBINE STRATEGY ---

  # 1. Complex Select Multiple Logic
  vars_multi <- var_labels |>
    dplyr::filter(.data$is_slt_multi) |>
    mutate(
      stata_cmd = purrr::pmap_chr(
        list(
          .data$regex_varname,
          .data$list_multi,
          .data$var_label,
          .data$var_note,
          .data$name
        ),
        function(n, l, v, vn, ln) {
          base_cmd <- str_c(
            "cap {\n",
            "\tunab vars : ",
            ln,
            "*\n",
            "\tforeach var of local vars {\n",
            "\t\tif regexm(\"`var'\", \"",
            n,
            "\") {\n",
            "\t\t\tcap label variable `var' \"",
            v,
            "\"\n",
            "\t\t\tcap note `var': ",
            vn,
            "\n",
            "\t\t\tcap destring `var', replace\n",
            "\t\t\tcap label values `var' slt_multi_binary\n"
          )

          choice_cmds <- ""
          if (!is.na(l) && l %in% names(multi_lookup)) {
            choices <- multi_lookup[[l]]
            patterns <- stringr::str_replace(
              n,
              stringr::fixed("*[0-9]+"),
              choices$value
            )
            full_labels <- ifelse(
              !is.na(choices$label_clean) & choices$label_clean != "",
              paste0(choices$label_clean, " - ", v),
              v
            )
            #full_labels <- paste0(choices$label_clean, " - ", v)
            full_labels <- ifelse(
              nchar(full_labels) > 80,
              stringr::str_trunc(full_labels, 80),
              full_labels
            )

            choice_cmds <- paste0(
              "\t\tif regexm(\"`var'\", \"",
              patterns,
              "\") {\n",
              "\t\t\tcap label variable `var' \"",
              full_labels,
              "\"\n",
              "\t\t\t}\n",
              collapse = ""
            )
          } else {
            choice_cmds <- paste0("\t\tcap label variable `var' \"", v, "\"\n")
          }

          str_c(base_cmd, choice_cmds, "\t\t}\n", "\t}\n", "}")
        }
      )
    )

  # 2. Logic for Repeats (Non-Multi)
  vars_repeat <- var_labels |>
    dplyr::filter(!.data$is_slt_multi & .data$is_repeat) |>
    mutate(
      stata_cmd = str_c(
        "cap {\n",
        "\tunab vars : ",
        .data$name,
        "*\n",
        "\tforeach var of local vars {\n",
        "\t\tif regexm(\"`var'\", \"",
        .data$regex_varname,
        "\") {\n",
        ifelse(
          .data$is_num_type,
          "\t\t\tcap destring `var', replace\n",
          ""
        ),
        "\t\t\tcap label variable `var' \"",
        .data$var_label,
        "\"\n",
        "\t\t\tcap note `var': ",
        .data$var_note,
        "\n",
        ifelse(
          .data$has_list,
          paste0(
            "\t\t\tcap destring `var', replace\n",
            "\t\t\tcap label values `var' ",
            .data$list_name,
            "\n"
          ),
          ""
        ),
        "\t\t}\n",
        "\t}\n",
        "}"
      )
    )

  # 3. Simple Variables (Non-Multi, Non-Repeat)
  vars_simple <- var_labels |>
    dplyr::filter(!.data$is_slt_multi & !.data$is_repeat) |>
    mutate(
      stata_cmd = str_c(
        ifelse(
          .data$is_num_type,
          paste0("cap destring ", .data$name, ", replace\n"),
          ""
        ),
        "cap label variable ",
        .data$name,
        " \"",
        .data$var_label,
        "\"\n",
        "cap note ",
        .data$name,
        ": ",
        .data$var_note,
        ifelse(
          .data$has_list,
          paste0(
            "\ncap destring ",
            .data$name,
            ", replace",
            "\ncap label values ",
            .data$name,
            " ",
            .data$list_name
          ),
          ""
        )
      )
    )

  # Combine and Restore Order
  labels_set <- dplyr::bind_rows(vars_multi, vars_repeat, vars_simple) |>
    dplyr::arrange(.data$orig_order)

  # --- 6. Final File Assembly ---

  do_file_content <- c(
    header_content,
    if (length(null_block) > 0) {
      c(
        paste0("*", center_text(" EMPTY FIELDS ", "-"), "*"),
        "",
        null_block,
        ""
      )
    },
    if (length(datetime_block) > 0) {
      c(
        paste0("*", center_text(" DATE AND TIME FIELDS ", "-"), "*"),
        "",
        datetime_block,
        ""
      )
    },
    paste0("*", center_text(" VALUE LABELS ", "-"), "*"),
    "",
    "label define slt_multi_binary 1 \"Yes\" 0 \"No\", modify",
    choice_sets_s1[["stata_cmd"]],
    "",
    "",
    paste0("*", center_text(" VARIABLE LABELS ", "-"), "*"),
    "",
    labels_set[["stata_cmd"]],
    "",
    "",
    paste0("*", center_text(" DEFAULT FIELDS ", "-"), "*"),
    "",
    c(
      'cap replace KEY = instanceID if KEY==""',
      'cap drop instanceID',
      'cap label variable KEY "Unique submission ID"',
      'cap label variable SubmissionDate "Date/time submitted"',
      'cap label variable CompletionDate "Date/time review completed"',
      'cap label variable formdef_version "Form version used on device"',
      'cap label variable review_status "Review status"',
      'cap label variable review_comments "Comments made during review"',
      'cap label variable review_corrections "Corrections made during review"'
    ),
    "",
    "",
    paste0("*", center_text(" THE END! ", "-"), "*")
  )

  # Incase of missing value labels for binaries
  #do_file_content <- sub('(") - ', '\\1', do_file_content)

  if (!is.null(path)) {
    # readxl returns UTF-8, but writeLines() would otherwise re-encode to the
    # session's native encoding and mangle non-ASCII labels on Windows.
    con <- file(path, open = "wb")
    on.exit(close(con), add = TRUE)
    writeLines(enc2utf8(do_file_content), con, useBytes = TRUE)
  }
  return(invisible(do_file_content))
}
