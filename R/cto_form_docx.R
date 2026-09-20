#' Generate a Printable Word Document of a Deployed SurveyCTO Form
#'
#' @description
#' Downloads the XLSForm definition of a form and renders the fields an
#' enumerator actually sees as a formatted Word table, for review and
#' sign-off. Calculates, metadata fields and disabled rows are left out,
#' groups and repeat groups become banded section headers, and every row is
#' shaded according to its field type.
#'
#' @param form_id A character string specifying the SurveyCTO form ID.
#' @param path Optional character string giving the output file path. Must
#'   end in `.docx`. If `NULL` (default), the file is written to `tempdir()`
#'   under the form ID.
#' @param version Optional string specifying a particular form version,
#'   passed to [cto_form_definition()]. If `NULL` (default), the currently
#'   deployed version is used.
#' @param language Optional string naming the label language to render, for
#'   example `"English (en)"`. If `NULL` (default), the form's
#'   `default_language` is used, falling back to the first label column.
#' @param show_metadata Logical. If `TRUE`, `calculate` and metadata fields
#'   such as `start`, `end`, `deviceid` and the audit fields are included.
#'   Defaults to `FALSE`, which is what makes the output a collector's-eye
#'   view of the form.
#' @param palette Optional named character vector of hex colours overriding
#'   the defaults returned by [cto_docx_palette()]. Names that are not
#'   supplied keep their default colour.
#'
#' @details
#' `cto_form_printable()` asks the SurveyCTO server for its own printable
#' rendering of a form. `cto_form_docx()` is different: it builds the
#' document locally from the XLSForm definition, so it can show the variable
#' names, relevance and constraint expressions that a reviewer needs and the
#' server-side printable does not carry.
#'
#' Fields are shaded by family rather than by raw type, so that
#' `select_one yes_no` and `select_one gender` share a colour. The families
#' are `note`, `text`, `numeric`, `select_one`, `select_multiple`,
#' `datetime`, `geo`, `media` and `other`, with separate band colours for
#' groups and repeat groups.
#'
#' Nesting depth is counted before any row is dropped, so a question still
#' shows the indentation of the groups it sits inside even when one of those
#' wrappers is filtered out. A group's own relevance travels with its band,
#' since a condition that governs the whole section would otherwise be lost.
#'
#' @return A data frame of the rendered fields, returned invisibly. The
#'   document is written to `path` for its side effect.
#'
#' @family Form Management Functions
#'
#' @seealso [cto_form_printable()] for the server's own printable version.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Review document for the deployed version
#' cto_form_docx("household_survey", path = "household_survey_review.docx")
#'
#' # A specific version, in a named language
#' cto_form_docx(
#'   "household_survey",
#'   path = "review_v2.docx",
#'   version = "20231001",
#'   language = "Amharic (am)"
#' )
#'
#' # Include the calculates and metadata fields as well
#' cto_form_docx("household_survey", path = "full.docx", show_metadata = TRUE)
#' }
cto_form_docx <- function(
  form_id,
  path = NULL,
  version = NULL,
  language = NULL,
  show_metadata = FALSE,
  palette = NULL
) {
  verbose <- get_verbose()

  assert_string(language, null.ok = TRUE)
  assert_flag(show_metadata)
  assert_character(palette, null.ok = TRUE, names = "named")

  if (!is.null(path)) {
    checkmate::assert_path_for_output(path, TRUE, "docx")
  }

  fp <- cto_form_definition(
    form_id,
    version = version,
    dir = tempdir(),
    overwrite = TRUE
  )
  form <- list(
    survey = readxl::read_excel(fp, sheet = "survey"),
    choices = readxl::read_excel(fp, sheet = "choices"),
    settings = readxl::read_excel(fp, sheet = "settings")
  )

  if (is.null(path)) {
    path <- file.path(tempdir(), paste0(form_id, "_review.docx"))
  }

  if (verbose) {
    cli_progress_step(
      "Writing {.val {form_id}} review document to {.file {path}}",
      "Wrote {.val {form_id}} review document to {.file {path}}"
    )
  }

  lang <- resolve_form_language(form$settings, language)
  fields <- form_display_fields(form$survey, form$choices, lang, show_metadata)

  if (nrow(fields) == 0) {
    cli_warn(c(
      "!" = "{.val {form_id}} has no fields to show.",
      "i" = "An empty document was written to {.file {path}}."
    ))
  }

  pal <- merge_docx_palette(palette)

  doc <- officer::read_docx() |>
    build_docx_cover(form_id, form$settings, fields, lang) |>
    flextable::body_add_flextable(build_legend_table(pal), align = "left") |>
    officer::body_add_par("", style = "Normal") |>
    flextable::body_add_flextable(build_field_table(fields, pal), align = "left") |>
    officer::body_end_section_landscape()

  print(doc, target = path)
  invisible(fields)
}


#' Row Colours Used by the Form Review Document
#'
#' @description
#' The default fill colour of each field family in the document that
#' [cto_form_docx()] produces. The values are Office theme tints, so the
#' table looks native in Word and stays legible when printed in greyscale.
#'
#' @return A named character vector of hex colours.
#'
#' @family Form Management Functions
#'
#' @export
#'
#' @examples
#' cto_docx_palette()
#'
#' # Override a single family
#' cto_form_docx_palette <- cto_docx_palette()
#' cto_form_docx_palette["numeric"] <- "#FFF2CC"
cto_docx_palette <- function() {
  c(
    group = "#2F5496",
    `repeat` = "#BF8F00",
    note = "#F2F2F2",
    text = "#FFFFFF",
    numeric = "#E2EFDA",
    select_one = "#DDEBF7",
    select_multiple = "#BDD7EE",
    datetime = "#E4DFEC",
    geo = "#DAEEF3",
    media = "#FCE4D6",
    other = "#FFFFFF"
  )
}


# Printable name of each field family ----
cto_family_labels <- c(
  note = "Note",
  text = "Text",
  numeric = "Numeric",
  select_one = "Select one",
  select_multiple = "Select multiple",
  datetime = "Date / time",
  geo = "Geopoint",
  media = "Media",
  other = "Other"
)


# Label language to render ----
# An explicit language wins; otherwise the form's own default is used.
resolve_form_language <- function(settings, language = NULL) {
  if (!is.null(language)) {
    return(language)
  }
  if (!any(grepl("^default_language$", names(settings)))) {
    return(NULL)
  }
  dl <- settings$default_language[1]
  if (is.na(dl) || !nzchar(str_squish(dl))) NULL else dl
}


# User colours over the defaults ----
merge_docx_palette <- function(palette = NULL) {
  pal <- cto_docx_palette()
  if (is.null(palette)) {
    return(pal)
  }
  known <- intersect(names(palette), names(pal))
  pal[known] <- palette[known]
  pal
}


# Blank cells become NA so they are easy to test for ----
blank_to_na <- function(x, keep_breaks = FALSE) {
  trimmed <- if (keep_breaks) stringr::str_trim(x) else str_squish(x)
  ifelse(is.na(x) | !nzchar(str_squish(x)), NA_character_, trimmed)
}


# Contents of the label cell ----
# The label, then the hint beneath it, then the constraint message. Keeping
# all three in one cell is what lets the table fit across a printed page.
build_label_par <- function(label, hint, message, required) {
  parts <- list(flextable::as_chunk(
    if (is.na(label)) "" else label,
    props = officer::fp_text(font.size = 9, font.family = "Calibri")
  ))

  if (isTRUE(required)) {
    parts <- c(parts, list(flextable::as_chunk(
      " *",
      props = officer::fp_text(
        font.size = 9,
        font.family = "Calibri",
        color = "#C00000",
        bold = TRUE
      )
    )))
  }

  if (!is.na(hint)) {
    parts <- c(parts, list(flextable::as_chunk(
      paste0("\n", hint),
      props = officer::fp_text(
        font.size = 8,
        font.family = "Calibri",
        italic = TRUE,
        color = "#595959"
      )
    )))
  }

  if (!is.na(message)) {
    parts <- c(parts, list(flextable::as_chunk(
      paste0("\nError: ", message),
      props = officer::fp_text(
        font.size = 8,
        font.family = "Calibri",
        italic = TRUE,
        color = "#843C0C"
      )
    )))
  }

  do.call(flextable::as_paragraph, parts)
}


# The field table ----
build_field_table <- function(fields, palette = cto_docx_palette()) {
  d <- fields |>
    mutate(
      across(
        c(
          "label", "hint", "relevance", "constraint", "constraint_message",
          "appearance", "repeat_count"
        ),
        blank_to_na
      ),
      # The choice list is the one column whose line breaks carry meaning,
      # so it is trimmed rather than squished.
      options = blank_to_na(.data$options, keep_breaks = TRUE),
      required = !is.na(.data$required) &
        grepl("^(yes|true|1)$", str_squish(.data$required), TRUE),
      # A group's relevance governs everything inside it, so it travels with
      # the band rather than being dropped along with the other cells.
      band_relevance = ifelse(
        is.na(.data$relevance) | !.data$is_header,
        "",
        paste0("      [ shown if:  ", .data$relevance, " ]")
      ),
      # merge_h_range() keeps the first column's contents, so a band has to
      # carry its text there.
      band_text = dplyr::case_when(
        .data$family == "group" ~ paste0(
          dplyr::coalesce(.data$label, .data$name),
          .data$band_relevance
        ),
        .data$family == "repeat" ~ paste0(
          "REPEAT GROUP:  ",
          dplyr::coalesce(.data$label, .data$name),
          ifelse(
            is.na(.data$repeat_count),
            "",
            paste0("   (count: ", .data$repeat_count, ")")
          ),
          .data$band_relevance
        ),
        TRUE ~ ""
      )
    )

  ft <- flextable::flextable(data.frame(
    seq = ifelse(d$is_header, d$band_text, as.character(d$seq)),
    type = ifelse(
      d$is_header,
      "",
      dplyr::coalesce(unname(cto_family_labels[d$family]), d$family)
    ),
    name = ifelse(d$is_header, "", d$name),
    label = dplyr::coalesce(d$label, ""),
    options = dplyr::coalesce(d$options, ""),
    relevance = ifelse(d$is_header, "", dplyr::coalesce(d$relevance, "")),
    constraint = dplyr::coalesce(d$constraint, ""),
    stringsAsFactors = FALSE
  ))

  ft <- ft |>
    flextable::set_header_labels(
      seq = "#",
      type = "Type",
      name = "Variable name",
      label = "Label / hint",
      options = "Choices",
      relevance = "Relevance",
      constraint = "Constraint"
    ) |>
    flextable::width(j = 1, width = 0.35) |>
    flextable::width(j = 2, width = 0.95) |>
    flextable::width(j = 3, width = 1.25) |>
    flextable::width(j = 4, width = 2.70) |>
    flextable::width(j = 5, width = 1.55) |>
    flextable::width(j = 6, width = 1.60) |>
    flextable::width(j = 7, width = 1.60) |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::font(fontname = "Calibri", part = "all") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::valign(valign = "top", part = "body") |>
    flextable::border_remove() |>
    flextable::border_inner_h(
      border = officer::fp_border(color = "#BFBFBF", width = 0.5)
    ) |>
    flextable::border_inner_v(
      border = officer::fp_border(color = "#BFBFBF", width = 0.5)
    ) |>
    flextable::border_outer(
      border = officer::fp_border(color = "#7F7F7F", width = 1)
    ) |>
    flextable::bg(bg = "#404040", part = "header") |>
    flextable::color(color = "white", part = "header") |>
    flextable::bold(part = "header")

  # Variable names and XLSForm expressions are monospaced, so a reviewer can
  # tell ${field} from prose at a glance.
  ft <- ft |>
    flextable::font(
      j = c("name", "relevance", "constraint"),
      fontname = "Consolas",
      part = "body"
    ) |>
    flextable::fontsize(
      j = c("relevance", "constraint"),
      size = 8,
      part = "body"
    ) |>
    flextable::fontsize(j = "options", size = 8, part = "body") |>
    flextable::align(j = "seq", align = "right", part = "body")

  for (i in seq_len(nrow(d))) {
    ft <- flextable::bg(
      ft,
      i = i,
      bg = unname(palette[d$family[i]]),
      part = "body"
    )

    if (d$is_header[i]) {
      ft <- ft |>
        flextable::merge_h_range(i = i, j1 = 1, j2 = 7, part = "body") |>
        flextable::color(i = i, color = "white", part = "body") |>
        flextable::bold(i = i, part = "body") |>
        flextable::fontsize(i = i, size = 10, part = "body") |>
        flextable::font(i = i, fontname = "Calibri", part = "body") |>
        flextable::align(i = i, align = "left", part = "body") |>
        flextable::padding(
          i = i,
          padding.top = 5,
          padding.bottom = 5,
          padding.left = 4 + d$depth[i] * 8,
          part = "body"
        )
      next
    }

    ft <- flextable::compose(
      ft,
      i = i,
      j = "label",
      value = build_label_par(
        d$label[i],
        d$hint[i],
        d$constraint_message[i],
        d$required[i]
      ),
      part = "body"
    )

    # The choices need to go through as_chunk() as well, or their line
    # breaks arrive in Word as spaces.
    if (!is.na(d$options[i])) {
      ft <- flextable::compose(
        ft,
        i = i,
        j = "options",
        value = flextable::as_paragraph(flextable::as_chunk(
          d$options[i],
          props = officer::fp_text(font.size = 8, font.family = "Calibri")
        )),
        part = "body"
      )
    }

    # A field inside a group is indented by its depth, so the nesting shows
    # without drawing a second table.
    if (d$depth[i] > 0) {
      ft <- flextable::padding(
        ft,
        i = i,
        j = "label",
        padding.left = 3 + d$depth[i] * 6,
        part = "body"
      )
    }
  }

  ft
}


# The colour key ----
build_legend_table <- function(palette = cto_docx_palette()) {
  families <- names(cto_family_labels)
  families <- families[families != "other"]

  row <- as.list(unname(cto_family_labels[families]))
  names(row) <- paste0("c", seq_along(families))
  row <- as.data.frame(row, stringsAsFactors = FALSE)

  ft <- flextable::flextable(row) |>
    flextable::delete_part("header") |>
    flextable::fontsize(size = 8, part = "body") |>
    flextable::font(fontname = "Calibri", part = "body") |>
    flextable::align(align = "center", part = "body") |>
    flextable::padding(padding = 3, part = "body") |>
    flextable::border_remove() |>
    flextable::border_outer(
      border = officer::fp_border(color = "#BFBFBF", width = 0.5)
    ) |>
    flextable::border_inner_v(
      border = officer::fp_border(color = "#BFBFBF", width = 0.5)
    ) |>
    flextable::width(width = 1.1)

  for (k in seq_along(families)) {
    ft <- flextable::bg(ft, j = k, bg = unname(palette[families[k]]))
  }
  ft
}


# The block above the table ----
build_docx_cover <- function(doc, form_id, settings, fields, lang) {
  title <- if (any(grepl("^form_title$", names(settings)))) {
    settings$form_title[1]
  } else {
    form_id
  }
  version <- if (any(grepl("^version$", names(settings)))) {
    as.character(settings$version[1])
  } else {
    NA_character_
  }

  n_questions <- sum(!fields$is_header)
  n_groups <- sum(fields$family == "group")
  n_repeats <- sum(fields$family == "repeat")

  meta <- paste0(
    "Form ID: ", form_id,
    if (!is.na(version)) paste0("    |    Version: ", version) else "",
    "    |    Language: ", if (is.null(lang)) "default" else lang
  )

  summary <- str_glue(
    "{n_questions} question{ifelse(n_questions == 1, '', 's')} shown to the \\
     enumerator, in {n_groups} group{ifelse(n_groups == 1, '', 's')} and \\
     {n_repeats} repeat group{ifelse(n_repeats == 1, '', 's')}. Calculates, \\
     metadata fields and disabled rows are omitted."
  )

  ts <- format(Sys.time(), "%d %B %Y at %H:%M %Z")

  doc |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        if (is.na(title)) form_id else title,
        officer::fp_text(font.size = 18, bold = TRUE, font.family = "Calibri")
      )
    )) |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        meta,
        officer::fp_text(font.size = 10, font.family = "Calibri")
      )
    )) |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        as.character(summary),
        officer::fp_text(font.size = 10, font.family = "Calibri")
      )
    )) |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        paste0("Generated ", ts, " by the ctoclient package for R."),
        officer::fp_text(
          font.size = 9,
          italic = TRUE,
          color = "#595959",
          font.family = "Calibri"
        )
      )
    )) |>
    officer::body_add_par("", style = "Normal")
}
