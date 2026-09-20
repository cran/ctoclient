
# End-to-end test of the form review document. "fixtures/demoform.xlsx" is an
# XLSForm carrying the cases the parser has to get right - metadata fields,
# a nested repeat, a group with its own relevance, or_other, constraints and
# hints - so the only thing mocked out is the download.

demo_form <- function() {
  path <- test_path("fixtures", "demoform.xlsx")
  list(
    survey = readxl::read_excel(path, sheet = "survey"),
    choices = readxl::read_excel(path, sheet = "choices"),
    settings = readxl::read_excel(path, sheet = "settings")
  )
}

docx <- function(path = NULL, ...) {
  local_mocked_bindings(
    cto_form_definition = function(...) test_path("fixtures", "demoform.xlsx"),
    .package = "ctoclient"
  )
  old <- options(ctoclient.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  cto_form_docx("demoform", path = path, ...)
}


# ---- field_family() ----

test_that(
  "field_family() groups the types that should share a colour",
  {
    expect_identical(field_family("select_one yn"), "select_one")
    expect_identical(field_family("select_one gender"), "select_one")
    expect_identical(
      field_family("select_multiple crops or_other"),
      "select_multiple"
    )
    expect_identical(field_family("select_one_from_file x.csv"), "select_one")
    expect_identical(field_family("begin group"), "group")
    expect_identical(field_family("begin_repeat"), "repeat")
    expect_identical(field_family("end_group"), "end")
    expect_identical(field_family("integer"), "numeric")
    expect_identical(field_family("geopoint"), "geo")
    expect_identical(field_family("image"), "media")
  }
)

test_that(
  "field_family() does not let one type swallow another",
  {
    # "date" must not catch "deviceid", "text" must not catch "text_audit"
    expect_identical(field_family("deviceid"), "other")
    expect_identical(field_family("text_audit"), "other")
    expect_identical(field_family("datetime"), "datetime")
    expect_identical(field_family("text"), "text")
    # a bare "select_one" with no list is still a select_one
    expect_identical(field_family("select_one"), "select_one")
  }
)


# ---- form_display_fields() ----

test_that(
  "form_display_fields() keeps only what the enumerator sees",
  {
    f <- demo_form()
    out <- form_display_fields(f$survey, f$choices)

    # metadata and calculates never reach the document
    expect_false(any(c("starttime", "endtime", "today", "deviceid",
                       "n_plots", "audit") %in% out$name))
    # notes and questions do
    expect_true(all(c("note_intro", "resp_name", "plot_size") %in% out$name))
    # the closing rows carry no name and contribute nothing
    expect_false(any(out$family == "end"))
  }
)

test_that(
  "form_display_fields() can be asked for the metadata as well",
  {
    f <- demo_form()
    out <- form_display_fields(f$survey, f$choices, show_metadata = TRUE)

    expect_true(all(c("starttime", "deviceid", "n_plots") %in% out$name))
    # end rows are structural, so they stay out either way
    expect_false(any(out$family == "end"))
  }
)

test_that(
  "form_display_fields() numbers questions but not the bands",
  {
    f <- demo_form()
    out <- form_display_fields(f$survey, f$choices)

    expect_false(any(duplicated(out$seq[!out$is_header])))
    expect_identical(out$seq[!out$is_header], seq_len(sum(!out$is_header)))
  }
)

test_that(
  "form_display_fields() keeps the nesting depth of every field",
  {
    f <- demo_form()
    out <- form_display_fields(f$survey, f$choices)
    depth <- stats::setNames(out$depth, out$name)

    expect_equal(unname(depth["note_intro"]), 0)   # outside any group
    expect_equal(unname(depth["resp_name"]), 1)    # inside SECTION A
    expect_equal(unname(depth["plot_rpt"]), 1)     # the band sits at its parent
    expect_equal(unname(depth["plot_size"]), 2)    # inside the repeat
    expect_equal(unname(depth["visit_date"]), 1)   # back out in SECTION C
  }
)

test_that(
  "form_display_fields() reads the list name, not the or_other suffix",
  {
    f <- demo_form()
    out <- form_display_fields(f$survey, f$choices)
    illness <- out[out$name == "illness", ]

    # the list is "diseases"; the trailing or_other is not part of the name
    expect_identical(illness$list_name, "diseases")
    expect_match(illness$options, "Malaria", fixed = TRUE)
    expect_no_match(illness$options, "or_other", fixed = TRUE)

    # a question that is not a select carries no list at all
    expect_true(is.na(out$list_name[out$name == "resp_name"]))
  }
)

test_that(
  "form_display_fields() carries the group's own relevance",
  {
    f <- demo_form()
    out <- form_display_fields(f$survey, f$choices)
    band <- out[out$name == "grp_prod", ]

    expect_true(band$is_header)
    expect_identical(band$relevance, "${owns_land} = 1")
  }
)


# ---- format_choice_value() ----

test_that(
  "format_choice_value() never puts a decimal on a whole code",
  {
    # readxl hands back a numeric column whenever the sheet holds only
    # numbers; code 1 must not become "1.0"
    expect_identical(format_choice_value(c(1, 0, 2, 96)), c("1", "0", "2", "96"))
    expect_identical(format_choice_value(c(1L, 0L)), c("1", "0"))
    # one fractional code in a list must not put a decimal on the rest
    expect_identical(format_choice_value(c(1, 0, 1.5)), c("1", "0", "1.5"))
    # nor may a trailing ".0" survive when it arrives as text
    expect_identical(format_choice_value(c("1.0", "0.0")), c("1", "0"))
  }
)

test_that(
  "format_choice_value() leaves everything else as written",
  {
    # a zero-padded or non-numeric code keeps its spelling
    expect_identical(
      format_choice_value(c("01", "other", "1.5", "1.50")),
      c("01", "other", "1.5", "1.50")
    )
    expect_identical(format_choice_value(c(-9, 0.25)), c("-9", "0.25"))
    # a large code must not turn into scientific notation
    expect_identical(format_choice_value(1e6), "1000000")
  }
)


# ---- format_choices() ----

test_that(
  "format_choices() puts one choice on each line",
  {
    f <- demo_form()
    # the fixture stores the values as numbers, which is what a real
    # SurveyCTO choices sheet does
    expect_type(f$choices$value, "double")
    expect_identical(format_choices("sex", f$choices), "1 = Male\n2 = Female")
    expect_identical(format_choices("yn", f$choices), "1 = Yes\n0 = No")
    expect_identical(
      format_choices("crops", f$choices),
      "1 = Maize\n2 = Teff\n3 = Wheat\n4 = Coffee\n96 = Other (specify)"
    )
  }
)

test_that(
  "format_choices() skips a choice that carries no value",
  {
    ch <- data.frame(
      list_name = c("yn", "yn", "yn"),
      value = c(1, NA, 0),
      label = c("Yes", "", "No"),
      stringsAsFactors = FALSE
    )
    expect_identical(format_choices("yn", ch), "1 = Yes\n0 = No")
  }
)

test_that(
  "format_choices() returns NA for a list it cannot find",
  {
    f <- demo_form()
    expect_true(is.na(format_choices(NA_character_, f$choices)))
    expect_true(is.na(format_choices("not_a_list", f$choices)))
  }
)


# ---- pick_form_col() ----

test_that(
  "pick_form_col() prefers the requested language",
  {
    d <- data.frame(
      `label::Amharic (am)` = "a",
      `label::English (en)` = "b",
      check.names = FALSE
    )
    expect_identical(pick_form_col(d, "^label", "English (en)"), "label::English (en)")
    expect_identical(pick_form_col(d, "^label", "Amharic (am)"), "label::Amharic (am)")
    # no language given, or one the form does not have: first column wins
    expect_identical(pick_form_col(d, "^label"), "label::Amharic (am)")
    expect_identical(pick_form_col(d, "^label", "Swahili (sw)"), "label::Amharic (am)")
  }
)

test_that(
  "pick_form_col() matches both spellings of constraint message",
  {
    expect_identical(
      pick_form_col(data.frame(`constraint message` = "x", check.names = FALSE),
                    "^constraint[ _]message"),
      "constraint message"
    )
    expect_identical(
      pick_form_col(data.frame(constraint_message = "x"),
                    "^constraint[ _]message"),
      "constraint_message"
    )
    expect_null(pick_form_col(data.frame(a = 1), "^constraint"))
  }
)


# ---- merge_docx_palette() ----

test_that(
  "merge_docx_palette() overrides only the families it is given",
  {
    pal <- merge_docx_palette(c(numeric = "#FFF2CC", nonsense = "#000000"))

    expect_identical(unname(pal["numeric"]), "#FFF2CC")
    expect_identical(unname(pal["note"]), unname(cto_docx_palette()["note"]))
    expect_false("nonsense" %in% names(pal))
    expect_identical(names(pal), names(cto_docx_palette()))
  }
)


# ---- cto_form_docx() ----

test_that(
  "cto_form_docx() writes a document and returns the fields",
  {
    p <- file.path(tempdir(), "demoform_review.docx")
    on.exit(unlink(p), add = TRUE)

    fields <- docx(path = p)

    expect_true(file.exists(p))
    expect_gt(file.size(p), 5000)
    expect_s3_class(fields, "data.frame")
    expect_true(all(c("seq", "family", "name", "options", "depth", "is_header")
                    %in% names(fields)))
  }
)

test_that(
  "the written document is a readable Word file",
  {
    skip_if_not_installed("xml2")

    p <- file.path(tempdir(), "demoform_check.docx")
    on.exit(unlink(p), add = TRUE)
    docx(path = p)

    parts <- utils::unzip(p, list = TRUE)$Name
    expect_true("word/document.xml" %in% parts)

    xml <- paste(readLines(unz(p, "word/document.xml"), warn = FALSE),
                 collapse = "")

    # the section bands, the shading, and the repeating header row
    expect_match(xml, "SECTION A. HOUSEHOLD ROSTER", fixed = TRUE)
    expect_match(xml, "REPEAT GROUP", fixed = TRUE)
    expect_match(xml, "2F5496", fixed = TRUE)  # group band
    expect_match(xml, "BF8F00", fixed = TRUE)  # repeat band
    expect_match(xml, "tblHeader", fixed = TRUE)
    # each choice is its own run, so the list breaks across lines rather
    # than running together in one cell
    expect_match(xml, ">1 = Male</w:t>", fixed = TRUE)
    expect_match(xml, ">2 = Female</w:t>", fixed = TRUE)
    expect_no_match(xml, "1 = Male 2 = Female", fixed = TRUE)
    expect_match(xml, "<w:br/>", fixed = TRUE)

    # the codes reach the page as the form writes them, not as decimals
    expect_no_match(xml, "1.0 = ", fixed = TRUE)
    expect_no_match(xml, "0.0 = ", fixed = TRUE)
    expect_match(xml, ">96 = Other (specify)</w:t>", fixed = TRUE)
    # a group's relevance is on its band
    expect_match(xml, "shown if:", fixed = TRUE)
    # and no metadata field leaked in
    expect_no_match(xml, "deviceid", fixed = TRUE)
  }
)

test_that(
  "cto_form_docx() defaults to a file in the session temp directory",
  {
    fields <- docx()
    p <- file.path(tempdir(), "demoform_review.docx")
    on.exit(unlink(p), add = TRUE)

    expect_true(file.exists(p))
    expect_gt(nrow(fields), 0)
  }
)

test_that(
  "cto_form_docx() rejects a path that is not a .docx",
  {
    expect_error(docx(path = file.path(tempdir(), "review.pdf")))
  }
)
