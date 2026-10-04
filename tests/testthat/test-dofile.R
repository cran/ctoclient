
# End-to-end test of the Stata do-file generator. "fixtures/testform.xlsx" is a
# small XLSForm covering the cases the generator has to get right, so the only
# thing mocked out is the download.

dofile <- function(path = NULL) {
  local_mocked_bindings(
    cto_form_definition = function(...) test_path("fixtures", "testform.xlsx"),
    .package = "ctoclient"
  )
  old <- options(ctoclient.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  cto_form_dofile("testform", path = path)
}

test_that(
  "the date and time lists come from the form definition",
  {
    out <- dofile()

    # starttime, endtime and interview_dt are the start/end/datetime questions
    expect_true(any(grepl(
      "local dtvarlist CompletionDate SubmissionDate starttime endtime interview_dt",
      out,
      fixed = TRUE
    )))
    # today and visit_date are the today/date questions
    expect_true(any(grepl("local dtvarlist today visit_date", out, fixed = TRUE)))
    # a question of any other type must not reach either list
    expect_false(any(grepl("dtvarlist.*hh_size", out)))
    expect_false(any(grepl("dtvarlist.*resp_name", out)))
  }
)

test_that(
  "date and datetime fields get the right function and format",
  {
    out <- paste(dofile(), collapse = "\n")

    expect_match(out, "clock(`tempdtvar',\"MDYhms\",", fixed = TRUE)
    expect_match(out, "format %tc `dtvar'", fixed = TRUE)
    expect_match(out, "date(`tempdtvar',\"MDY\",", fixed = TRUE)
    expect_match(out, "format %td `dtvar'", fixed = TRUE)
    expect_match(out, "cap confirm string variable `dtvar'", fixed = TRUE)
  }
)

test_that(
  "integer and decimal questions are destrung before their labels",
  {
    out <- paste(dofile(), collapse = "\n")

    expect_match(
      out,
      "cap destring hh_size, replace\ncap label variable hh_size",
      fixed = TRUE
    )
    expect_match(
      out,
      "cap destring land_area, replace\ncap label variable land_area",
      fixed = TRUE
    )
    # an integer inside a repeat group is destrung too
    expect_match(
      out,
      "cap destring `var', replace\n\t\t\tcap label variable `var' \"Plot size\"",
      fixed = TRUE
    )
    # a text question is left alone
    expect_no_match(out, "cap destring resp_name", fixed = TRUE)
  }
)

test_that(
  "value labels are preceded by a destring",
  {
    out <- paste(dofile(), collapse = "\n")

    expect_match(
      out,
      "cap destring owns_land, replace\ncap label values owns_land yn",
      fixed = TRUE
    )
    expect_match(
      out,
      "cap destring `var', replace\n\t\t\tcap label values `var' slt_multi_binary",
      fixed = TRUE
    )
  }
)

test_that(
  "the default language wins over the first label column",
  {
    out <- paste(dofile(), collapse = "\n")

    # settings name English (en); the Amharic column comes first in the sheet
    expect_match(out, "cap label variable today \"Today\"", fixed = TRUE)
    expect_no_match(out, "ዛሬ", fixed = TRUE)
  }
)

test_that(
  "labels are escaped for Stata",
  {
    out <- paste(dofile(), collapse = "\n")

    expect_match(out, "Cost (\\$USD)", fixed = TRUE)
    expect_no_match(out, "Cost ($USD)", fixed = TRUE)
    expect_match(out, "Respondent name", fixed = TRUE)
    expect_no_match(out, "<b>", fixed = TRUE)
  }
)

test_that(
  "notes use Stata's note syntax",
  {
    out <- paste(dofile(), collapse = "\n")

    expect_match(out, "cap note hh_size: Household size", fixed = TRUE)
    expect_no_match(out, "cap note variable", fixed = TRUE)
    # the text after the colon is literal, so quotes would be stored as
    # part of the note
    expect_no_match(out, "cap note hh_size: \"", fixed = TRUE)
    expect_no_match(out, "cap note `var': \"", fixed = TRUE)
  }
)

test_that(
  "label define is capped and skips values Stata cannot use",
  {
    out <- paste(dofile(), collapse = "\n")

    expect_match(out, "cap label define yn 1 \"Yes\" 0 \"No\", modify", fixed = TRUE)
    # 1.5 is not a legal Stata value label
    expect_no_match(out, "1.5", fixed = TRUE)
  }
)

test_that(
  "writing to a path produces a readable UTF-8 file",
  {
    p <- file.path(tempdir(), "testform_labels.do")
    on.exit(unlink(p), add = TRUE)

    dofile(path = p)

    expect_true(file.exists(p))
    txt <- readLines(p, encoding = "UTF-8", warn = FALSE)
    expect_true(any(grepl("Land area in ha", txt, fixed = TRUE)))
    expect_true(all(validUTF8(txt)))
  }
)

test_that(
  "structural fields are confirmed empty before being dropped",
  {
    out <- paste(dofile(), collapse = "\n")

    # the note, the underscore-spelled group, the note inside the repeat,
    # and the repeat counter
    expect_match(
      out,
      "local nullvars1 grp_demog note_intro note_plot plot_rpt_count",
      fixed = TRUE
    )

    expect_match(out, "cap unab matched : `stub'*", fixed = TRUE)
    expect_match(out, "qui count if !missing(`var')", fixed = TRUE)
    expect_match(out, "drop `null_confirmed'", fixed = TRUE)

    # a real question must never become a drop candidate
    expect_false(any(grepl("^\tlocal nullvars[0-9].*\\b(hh_size|resp_name|plot_size)\\b",
                           dofile())))
  }
)

test_that(
  "the empty field section runs before anything else",
  {
    out <- paste(dofile(), collapse = "\n")
    expect_lt(
      regexpr("EMPTY FIELDS", out, fixed = TRUE),
      regexpr("DATE AND TIME", out, fixed = TRUE)
    )
  }
)

test_that(
  "a date field is only parsed while it is still a string",
  {
    out <- paste(dofile(), collapse = "\n")

    expect_match(out, "cap confirm string variable `dtvar'", fixed = TRUE)
    # the bare existence check would re-parse an already converted variable,
    # replacing it with missing values
    expect_no_match(out, "cap confirm variable `dtvar'", fixed = TRUE)
  }
)

test_that(
  "one loop labels a repeat question bare and indexed",
  {
    out <- paste(dofile(), collapse = "\n")

    # the index is optional and repeatable, so the bare name and every
    # nested copy are handled without lines of their own
    expect_match(out, "if regexm(\"`var\'\", \"^plot_size(_[0-9]+)*$\")", fixed = TRUE)
    expect_match(out, "\tunab vars : plot_size*\n\tforeach var of local vars {", fixed = TRUE)

    # nothing outside the loop refers to the bare name
    expect_no_match(out, "\tcap label variable plot_size ", fixed = TRUE)
    expect_no_match(out, "\tcap destring plot_size, replace", fixed = TRUE)
  }
)

test_that(
  "only a select_one is treated as carrying a value label",
  {
    # "fixtures/audittest.xlsx" holds the types whose names contain a space
    # but name no choice list: text audit, audio audit, sensor_statistic
    local_mocked_bindings(
      cto_form_definition = function(...) test_path("fixtures", "audittest.xlsx"),
      .package = "ctoclient"
    )
    old <- options(ctoclient.verbose = FALSE)
    on.exit(options(old), add = TRUE)
    out <- paste(cto_form_dofile("audittest"), collapse = "\n")

    # the select_one keeps its destring and value label
    expect_match(out, "cap destring owns, replace", fixed = TRUE)
    expect_match(out, "cap label values owns yn", fixed = TRUE)
    # an integer is still destrung
    expect_match(out, "cap destring age, replace", fixed = TRUE)

    # the audits hold file names: labelling them is fine, destringing them
    # and giving them a value label named after the second word is not
    expect_match(out, "cap label variable audit_txt \"Text audit\"", fixed = TRUE)
    expect_no_match(out, "cap destring audit_txt", fixed = TRUE)
    expect_no_match(out, "cap destring audit_aud", fixed = TRUE)
    expect_no_match(out, "cap destring sens_acc", fixed = TRUE)
    expect_no_match(out, "label values audit_txt audit", fixed = TRUE)
    expect_no_match(out, "label values audit_aud audit", fixed = TRUE)
    expect_no_match(out, "label values sens_acc acc", fixed = TRUE)
  }
)

test_that(
  "each variable's commands are separated by a blank line",
  {
    p <- file.path(tempdir(), "spacing.do")
    on.exit(unlink(p), add = TRUE)
    dofile(path = p)
    txt <- readLines(p, warn = FALSE)

    # a variable's own commands stay together
    i <- which(txt == "cap label variable today \"Today\"")
    expect_length(i, 1L)
    expect_identical(txt[i + 1L], "cap note today: Today")

    # and the next variable begins after one blank line, not straight after
    expect_identical(txt[i + 2L], "")
    expect_false(txt[i + 3L] == "")
    expect_match(txt[i + 3L], "^cap ")
  }
)
