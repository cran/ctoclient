
# End-to-end test of the tidying pipeline in cto_form_data(). Everything that
# reaches the network is mocked, so the whole transform runs against
# "fixtures/testform.xlsx" and the synthetic wide export below.

fake_session <- function() {
  req <- httr2::request("https://demo.surveycto.com")
  req$server <- "demo"
  structure(req, class = c(class(req), "cto_session"))
}

# One row per submission, named as SurveyCTO names them in a wide export.
# crops_2 and illness_2 are deliberately absent: no respondent picked them,
# which is what the backfill step exists for.
raw_export <- function() {
  data.frame(
    KEY = c("uuid:1", "uuid:2"),
    SubmissionDate = c("March 12, 2026 10:33:21 AM", "March 13, 2026 09:00:00 AM"),
    CompletionDate = c("March 12, 2026 10:40:00 AM", "March 13, 2026 09:30:00 AM"),
    starttime = c("March 12, 2026 10:00:00 AM", "March 13, 2026 08:30:00 AM"),
    endtime = c("March 12, 2026 10:33:00 AM", "March 13, 2026 09:00:00 AM"),
    today = c("March 12, 2026", "March 13, 2026"),
    visit_date = c("March 12, 2026", "March 13, 2026"),
    interview_dt = c("March 12, 2026 10:05:00 AM", "March 13, 2026 08:35:00 AM"),
    hh_size = c("5", "7"),
    land_area = c("1.25", "0.5"),
    resp_name = c("Abebe", "Chala"),
    any_selected = c("keep me", "and me"),
    owns_land = c("1", "0"),
    crops_1 = c("1", NA),
    gps = c("9.03 38.74 2355 4.9", "8.98 38.80 2400 5.1"),
    illness_1 = c("1", NA),
    plot_size_1 = c("2", "3"),
    stringsAsFactors = FALSE
  )
}

tidy_export <- function(raw = raw_export(), ...) {
  local_mocked_bindings(
    get_session = function() fake_session(),
    assert_form_id = function(form_id, session = NULL) invisible(TRUE),
    confirm_cookies = function() invisible(TRUE),
    fetch_api_response = function(req, url_path = NULL, file_path = NULL) raw,
    cto_form_definition = function(...) test_path("fixtures", "testform.xlsx"),
    .package = "ctoclient"
  )
  old <- options(ctoclient.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  cto_form_data("testform", ...)
}


test_that(
  "datetime questions keep their time and date questions do not gain one",
  {
    out <- tidy_export()

    # a datetime question must not be downgraded to Date
    expect_s3_class(out$interview_dt, "POSIXct")
    expect_equal(format(out$interview_dt[1], "%H:%M:%S"), "10:05:00")

    expect_s3_class(out$starttime, "POSIXct")
    expect_s3_class(out$endtime, "POSIXct")
    expect_s3_class(out$SubmissionDate, "POSIXct")

    expect_s3_class(out$today, "Date")
    expect_s3_class(out$visit_date, "Date")
    expect_equal(as.character(out$visit_date), c("2026-03-12", "2026-03-13"))
  }
)

test_that(
  "the raw geopoint survives the split under its own name",
  {
    raw <- raw_export()
    out <- tidy_export(raw)

    expect_true("gps" %in% names(out))
    expect_identical(out$gps, raw$gps)
    expect_false(any(grepl("gps_gps", names(out), fixed = TRUE)))

    expect_equal(out$gps_lat, c(9.03, 8.98))
    expect_equal(out$gps_long, c(38.74, 38.80))
    expect_equal(out$gps_alt, c(2355, 2400))
    expect_equal(out$gps_acc, c(4.9, 5.1))
  }
)

test_that(
  "an or_other list does not produce an all-missing column",
  {
    out <- tidy_export()

    # the list name is "diseases", not the trailing "or_other"
    expect_false(any(grepl("_NA$", names(out))))
    expect_true(all(c("illness_1", "illness_2") %in% names(out)))
  }
)

test_that(
  "a choice value Stata and SurveyCTO cannot name is not backfilled",
  {
    out <- tidy_export()

    # the crops list carries a 1.5 choice; no export can contain crops_1.5
    expect_false("crops_1.5" %in% names(out))
    expect_false(any(grepl("crops_1[.]5", names(out))))
  }
)

test_that(
  "a binary column nobody selected is still added",
  {
    raw <- raw_export()
    out <- tidy_export(raw)

    expect_false("crops_2" %in% names(raw))
    expect_true("crops_2" %in% names(out))
    expect_true("illness_2" %in% names(out))
  }
)

test_that(
  "a question named any_selected keeps its values",
  {
    raw <- raw_export()
    out <- tidy_export(raw)

    # the zero-fill step uses a working column; it must not be this one
    expect_true("any_selected" %in% names(out))
    expect_identical(out$any_selected, raw$any_selected)
  }
)

test_that(
  "tidy = FALSE returns the export untouched",
  {
    raw <- raw_export()
    expect_identical(tidy_export(raw, tidy = FALSE), raw)
  }
)

test_that(
  "a form with no submissions warns instead of tidying",
  {
    expect_warning(out <- tidy_export(list()), "No submissions")
    expect_length(out, 0)
  }
)
