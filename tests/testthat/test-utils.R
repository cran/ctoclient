
# Unit tests for the internal helpers in R/zzz.R. These are pure functions,
# so they need neither a connection nor a fixture.

# ---- gen_regex_varname() ----

test_that(
  "gen_regex_varname() anchors plain variable names",
  {
    expect_identical(gen_regex_varname("age", 0, FALSE), "^age$")
    expect_identical(gen_regex_varname("age", 1, FALSE), "^age(_[0-9]+)*$")
    expect_identical(gen_regex_varname("age", 2, FALSE), "^age(_[0-9]+)*$")
  }
)

test_that(
  "gen_regex_varname() appends the select_multiple suffix",
  {
    expect_identical(gen_regex_varname("crops", 0, TRUE), "^crops_*[0-9]+$")
    expect_identical(gen_regex_varname("crops", 1, TRUE), "^crops_*[0-9]+(_[0-9]+)*$")
    expect_identical(gen_regex_varname("crops", 0, TRUE, "_values"), "^crops_values$")
  }
)

test_that(
  "gen_regex_varname() patterns select the columns they describe",
  {
    nms <- c("age", "age_1", "age_1_2", "crops_1", "crops_2", "cropsx")
    expect_identical(
      grep(gen_regex_varname("age", 0, FALSE), nms, value = TRUE),
      "age"
    )
    # a repeat variable matches the bare name and every indexed copy
    expect_identical(
      grep(gen_regex_varname("age", 1, FALSE), nms, value = TRUE),
      c("age", "age_1", "age_1_2")
    )
    # the index repeats, so a nested copy is matched at any depth
    expect_identical(
      grep(gen_regex_varname("age", 2, FALSE), nms, value = TRUE),
      c("age", "age_1", "age_1_2")
    )
    expect_identical(
      grep(gen_regex_varname("crops", 0, TRUE), nms, value = TRUE),
      c("crops_1", "crops_2")
    )
  }
)


# ---- center_text() ----

test_that(
  "center_text() pads text to the requested width",
  {
    expect_identical(center_text("ab", "-", 6), "--ab--")
    expect_identical(nchar(center_text("abc", " ", 78)), 78L)
  }
)

test_that(
  "center_text() puts the extra character on the right",
  {
    expect_identical(center_text("ab", "-", 7), "--ab---")
  }
)

test_that(
  "center_text() returns text wider than the width unchanged",
  {
    expect_identical(center_text("abcdef", "-", 6), "abcdef")
    expect_identical(center_text("abcdefg", "-", 6), "abcdefg")
  }
)


# ---- drop_nulls_recursive() ----

test_that(
  "drop_nulls_recursive() drops NULL elements at every level",
  {
    x <- list(a = 1, b = NULL, c = list(d = NULL, e = 2))
    expect_identical(drop_nulls_recursive(x), list(a = 1, c = list(e = 2)))
  }
)

test_that(
  "drop_nulls_recursive() drops lists left empty",
  {
    expect_identical(drop_nulls_recursive(list(a = 1, b = list())), list(a = 1))
    expect_length(drop_nulls_recursive(list(a = list(b = NULL))), 0)
  }
)

test_that(
  "drop_nulls_recursive() returns non-lists unchanged",
  {
    expect_identical(drop_nulls_recursive(1:3), 1:3)
    expect_identical(drop_nulls_recursive("a"), "a")
  }
)


# ---- assert_url_safe() ----

test_that(
  "assert_url_safe() accepts names a server may legitimately use",
  {
    expect_true(assert_url_safe("sctopackagetest", "server"))
    expect_true(assert_url_safe("my-org", "server"))
    expect_true(assert_url_safe("my_org", "server"))
    expect_true(assert_url_safe("MyOrg", "server"))
    expect_true(assert_url_safe(c("hh_listing", "sample.cases"), "id"))
  }
)

test_that(
  "assert_url_safe() rejects characters that change the URL structure",
  {
    expect_error(assert_url_safe("evil.com/#", "server"), "must not contain", fixed = TRUE)
    expect_error(assert_url_safe("a?b", "server"), "must not contain", fixed = TRUE)
    expect_error(assert_url_safe("a#b", "server"), "must not contain", fixed = TRUE)
    expect_error(assert_url_safe("a/b", "id"), "must not contain", fixed = TRUE)
    expect_error(assert_url_safe("a b", "id"), "must not contain", fixed = TRUE)
    expect_error(assert_url_safe("host:8080", "server"), "must not contain", fixed = TRUE)
    expect_error(assert_url_safe("user@host", "server"), "must not contain", fixed = TRUE)
  }
)

test_that(
  "assert_url_safe() rejects empty and missing values",
  {
    expect_error(assert_url_safe("", "id"))
    expect_error(assert_url_safe(NA_character_, "id"))
  }
)

test_that(
  "assert_url_safe() names the offending element",
  {
    expect_error(assert_url_safe(c("good", "al/so"), "id"), "al/so", fixed = TRUE)
  }
)

test_that(
  "cto_connect() rejects an unsafe server before contacting the network",
  {
    expect_error(
      cto_connect("evil.com/#", "user", "pass"),
      "must not contain",
      fixed = TRUE
    )
  }
)


# ---- split_gps_columns() ----

test_that(
  "split_gps_columns() keeps the raw geopoint under its own name",
  {
    d <- data.frame(
      id = 1:2,
      gps = c("9.0 38.7 2355 4.9", "9.1 38.8 2360 5.0"),
      stringsAsFactors = FALSE
    )
    out <- split_gps_columns(d, "^gps$")

    expect_true("gps" %in% names(out))
    expect_identical(out$gps, d$gps)
    expect_false(any(grepl("gps_gps", names(out))))
    expect_true(all(c("gps_lat", "gps_long", "gps_alt", "gps_acc") %in% names(out)))
    expect_identical(out$gps_lat, c("9.0", "9.1"))
  }
)

test_that(
  "split_gps_columns() handles several geopoints at once",
  {
    d <- data.frame(
      gps = c("9.0 38.7 2355 4.9"),
      plot_gps = c("8.1 39.2 1800 6.0"),
      stringsAsFactors = FALSE
    )
    out <- split_gps_columns(d, c("^gps$", "^plot_gps$"))

    expect_identical(out$gps, d$gps)
    expect_identical(out$plot_gps, d$plot_gps)
    expect_identical(out$plot_gps_long, "39.2")
  }
)

test_that(
  "split_gps_columns() survives a malformed point",
  {
    d <- data.frame(
      gps = c("9.0 38.7 2355 4.9", "9.0 38.7 2355 4.9 99", "9.0 38.7"),
      stringsAsFactors = FALSE
    )
    out <- expect_no_error(split_gps_columns(d, "^gps$"))

    expect_identical(out$gps, d$gps)
    expect_identical(out$gps_lat, c("9.0", "9.0", "9.0"))
    expect_true(is.na(out$gps_acc[3]))
  }
)

test_that(
  "split_gps_columns() returns the data untouched when there is nothing to split",
  {
    d <- data.frame(id = 1:2, name = c("a", "b"), stringsAsFactors = FALSE)
    expect_identical(split_gps_columns(d, character(0)), d)
    expect_identical(split_gps_columns(d, "^gps$"), d)
  }
)


# ---- fetch_paginated_response() ----

test_that(
  "fetch_paginated_response() follows the cursor to the end",
  {
    calls <- 0L
    local_mocked_bindings(
      fetch_api_response = function(req, url_path = NULL, file_path = NULL) {
        calls <<- calls + 1L
        if (calls == 1L) {
          list(data = data.frame(i = 1L), nextCursor = "c1")
        } else {
          list(data = data.frame(i = 2L), nextCursor = NULL)
        }
      }
    )
    out <- fetch_paginated_response(httr2::request("https://example.com"), "x")

    expect_equal(nrow(out), 2L)
    expect_equal(calls, 2L)
  }
)

test_that(
  "fetch_paginated_response() stops when the cursor stops advancing",
  {
    calls <- 0L
    local_mocked_bindings(
      fetch_api_response = function(req, url_path = NULL, file_path = NULL) {
        calls <<- calls + 1L
        list(data = data.frame(i = calls), nextCursor = "stuck")
      }
    )
    expect_warning(
      fetch_paginated_response(httr2::request("https://example.com"), "x"),
      "same cursor"
    )
    expect_lt(calls, 5L)
  }
)

test_that(
  "fetch_paginated_response() stops at max_pages",
  {
    calls <- 0L
    local_mocked_bindings(
      fetch_api_response = function(req, url_path = NULL, file_path = NULL) {
        calls <<- calls + 1L
        list(data = data.frame(i = calls), nextCursor = paste0("c", calls))
      }
    )
    expect_warning(
      fetch_paginated_response(httr2::request("https://example.com"), "x", max_pages = 3),
      "Stopped after"
    )
    expect_equal(calls, 3L)
  }
)


# ---- stata_escape_label() ----

test_that(
  "stata_escape_label() neutralises Stata macro syntax",
  {
    expect_identical(stata_escape_label("Cost ($USD)"), "Cost (\\$USD)")
    expect_identical(stata_escape_label("${calculated}"), "\\${calculated}")
    expect_identical(stata_escape_label("a `b` c"), "a 'b' c")
    expect_identical(stata_escape_label('He said "hi"'), "He said 'hi'")
    expect_identical(stata_escape_label("<b>Bold</b> text"), "Bold text")
    expect_identical(stata_escape_label("Plain label"), "Plain label")
  }
)


# ---- build_datetime_block() ----

test_that(
  "build_datetime_block() uses clock/%tc for datetimes and date/%td for dates",
  {
    out <- build_datetime_block("SubmissionDate", "visit_date", "2026")

    expect_true(any(grepl("local dtvarlist SubmissionDate", out, fixed = TRUE)))
    expect_true(any(grepl("clock(`tempdtvar',\"MDYhms\",2026)", out, fixed = TRUE)))
    expect_true(any(grepl("format %tc `dtvar'", out, fixed = TRUE)))

    expect_true(any(grepl("local dtvarlist visit_date", out, fixed = TRUE)))
    expect_true(any(grepl("date(`tempdtvar',\"MDY\",2026)", out, fixed = TRUE)))
    expect_true(any(grepl("format %td `dtvar'", out, fixed = TRUE)))
  }
)

test_that(
  "build_datetime_block() skips variables the dataset does not have",
  {
    out <- build_datetime_block("SubmissionDate", character(0), "2026")

    expect_true(any(grepl("cap confirm string variable `dtvar'", out, fixed = TRUE)))
    expect_true(any(grepl("if !_rc {", out, fixed = TRUE)))
  }
)

test_that(
  "build_datetime_block() emits only the lists it is given",
  {
    expect_length(build_datetime_block(character(0), character(0), "2026"), 0)
    expect_false(any(grepl("clock(", build_datetime_block(character(0), "today", "2026"), fixed = TRUE)))
    expect_false(any(grepl("date(", build_datetime_block("endtime", character(0), "2026"), fixed = TRUE)))
  }
)

test_that(
  "build_datetime_block() lists several variables in one local",
  {
    out <- build_datetime_block(c("SubmissionDate", "starttime", "endtime"), character(0), "2026")
    expect_true(any(grepl("local dtvarlist SubmissionDate starttime endtime", out, fixed = TRUE)))
  }
)


# ---- form_null_vars() ----

test_that(
  "form_null_vars() finds the fields that carry no data",
  {
    type <- c(
      "note", "begin_group", "integer", "end_group",
      "begin repeat", "note", "integer", "end repeat", "text"
    )
    name <- c("n1", "g1", "q1", "", "rpt", "n2", "q2", "", "q3")

    # a begin repeat contributes its counter, not its own name
    expect_equal(form_null_vars(name, type), c("g1", "n1", "n2", "rpt_count"))
  }
)

test_that(
  "form_null_vars() matches both the spaced and underscored spellings",
  {
    spaced <- form_null_vars(c("g", "r"), c("begin group", "begin repeat"))
    under <- form_null_vars(c("g", "r"), c("begin_group", "begin_repeat"))

    expect_equal(spaced, under)
    expect_equal(under, c("g", "r_count"))
  }
)

test_that(
  "form_null_vars() ignores unnamed rows and real questions",
  {
    type <- c("end_group", "end repeat", "integer", "select_one yn", "geopoint")
    name <- c("", NA, "q1", "q2", "q3")
    expect_length(form_null_vars(name, type), 0)
  }
)


# ---- build_null_block() ----

test_that(
  "build_null_block() wraps a long list by appending to the same local",
  {
    txt <- paste(
      build_null_block(paste0("n", 1:8), per_line = 6),
      collapse = "\n"
    )

    expect_match(txt, "local nullvars1 n1 n2 n3 n4 n5 n6", fixed = TRUE)
    expect_match(txt, "local nullvars1 `nullvars1' n7 n8", fixed = TRUE)
    # wrapping a line must not open a second local
    expect_no_match(txt, "local nullvars2", fixed = TRUE)
  }
)

test_that(
  "build_null_block() opens a new local once one is full",
  {
    txt <- paste(
      build_null_block(paste0("n", 1:5), per_line = 2, per_local = 4),
      collapse = "\n"
    )

    expect_match(txt, "local nullvars1 n1 n2", fixed = TRUE)
    expect_match(txt, "local nullvars1 `nullvars1' n3 n4", fixed = TRUE)
    expect_match(txt, "local nullvars2 n5", fixed = TRUE)
  }
)

test_that(
  "build_null_block() reads those locals with a single loop",
  {
    out <- build_null_block(paste0("n", 1:8), per_line = 6)
    txt <- paste(out, collapse = "\n")

    expect_equal(sum(grepl("foreach stub of local", out, fixed = TRUE)), 1L)
    expect_match(txt, "forvalues i = 1/100 {", fixed = TRUE)
    # the loop index names the local to read, and empty ones are skipped
    expect_match(txt, "if \"`nullvars`i\'\'\" != \"\" {", fixed = TRUE)
    expect_match(txt, "foreach stub of local nullvars`i'", fixed = TRUE)
  }
)

test_that(
  "build_null_block() matches a bare stub at any repeat depth",
  {
    txt <- paste(build_null_block("n1"), collapse = "\n")
    expect_match(txt, "if regexm(\"`var'\", \"^`stub'(_[0-9]+)*$\")", fixed = TRUE)

    # bare, indexed, and nested all count; anything else does not
    rx <- "^n1(_[0-9]+)*$"
    expect_true(grepl(rx, "n1"))
    expect_true(grepl(rx, "n1_7"))
    expect_true(grepl(rx, "n1_1_2"))
    expect_false(grepl(rx, "n1_other"))
    expect_false(grepl(rx, "n1x"))
  }
)

test_that(
  "build_null_block() confirms a variable is empty before dropping it",
  {
    txt <- paste(build_null_block("n1"), collapse = "\n")

    expect_match(txt, "cap unab matched : `stub'*", fixed = TRUE)
    expect_match(txt, "qui count if !missing(`var')", fixed = TRUE)
    expect_match(txt, "if r(N) == 0 {", fixed = TRUE)
    expect_match(txt, "local null_confirmed `null_confirmed' `var'", fixed = TRUE)
    expect_match(txt, "drop `null_confirmed'", fixed = TRUE)
  }
)

test_that(
  "build_null_block() never declares more locals than the loop reads",
  {
    # asking for 1 name per local would need 50 of them; the block has to
    # widen the locals instead of declaring more than the loop can read
    out <- build_null_block(
      paste0("n", 1:50),
      per_line = 4,
      per_local = 1,
      max_locals = 5
    )

    declared <- grep("^\tlocal nullvars", out, value = TRUE)
    macros <- unique(sub("^\tlocal (nullvars[0-9]+).*", "\\1", declared))
    expect_length(macros, 5L)
    expect_true(any(grepl("forvalues i = 1/5 {", out, fixed = TRUE)))

    # every candidate still reaches a local, whether the line opened it
    # or appended to it
    names_only <- sub("^\tlocal nullvars[0-9]+ (`nullvars[0-9]+' )?", "", declared)
    expect_setequal(unlist(strsplit(names_only, " ")), paste0("n", 1:50))
  }
)

test_that(
  "build_null_block() holds far more candidates than it has locals",
  {
    out <- build_null_block(paste0("n", 1:2000))

    macros <- unique(sub(
      "^\tlocal (nullvars[0-9]+).*", "\\1",
      grep("^\tlocal nullvars", out, value = TRUE)
    ))
    expect_lte(length(macros), 100L)

    declared <- grep("^\tlocal nullvars", out, value = TRUE)
    names_only <- sub("^\tlocal nullvars[0-9]+ (`nullvars[0-9]+' )?", "", declared)
    expect_setequal(unlist(strsplit(names_only, " ")), paste0("n", 1:2000))
  }
)

test_that(
  "build_null_block() emits nothing when there is nothing to drop",
  {
    expect_length(build_null_block(character(0)), 0)
  }
)


# ---- repeat counters in cto_form_data() ----

test_that(
  "a begin repeat resolves to its exported counter column",
  {
    # cto_form_data() builds this pattern from the repeat level. It used to be
    # produced by editing "[0-9]+$" out of gen_regex_varname()'s output, which
    # silently stopped working once that pattern gained a quantifier.
    counter <- function(name, level) {
      paste0("^", name, strrep("_[0-9]+", pmax(level - 1, 0)), "_count")
    }

    expect_identical(counter("rpt", 1), "^rpt_count")
    expect_identical(counter("rpt", 2), "^rpt_[0-9]+_count")

    expect_true(grepl(counter("rpt", 1), "rpt_count"))
    expect_true(grepl(counter("rpt", 2), "rpt_1_count"))

    # and the question pattern must not be mistaken for the counter
    expect_false(grepl(gen_regex_varname("rpt", 1, FALSE), "rpt_count"))
  }
)


# ---- SurveyCTO timestamps ----

test_that(
  "normalise_cto_timestamp() rewrites an export timestamp without a locale",
  {
    expect_identical(
      normalise_cto_timestamp("March 12, 2026 10:05:00 AM"),
      "2026-03-12 10:05:00"
    )
    # noon stays at 12, midnight becomes 00
    expect_identical(
      normalise_cto_timestamp("July 4, 2026 12:00:00 PM"),
      "2026-07-04 12:00:00"
    )
    expect_identical(
      normalise_cto_timestamp("July 4, 2026 12:30:00 AM"),
      "2026-07-04 00:30:00"
    )
    expect_identical(
      normalise_cto_timestamp("December 31, 2026 11:59:59 PM"),
      "2026-12-31 23:59:59"
    )
    # a date with no time keeps just the date
    expect_identical(normalise_cto_timestamp("March 12, 2026"), "2026-03-12")
    # strptime takes the abbreviated month for %B, so this does too
    expect_identical(normalise_cto_timestamp("Mar 12, 2026"), "2026-03-12")
  }
)

test_that(
  "normalise_cto_timestamp() returns NA for anything it cannot read",
  {
    expect_true(is.na(normalise_cto_timestamp(NA_character_)))
    expect_true(is.na(normalise_cto_timestamp("")))
    expect_true(is.na(normalise_cto_timestamp("Marchx 12, 2026")))
    expect_true(is.na(normalise_cto_timestamp("2026-03-12")))
  }
)

test_that(
  "the parsers read an export timestamp whatever the platform does",
  {
    # These assert the value the parser must produce, never what the
    # platform's own strptime makes of the string: a CRAN M1 mac reads
    # "%B %d, %Y %I:%M:%S %p" as NA throughout, so comparing against it
    # compares against a moving target.
    x <- c("March 12, 2026 10:05:00 AM", "July 4, 2026 12:00:00 PM", NA)
    expect_identical(
      format(parse_cto_datetime(x), "%Y-%m-%d %H:%M:%S"),
      c("2026-03-12 10:05:00", "2026-07-04 12:00:00", NA)
    )

    d <- c("March 12, 2026", "December 1, 2026", NA)
    expect_identical(
      as.character(parse_cto_date(d)),
      c("2026-03-12", "2026-12-01", NA)
    )
  }
)

test_that(
  "the parsers read a timestamp the platform format cannot",
  {
    # Padding is one thing no platform's strptime accepts for this format,
    # so these only pass through the rewritten, all-numeric path.
    expect_identical(
      format(parse_cto_datetime("  March 12, 2026 10:05:00 AM  "), "%H:%M:%S"),
      "10:05:00"
    )
    expect_identical(
      as.character(parse_cto_date("  March 12, 2026  ")),
      "2026-03-12"
    )
  }
)

test_that(
  "the parsers leave a column that is already a date alone",
  {
    now <- as.POSIXct("2026-03-12 10:05:00")
    expect_identical(parse_cto_datetime(now), now)
    expect_identical(parse_cto_date(as.Date("2026-03-12")), as.Date("2026-03-12"))
  }
)
