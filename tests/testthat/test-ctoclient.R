
# Recorded fixtures are replayed offline, so this file runs everywhere
# httptest2 is available. It is a suggested package, so guard its use.

if (requireNamespace("httptest2", quietly = TRUE)) {

  library(httptest2)

  # Only used when re-recording fixtures against a live server. The defaults
  # are placeholders: during replay the redactor strips the server name from
  # every URL, so the values below never reach the network.
  cto_server <- Sys.getenv("CTO_SERVER", "sctopackagetest")
  cto_user <- Sys.getenv("CTO_USER", "user@example.com")
  cto_pass <- Sys.getenv("CTO_PASS", "password")

  set_redactor(function(x) {
    x <- redact_cookies(x)                    # httptest2 default, kept
    x <- redact_headers(x, "x-csrf-token")    # Session token, never recorded
    x$url <- gsub("\\?t=[0-9]+", "", x$url)   # This is dynamic
    x$url <- gsub("\\?.*$", "", x$url)        # Hard to capture queries
    x$url <- gsub("[^/]+\\.surveycto\\.com", "", x$url)
    x$url <- gsub("forms/", "", x$url)
    x$url <- gsub("datasets/", "", x$url)
    x$url <- gsub("dataset-attachment/", "", x$url)
    x <- gsub_response(x, "api/v2/", "")
    return(x)
  })


  # ---- CONNECT ----

  with_mock_dir(
    "_connect",
    test_that(
      "Authentication works with and without cookies",
      {
        with_cookies <- cto_connect(
          cto_server,
          cto_user,
          cto_pass
        )
        without_cookies <- cto_connect(
          cto_server,
          cto_user,
          cto_pass,
          FALSE
        )
        expect_true(cto_is_connected())
        expect_error(confirm_cookies())
        expect_no_error(get_session())
        cto_set_connection(with_cookies)
        expect_no_error(confirm_cookies())
        expect_error(cto_set_connection("invalid"))
      })
  )

  #---- FORMS ----
  with_mock_dir(
    "_form",
    test_that(
      "Form endpoints work",
      {
        expect_vector(cto_form_ids())
        expect_no_error(cto_form_attachment("household_listing", dir = tempdir(), overwrite = TRUE))
        expect_no_error(cto_form_data("household_listing"))
        expect_warning(cto_form_data_attachment("household_listing", dir = tempdir()))
        expect_error(cto_form_data("invalidform"))
      }
    )
  )

  #---- DATASETS ----
  # Deleting a mock directory puts these blocks back into recording mode, which
  # creates, uploads, purges and deletes a dataset on the live server.
  with_mock_dir(
    "_datasets",
    test_that(
      "Datasets endpoints work",
      {
        expect_s3_class(cto_dataset_list(), "data.frame")
        expect_no_error(cto_dataset_info(cto_dataset_list()$id[1]))
        csv <- file.path(tempdir(), "band_members.csv")
        # httptest2 names the mock file after a hash of the request body, and
        # for an upload that body carries a hash of the file's contents. A
        # text-mode connection writes CRLF on Windows and LF everywhere else,
        # which would give one test two different mock file names, so the file
        # is written in binary mode to fix the bytes on every platform.
        con <- file(csv, open = "wb")
        utils::write.csv(dplyr::band_members, con)
        close(con)
        expect_no_error(cto_dataset_create("band_members"))
        expect_no_error(cto_dataset_upload("band_members", csv))
        expect_no_error(cto_dataset_download(dir = tempdir(), overwrite = TRUE))
        expect_no_error(cto_dataset_purge("band_members"))
        expect_no_error(cto_dataset_delete("band_members"))
      }
    )
  )

  # ---- SERVER METADATA ----
  with_mock_dir(
    "_metadata",
    test_that(
      "Server metadata endpoints work",
      {
        expect_no_error(cto_metadata())
        expect_no_error(cto_group_list())
        expect_no_error(cto_team_list())
        # Unavailable for this subscription
        expect_error(cto_role_list())
        expect_error(cto_user_list())
      }
    )
  )

  # ---- FORM FILES ----

  with_mock_dir(
    "_form_files",
    test_that(
      "Form files endpoints work",
      {
        form_id <- "locating_households"
        expect_no_error(cto_form_languages(form_id))
        expect_no_error(cto_form_stata_template(form_id, tempdir()))
        expect_no_error(cto_form_printable(form_id, tempdir()))
        expect_no_error(cto_form_mail_template(form_id, tempdir()))
        expect_error(cto_form_definition(form_id, "invalid"))
      }
    )
  )

  # ---- LIVE SERVER ONLY ----
  # cto_form_dofile() reads the XLSForm definition, and httptest2 has no
  # fixture format for a workbook, so this one needs a real server.

  test_that(
    "Stata do-file is generated",
    {
      skip_if_not(
        nzchar(Sys.getenv("CTO_SERVER")),
        "CTO_SERVER is not set"
      )
      cto_connect(cto_server, cto_user, cto_pass)
      expect_no_error(cto_form_dofile("locating_households"))
    }
  )

}
