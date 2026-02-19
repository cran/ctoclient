
if (Sys.getenv("SERVER") != "") {

  library(httptest2)

  set_redactor(function(x) {
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
          Sys.getenv("SERVER"),
          Sys.getenv("USER"),
          Sys.getenv("PASS")
        )
        without_cookies <- cto_connect(
          Sys.getenv("SERVER"),
          Sys.getenv("USER"),
          Sys.getenv("PASS"),
          FALSE
        )
        expect_true(cto_is_connected())
        expect_error(confirm_cookies())
        expect_no_error(get_session())
        cto_set_connection(with_cookies)
        expect_no_error(confirm_cookies())
        expect_error(cto_set_connection("invalid"))
        writeLines("", "_connect/NA.html")
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
  with_mock_dir(
    "_datasets",
    test_that(
      "Datasets endpoints work",
      {
        expect_s3_class(cto_dataset_list(), "data.frame")
        expect_no_error(cto_dataset_info(cto_dataset_list()$id[1]))
        csv <- file.path(tempdir(), "band_members.csv")
        utils::write.csv(dplyr::band_members, csv)
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

  if (cto_is_connected()) {
    test_that(
      "Non portable file",
      expect_no_error(cto_form_dofile("locating_households"))
    )
  }


}

