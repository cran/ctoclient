#' Create or Upload to Server Datasets
#'
#' @description
#' These functions manage the lifecycle of SurveyCTO server datasets: creating the
#' container definition and populating it with data.
#'
#' * `cto_dataset_create()`: Creates a new dataset with the specified configuration.
#' * `cto_dataset_upload()`: Uploads records from a CSV file to the specified dataset. Supports:
#'   * APPEND: add new records
#'   * MERGE: update existing records based on unique field
#'   * CLEAR: replace all data
#'
#' @param id String. The unique identifier for the dataset (e.g., "household_data").
#' @param title String. The display title of the dataset. Defaults to `id`.
#' @param id_format_options List. Options for formatting IDs within the dataset.
#' @param cases_management_options List. Specific configurations for case management
#' @param location_context List. Metadata regarding where the dataset resides.
#' @param discriminator String. The type of dataset to create.
#' @param unique_record_field String. The name of the field that
#'   uniquely identifies records. Required if `upload_mode` is "merge".
#' @param allow_offline_updates Logical. Whether the dataset allows
#'   updates while offline.
#' @param file String. Path to the local CSV file to upload.
#' @param upload_mode String. How the data should be handled.
#' @param joining_field String. The column name used to match records
#'   during a "merge". Often the same as `unique_record_field`.
#'
#' @return A list containing the API response (metadata for creation, or job summary for upload).
#'
#' @family Dataset Management Functions
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Create the container
#' cto_dataset_create(
#' id = "hh_data",
#' title = "Household Data",
#' unique_record_field = "hh_id"
#' )
#'
#' # 2. Upload data to it
#' cto_dataset_upload(
#' file = "data.csv",
#' id = "hh_data",
#' upload_mode = "merge",
#' joining_field = "hh_id"
#' )
#' }
cto_dataset_create <- function(
  id,
  title = id,
  discriminator = NULL,
  unique_record_field = NULL,
  allow_offline_updates = NULL,
  id_format_options = list(
    prefix = NULL,
    allowCapitalLetters = NULL,
    suffix = NULL,
    numberOfDigits = NULL
  ),
  cases_management_options = list(
    otherUserCode = NULL,
    showFinalizedSentWhenTree = NULL,
    enumeratorDatasetId = NULL,
    showColumnsWhenTable = NULL,
    displayMode = NULL,
    entryMode = NULL
  ),
  location_context = list(
    parentGroupId = 1,
    siblingBelow = list(
      itemClass = NULL,
      id = NULL
    ),
    siblingAbove = list(
      itemClass = NULL,
      id = NULL
    )
  )
) {
  session <- get_session()

  assert_string(id)
  assert_string(title)
  assert_string(unique_record_field, null.ok = TRUE)
  assert_flag(allow_offline_updates, null.ok = TRUE)

  ds <- c("CASES", "ENUMERATORS", "DATA")
  query <- list(
    idFormatOptions = id_format_options,
    allowOfflineUpdates = allow_offline_updates,
    id = id,
    title = title,
    casesManagementOptions = cases_management_options,
    locationContext = location_context,
    discriminator = if (!is.null(discriminator)) match.arg(discriminator, ds),
    uniqueRecordField = unique_record_field
  )
  query <- drop_nulls_recursive(query)

  if (get_verbose()) {
    cli_progress_step(
      "Creating {col_blue(id)} dataset",
      "Created {col_blue(id)} dataset"
    )
  }

  session <- httr2::req_body_json(session, query)
  session <- httr2::req_method(session, "POST")

  res <- fetch_api_response(session, "api/v2/datasets")
  class(res) <- "simple.list"
  res
}

#' @export
#' @rdname cto_dataset_create
cto_dataset_upload <- function(
  id,
  file,
  upload_mode = c("APPEND", "MERGE", "CLEAR"),
  joining_field = NULL
) {
  session <- get_session()
  checkmate::assert_file_exists(file, 'r', "csv")
  assert_string(joining_field, null.ok = TRUE)

  metadata <- list(
    joiningField = joining_field,
    uploadMode = match.arg(upload_mode)
  )

  metadata <- drop_nulls_recursive(metadata)
  metadata_json <- jsonlite::toJSON(metadata, auto_unbox = TRUE)

  if (get_verbose()) {
    cli_progress_step(
      "Uploading {col_blue(id)} dataset...",
      "Uploaded {col_blue(id)} dataset"
    )
  }

  path <- str_glue("api/v2/datasets/{id}/records/upload")
  session <- httr2::req_body_multipart(
    session,
    metadata = curl::form_data(metadata_json, type = "application/json"),
    file = curl::form_file(file, type = "text/csv", name = "file")
  )

  session <- httr2::req_method(session, "POST")
  res <- fetch_api_response(session, path)
  class(res) <- "simple.list"
  res
}
