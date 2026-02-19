#' Get Dataset Properties
#'
#' @description
#' Retrieves detailed metadata for a specific dataset, including its configuration,
#' schema, and status.
#'
#' @param id String. The unique identifier of the dataset.
#'
#' @return A list containing the dataset properties.
#'
#' @family Dataset Management Functions
#' @export
#'
#' @examples
#' \dontrun{
#' ds_info <- cto_dataset_info(id = "hh_data")
#' }
cto_dataset_info <- function(id) {
  session <- get_session()
  assert_string(id)

  if (get_verbose()) {
    cli_progress_step(
      "Fetching {col_blue(id)} dataset information",
      "Fetching {col_blue(id)} dataset information"
    )
  }

  path <- str_glue("api/v2/datasets/{id}")
  res <- fetch_api_response(session, path)
  class(res) <- "simple.list"
  res
}
