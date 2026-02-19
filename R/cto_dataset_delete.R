#' Delete or Purge a Dataset
#'
#' @description
#' Functions to permanently remove data from the server.
#'
#' * `cto_delete_dataset()`: **Permanently deletes** a dataset and all its associated data.
#'   This operation cannot be undone
#' * `cto_purge_dataset()`: Removes **all records** from the dataset but
#'   keeps the dataset definition (schema/ID) intact.
#'
#' @param id String. The unique identifier of the dataset.
#'
#' @return A list confirming the operation status.
#'
#' @name cto_dataset_delete
#' @family Dataset Management Functions
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Delete dataset
#' cto_dataset_delete(id = "hh_data")
#'
#' # 2. Purge dataset
#' cto_dataset_purge(id = "hh_data")
#' }
cto_dataset_delete <- function(id) {
  assert_string(id)
  session <- get_session()

  if (get_verbose()) {
    cli_progress_step(
      "Deleting dataset with {col_blue(id)} ID",
      "Deleted dataset with {col_blue(id)} ID"
    )
  }

  path <- str_glue("api/v2/datasets/{id}")
  session <- httr2::req_method(session, "DELETE")
  res <- fetch_api_response(session, path)
  class(res) <- "simple.list"
  res
}

#' @export
#' @rdname cto_dataset_delete
cto_dataset_purge <- function(id) {
  assert_string(id)
  session <- get_session()

  if (get_verbose()) {
    cli_progress_step(
      "Purging dataset with {col_blue(id)} ID",
      "Purged dataset with {col_blue(id)} ID"
    )
  }

  path <- str_glue("/api/v2/datasets/{id}/purge")
  session <- httr2::req_method(session, "POST")
  res <- fetch_api_response(session, path)
  class(res) <- "simple.list"
  res
}
