#' List Available Server Datasets
#'
#' @description
#' Retrieves a list of datasets that the authenticated user has access to.
#' Results can be filtered by team and ordered by specified fields.
#'
#' @param order_by String. The field to sort the results by. Options are:
#'   `"id"`, `"title"`, `"createdOn"`, `"modifiedOn"`, `"status"`, `"version"`,
#'   or `"discriminator"`. Defaults to `"createdOn"`.
#' @param sort String. The direction of the sort: `"asc"` (ascending) or
#'   `"desc"` (descending). Defaults to `"asc"`.
#' @param team_id String (Optional). Filter datasets by a specific Team ID.
#'   If provided, only datasets accessible to that team are returned.
#'   Example: `'team-456'`.
#'
#' @return A data frame containing the metadata of available datasets.
#'
#' @family Dataset Management Functions
#' @export
#'
#' @examples
#' \dontrun{
#' # List all datasets sorted by creation date
#' ds_list <- cto_dataset_list()
#'
#' # List datasets for a specific team, ordered by title
#' team_ds <- cto_dataset_list(team_id = "team-123", order_by = "title")
#' }
cto_dataset_list <- function(
  order_by = "createdOn",
  sort = c("ASC", "DESC"),
  team_id = NULL
) {
  session <- get_session()
  assert_string(team_id, null.ok = TRUE)

  order_choices <- c(
    "id",
    "title",
    "createdOn",
    "modifiedOn",
    "status",
    "version",
    "discriminator"
  )
  query <- list(
    limit = 1000,
    orderBy = match.arg(order_by, order_choices),
    orderByDirection = match.arg(sort),
    teamId = team_id
  )
  query <- drop_nulls_recursive(query)

  if (get_verbose()) {
    cli_progress_step(
      "Listing available datasets on {.field {session$server}}",
      "Listed available datasets on {.field {session$server}}"
    )
  }

  session <- req_url_query(session, !!!query)
  resp <- fetch_paginated_response(session, "api/v2/datasets")
  if (length(resp) == 0) {
    cli_warn("No datasets found on {.field {session$server}}")
  }
  return(resp)
}
