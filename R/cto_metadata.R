#' Retrieve Server Metadata and Resource Lists
#'
#' @description
#' These functions retrieve various metadata and lists of resources (forms,
#' groups, teams, roles, users) from the SurveyCTO server.
#'
#' * `cto_metadata()`: Retrieves a combined structure of forms, groups, and
#'   datasets (legacy console endpoint).
#' * `cto_form_ids()`: Returns a simple vector of all form IDs.
#' * `cto_group_list()`: Lists all form groups.
#' * `cto_team_list()`: Lists all available team IDs.
#' * `cto_role_list()`: Lists all defined user roles.
#' * `cto_user_list()`: Lists all users on the server.
#'
#' @param which String. Specifies which subset of metadata to return for
#'   `cto_metadata()`. One of:
#'   * `"all"` (default): Returns a list containing groups, datasets, and forms.
#'   * `"groups"`: Returns a data frame of form groups.
#'   * `"datasets"`: Returns a data frame of server datasets.
#'   * `"forms"`: Returns a data frame of deployed forms.
#' @param order_by String. Field to sort the results by. Available fields vary
#'   by function (e.g., `"createdOn"`, `"id"`, `"title"`, or `"username"`).
#' @param sort String. Sort direction: `"ASC"` (ascending) or `"DESC"` (descending).
#' @param parent_group_id Number (Optional). Filter groups by their parent group ID.
#' @param role_id String (Optional). Filter users by a specific Role ID.
#'
#' @return
#' The return value depends on the function:
#' * `cto_form_ids()` and `cto_team_list()` return a **character vector** of IDs.
#' * `cto_metadata()` returns a **list** (if `which = "all"`) or a **data frame**.
#' * `cto_group_list()`, `cto_role_list()`, and `cto_user_list()` return a **list**
#'   or **data frame** of the requested resources (depending on pagination handling).
#'
#' @family Server Metadata
#' @name cto_metadata
#'
#' @examples
#' \dontrun{
#' # --- 1. Basic Metadata ---
#' # Get all form IDs as a vector
#' ids <- cto_form_ids()
#'
#' # Get detailed metadata about forms
#' meta_forms <- cto_metadata("forms")
#'
#' # --- 2. Resource Lists ---
#' # List all groups, sorted by title
#' groups <- cto_group_list(order_by = "title", sort = "asc")
#'
#' # List all users with a specific role
#' admins <- cto_user_list(role_id = "admin_role_id")
#' }
NULL

#' @export
#' @rdname cto_metadata
cto_form_ids <- function() {
  session <- get_session()
  if (get_verbose()) {
    cli_progress_step(
      "Listing form IDs on {.field {session$server}}",
      "Listed form IDs on {.field {session$server}}"
    )
  }
  fetch_api_response(session, "api/v2/forms/ids")
}

#' @export
#' @rdname cto_metadata
cto_metadata <- function(which = c("all", "datasets", "forms", "groups")) {
  confirm_cookies()
  which <- match.arg(which)
  session <- get_session()
  if (get_verbose()) {
    cli_progress_step(
      "Fetching {col_blue(which)} metadata from {.field {session$server}}",
      "Fetched {col_blue(which)} metadata from {.field {session$server}}"
    )
  }
  metadata <- fetch_api_response(session, "console/forms-groups-datasets/get")
  if (which == "all") metadata else metadata[[which]]
}

#' @export
#' @rdname cto_metadata
cto_group_list <- function(
  order_by = c("createdOn", "id", "title"),
  sort = c("ASC", "DESC"),
  parent_group_id = NULL
) {
  verbose <- get_verbose()
  session <- get_session()
  checkmate::assert_number(parent_group_id, null.ok = TRUE)

  query <- list(
    limit = 1000,
    orderBy = match.arg(order_by),
    orderByDirection = match.arg(sort),
    parentGroupId = parent_group_id
  )
  query <- drop_nulls_recursive(query)

  if (verbose) {
    cli_progress_step(
      "Listing available groups on {.field {session$server}}",
      "Listed available groups on {.field {session$server}}"
    )
  }

  session <- req_url_query(session, !!!query)
  fetch_paginated_response(session, "api/v2/groups")
}


#' @export
#' @rdname cto_metadata
cto_team_list <- function() {
  session <- get_session()
  if (get_verbose()) {
    cli_progress_step(
      "Listing available teams on {.field {session$server}}",
      "Listed available teams on {.field {session$server}}"
    )
  }
  fetch_api_response(session, "api/v2/teams/ids")
}


#' @export
#' @rdname cto_metadata
cto_role_list <- function(
  order_by = c("createdOn", "id", "title", "createdBy"),
  sort = c("ASC", "DESC")
) {
  session <- get_session()

  query <- list(
    limit = 1000,
    orderBy = match.arg(order_by),
    orderByDirection = match.arg(sort)
  )
  query <- drop_nulls_recursive(query)

  if (get_verbose()) {
    cli_progress_step(
      "Listing available roles on {.field {session$server}}",
      "Listed available roles on {.field {session$server}}"
    )
  }

  session <- req_url_query(session, !!!query)
  fetch_paginated_response(session, "api/v2/roles")
}


#' @export
#' @rdname cto_metadata
cto_user_list <- function(
  order_by = c("createdOn", "username", "roleId", "modifiedOn"),
  sort = c("ASC", "DESC"),
  role_id = NULL
) {
  session <- get_session()
  assert_string(role_id, null.ok = TRUE)

  query <- list(
    limit = 1000,
    orderBy = match.arg(order_by),
    orderByDirection = match.arg(sort),
    roleId = role_id
  )
  query <- drop_nulls_recursive(query)

  if (get_verbose()) {
    cli_progress_step(
      "Listing available users on {.field {session$server}}",
      "Listed available users on {.field {session$server}}"
    )
  }

  session <- req_url_query(session, !!!query)
  fetch_paginated_response(session, "api/v2/users")
}
