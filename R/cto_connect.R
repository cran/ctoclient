#' Connect to and manage a SurveyCTO Server connection
#'
#' @description
#' * `cto_connect()` authenticates against a SurveyCTO server, verifies credentials,
#' and handles cookies.
#'
#' * `cto_set_connection()` manually sets or restores an existing session object.
#'
#' * `cto_is_connected()` checks if an active session currently exists in the
#' internal environment.
#'
#' @details
#' ## Session Management
#' By default, this package operates **statefully** by preserving cookies
#' if the connection is established with the `cookies = TRUE` argument.
#' Upon successful authentication, the request object (`.session`) is assigned to an internal
#' package environment (`.ctoclient_env`). Therefore, you do not need to pass a request
#' object to other functions in this package; they will automatically use the
#' active session. If you are working with multiple servers, please use `cto_set_connection()`
#' to switch the server connection.
#'
#' ## Security Best Practices
#' It is highly recommended to avoid hard-coding passwords in your scripts.
#'
#' * **Interactive Session:** Pass the password securely via console input (leave it `NULL`) or
#'   keychain management tools.
#' * **Automation/Scripts:** Store your credentials in your `.Renviron` file
#'   (e.g., `SCTO_PASSWORD`) and retrieve them with [Sys.getenv()].
#'
#' @param server String. The subdomain of your SurveyCTO server.
#'   For example, if the full URL is `https://my-org.surveycto.com`,
#'   set this to `"my-org"`.
#' @param username String. The username or email address associated with the account.
#' @param password String. The user password. If left `NULL` (recommended),
#' it prompts you for the password interactively.
#' @param cookies Logical. If `TRUE` (default), the client preserves cookies across
#'   requests and handles CSRF tokens automatically. This is required for maintaining
#'   stateful sessions to access endpoints that not available through the REST API.
#' @param session A `cto_session` object previously created by `cto_connect()`.
#'
#' @return
#' * `cto_connect()`: The session object (invisibly).
#' * `cto_set_connection()`: `NULL` (invisibly), called for its side effect of setting the session.
#' * `cto_is_connected()`: A logical `TRUE` or `FALSE`.
#'
#' @seealso [httr2::req_auth_basic()], [usethis::edit_r_environ()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Standard authentication
#' cto_connect("my-org", "user@org.com", Sys.getenv("SCTO_PASSWORD"))
#'
#' # 2. Check if connected
#' cto_is_connected()
#'
#' # 3. Recommended for interactive use
#' con <- cto_connect("my-org", "user@org.com")
#'
#' # 4. Restore and existing connection
#' cto_set_connection(con)
#' }
cto_connect <- function(
  server,
  username,
  password = NULL,
  cookies = TRUE
) {
  assert_string(server)
  assert_string(username)
  assert_string(password, null.ok = TRUE)
  assert_flag(cookies)

  if (get_verbose()) {
    cli_progress_step(
      "Requesting access to {.field {server}}...",
      "Access to {.field {server}} granted successfully",
      "Failed to access {.field {server}}"
    )
  }

  base_url <- str_glue("https://{server}.surveycto.com")

  req <- httr2::request(base_url) |>
    httr2::req_user_agent("ctoclient package in R") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_throttle(capacity = 30, fill_time_s = 60) |>
    httr2::req_error(
      body = function(resp) {
        if (
          httr2::resp_has_body(resp) &&
            httr2::resp_content_type(resp) == "application/json"
        ) {
          r <- httr2::resp_body_json(resp)
          if (is.list(r$error)) {
            code <- str_glue("Code: {r$error$code}")
            message <- str_glue("Message: {r$error$message}")
          } else if (is.character(r$error)) {
            code <- str_glue("Code: {r$code}")
            message <- str_glue("Message: {r$error}")
          }
          return(c(code, message))
        }
        return(NULL)
      }
    )

  if (cookies) {
    req <- httr2::req_cookie_preserve(req, tempfile())
  }

  resp <- httr2::req_perform(req)

  if (cookies) {
    csrf_exist <- httr2::resp_header_exists(resp, "x-csrf-token")
    if (csrf_exist) {
      csrf <- httr2::resp_header(resp, "x-csrf-token")
      req <- httr2::req_headers(req, `x-csrf-token` = csrf)
    }
  }

  req$server <- server
  class(req) <- c(class(req), "cto_session")
  assign(".session", req, envir = .ctoclient_env)
  invisible(req)
}


#' @export
#' @rdname cto_connect
cto_set_connection <- function(session) {
  if (!inherits(session, "cto_session")) {
    cli_abort(c(
      x = "The provided {.var session} is not valid SurveyCTO session.",
      i = "Please connect first using {.run ctoclient::cto_connect()}."
    ))
  }
  assign(".session", session, envir = .ctoclient_env)
}

#' @export
#' @rdname cto_connect
cto_is_connected <- function() exists(".session", envir = .ctoclient_env)
