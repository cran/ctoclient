
# The form ID list is cached per server. These tests count requests, since the
# point of the cache is to make fewer of them without ever missing a form.

fake_session <- function(server = "server-a") {
  structure(list(server = server), class = c("httr2_request", "cto_session"))
}

clear_cache <- function() {
  assign(".form_ids", NULL, envir = .ctoclient_env)
}

# Returns a counter and installs a mocked fetch_api_response in the caller's
# frame. `ids` may be a function of the call number.
mock_ids <- function(ids, env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(
    fetch_api_response = function(req, url_path = NULL, file_path = NULL) {
      calls$n <- calls$n + 1L
      if (is.function(ids)) ids(calls$n) else ids
    },
    .env = env
  )
  calls
}

test_that(
  "the list is fetched once and then reused",
  {
    clear_cache()
    calls <- mock_ids(c("form_a", "form_b"))
    s <- fake_session()

    expect_true(assert_form_id("form_a", session = s))
    expect_equal(calls$n, 1L)
    expect_true(assert_form_id("form_b", session = s))
    expect_true(assert_form_id("form_a", session = s))
    expect_equal(calls$n, 1L)
  }
)

test_that(
  "a form deployed after the list was cached is still found",
  {
    clear_cache()
    calls <- mock_ids(function(n) if (n == 1L) "form_a" else c("form_a", "form_new"))
    s <- fake_session()

    expect_true(assert_form_id("form_a", session = s))
    expect_equal(calls$n, 1L)
    # miss against the cache triggers exactly one refresh, which finds it
    expect_true(assert_form_id("form_new", session = s))
    expect_equal(calls$n, 2L)
  }
)

test_that(
  "an unknown form still aborts, and costs no more requests than before",
  {
    clear_cache()
    calls <- mock_ids(c("form_a"))
    s <- fake_session()

    # nothing cached yet: one request, and no pointless refresh after it
    expect_error(assert_form_id("nope", session = s), "There is no form")
    expect_equal(calls$n, 1L)

    # that same request populated the cache, so a valid id is now free
    expect_true(assert_form_id("form_a", session = s))
    expect_equal(calls$n, 1L)

    # a miss against a warm cache costs exactly one refresh, then aborts
    expect_error(assert_form_id("nope", session = s), "There is no form")
    expect_equal(calls$n, 2L)
  }
)

test_that(
  "one server's list is never used for another",
  {
    clear_cache()
    calls <- mock_ids(function(n) if (n == 1L) "only_on_a" else "only_on_b")

    expect_true(assert_form_id("only_on_a", session = fake_session("server-a")))
    expect_equal(calls$n, 1L)

    # server-b must not inherit server-a's list
    expect_error(
      assert_form_id("only_on_a", session = fake_session("server-b")),
      "There is no form"
    )
    expect_true(assert_form_id("only_on_b", session = fake_session("server-b")))
  }
)

test_that(
  "a session without a usable server name is never cached",
  {
    for (bad in list(NULL, NA_character_, "", c("a", "b"))) {
      clear_cache()
      calls <- mock_ids(c("form_a"))
      s <- structure(list(server = bad), class = c("httr2_request", "cto_session"))

      expect_true(assert_form_id("form_a", session = s))
      expect_true(assert_form_id("form_a", session = s))
      # no key, so every lookup goes to the server as it did before
      expect_equal(calls$n, 2L)
    }
  }
)

test_that(
  "an empty or malformed response is not cached",
  {
    clear_cache()
    calls <- mock_ids(function(n) if (n <= 2L) character(0) else "form_a")
    s <- fake_session()

    expect_error(assert_form_id("form_a", session = s), "There is no form")
    expect_equal(calls$n, 1L)
    # nothing was cached, so the next call asks again and then succeeds
    expect_error(assert_form_id("form_a", session = s), "There is no form")
    expect_equal(calls$n, 2L)
    expect_true(assert_form_id("form_a", session = s))
    expect_equal(calls$n, 3L)
  }
)
