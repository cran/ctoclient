## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(ctoclient)
# 
# cto_connect(
#   server   = Sys.getenv("CTO_SERVER"),
#   username = Sys.getenv("CTO_USER"),
#   password = Sys.getenv("CTO_PASS")
# )

## -----------------------------------------------------------------------------
# cto_connect(server = "myorg", username = "admin@example.com")

## -----------------------------------------------------------------------------
# # Once, interactively:
# keyring::key_set("ctoclient", username = "admin@example.com")
# 
# # In every script from then on:
# cto_connect(
#   server   = "myorg",
#   username = "admin@example.com",
#   password = keyring::key_get("ctoclient", username = "admin@example.com")
# )

## -----------------------------------------------------------------------------
# cto_connect("myorg", "admin@example.com")
# 
# # No connection argument anywhere
# cto_form_ids()
# cto_form_data("baseline_survey")
# cto_dataset_list()

## -----------------------------------------------------------------------------
# cto_is_connected()
# #> [1] TRUE

## -----------------------------------------------------------------------------
# conn_staging <- cto_connect("org-staging", "admin@example.com")
# conn_prod    <- cto_connect("org-prod", "admin@example.com")
# 
# # The most recent connect() is the active one, so this reads from prod
# data_prod <- cto_form_data("baseline_survey")
# 
# # Switch to staging and write the same data to a dataset there
# cto_set_connection(conn_staging)
# cto_dataset_upload("aggregated_data", file = "data/baseline.csv")

## -----------------------------------------------------------------------------
# tmp <- tempfile(fileext = ".csv")
# readr::write_csv(data_prod, tmp)
# cto_dataset_upload("aggregated_data", file = tmp)

## -----------------------------------------------------------------------------
# library(httr2)
# 
# custom <- conn_prod |>
#   req_user_agent("MyResearchBot/1.0") |>
#   req_retry(max_tries = 5) |>
#   req_timeout(120)
# 
# cto_set_connection(custom)
# cto_form_data("my_form")

## -----------------------------------------------------------------------------
# options(ctoclient.verbose = FALSE)

