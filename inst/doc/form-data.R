## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(ctoclient)
# 
# cto_connect("myorg", "admin@example.com")
# 
# data <- cto_form_data("baseline_survey")

## -----------------------------------------------------------------------------
# data <- cto_form_data(
#   form_id     = "baseline_survey",
#   private_key = "keys/baseline.pem",
#   start_date  = as.POSIXct("2026-01-01"),
#   status      = c("approved", "pending"),
#   tidy        = TRUE
# )

## -----------------------------------------------------------------------------
# approved <- cto_form_data("baseline_survey", status = "approved")

## -----------------------------------------------------------------------------
# raw <- cto_form_data("baseline_survey", tidy = FALSE)

## -----------------------------------------------------------------------------
# names(data)[grepl("^gps", names(data))]
# #> [1] "gps"      "gps_lat"  "gps_long" "gps_alt"  "gps_acc"

## -----------------------------------------------------------------------------
# library(tidyr)
# 
# plots <- data |>
#   pivot_longer(
#     cols = matches("^plot_(size|id)_[0-9]+$"),
#     names_to = c(".value", "plot_number"),
#     names_pattern = "^(plot_(?:size|id))_([0-9]+)$"
#   ) |>
#   drop_na(plot_id)

