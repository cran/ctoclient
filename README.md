
# ctoclient: A Modern and Flexible Data Pipeline for 'SurveyCTO'

<!-- badges: start -->
[![R-CMD-check](https://github.com/GutUrago/ctoclient/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GutUrago/ctoclient/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/1121002963.svg)](https://doi.org/10.5281/zenodo.18107568)
<!-- badges: end -->


A modern and flexible R client for the [SurveyCTO REST API](https://developer.surveycto.com/), 
a mobile and offline data collection platform, providing a modern and consistent interface for 
programmatic access to server resources. Built on top of the [httr2 package](https://httr2.r-lib.org/), 
it enables secure and efficient data retrieval and returns analysis-ready 
data through optional tidying. It includes functions to create, upload, and 
download server datasets, in addition to fetching form data, files, and 
submission attachments. Robust authentication and request handling make the 
package suitable for automated survey monitoring and downstream analysis.


This package is built with robustness and efficiency in mind, aiming to streamline 
the workflow for researchers and data analysts who rely on SurveyCTO. By automating 
the retrieval of data, attachments, and server metadata, `ctoclient` allows you to 
focus on analysis rather than manual data management. Whether you are running daily 
monitoring dashboards or final impact evaluations, this tool ensures your data 
pipeline is reproducible and reliable.

We welcome contributions from the community! If you encounter a bug, have a 
feature request, or want to improve the documentation, please feel free to open 
an issue or submit a pull request.

## Installation

You can install the stable version of ctoclient:

```r
install.packages("ctoclient")
```
or development version:

``` r
# install.packages("devtools")
devtools::install_github("GutUrago/ctoclient")

# or 

# install.packages("remotes")
remotes::install_github("GutUrago/ctoclient")
```

## Setup & Authentication

To avoid hard-coding credentials in your scripts, it is highly recommended to store your SurveyCTO server details in your .Renviron file.

1. Run usethis::edit_r_environ() to open your environment file.
2. Add your credentials:

``` r
SERVER="myorganization.surveycto.com"
USER="myemail@example.com"
PASS="mypassword"
```
3. Restart R.

## Usage

### 1. Connect to the Server

``` r
library(ctoclient)

# Connect using environment variables (recommended)
cto_connect(
  server    = Sys.getenv("SERVER"),
  user      = Sys.getenv("USER"),
  password  = Sys.getenv("PASS")
)

# Verify connection
if (cto_is_connected()) {
  message("Successfully connected to SurveyCTO!")
}
```

### 2. Working with Forms and Data

Download form definitions, raw data, and attachments. Export functions with cto_*

``` r
# List all available forms
forms <- cto_form_ids()

# Get metadata form for specific form
cto_form_metadata('myform')

# Download data for a specific form
data <- cto_form_data("myform")

# Download form submission medias
cto_form_data_attachment('myform', ends_with('_img'), "mykey")

# Download the default form import do-file
cto_form_stata_template('myform')

# Build custom Stata import do-file
cto_form_dofile('myform', "form.do")

# Download attachments (e.g., photos, audio)
cto_form_attachment("myform", dir = "data/attachments", overwrite = TRUE)

```

### 3. Server Datasets

Manage server-side datasets.

```r
# List existing datasets
datasets <- cto_dataset_list()

# Create server dataset
cto_dataset_create("mydata")

# Upload a local CSV to a server dataset
cto_dataset_upload("mydata", "data/mydata.csv")

# Download a server dataset to a local file
cto_dataset_download(dir = "data/downloads", overwrite = TRUE)

# Purge server dataset
cto_dataset_purge("mydata")

# Delete server dataset
cto_dataset_delete("mydata")
```


### 4. Utilities and Metadata

Retrieve server configuration and helper files.

```r
# Get server metadata
meta <- cto_metadata()

# Generate a Stata template for a form
cto_form_languages("myform")

# Get a printable version of the form
cto_form_printable("myform")

# Get a mail-merge template of the form
cto_form_mail_template
```


## Disclaimer
This package is an independent, open-source project. It is not affiliated with, 
endorsed by, or maintained by SurveyCTO or Dobility, Inc. Use it at your own risk, 
and always ensure you handle survey credentials and participant data securely.
