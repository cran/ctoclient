# ctoclient 0.2.0

# ctoclient 0.1.0.9000

**cto_form_data**
* Returns `datetime` questions as date-times. They were also matched by the date pattern and converted to `Date`, dropping the time of day
* Keeps the original geopoint column under its own name. Splitting renamed it to `<name>_<name>`
* Splits geopoints even when one point has an unexpected number of parts. A single malformed point previously skipped the split for the whole form
* Reads the choice list of a `select_multiple ... or_other` question from the list name instead of `or_other`, which produced an empty `<question>_NA` column
* No longer overwrites a question named `any_selected`
* Recognises a form that has no submissions

**cto_form_dofile**
* Converts date and date-time fields to Stata numeric formats at the top of the do-file, using the fields found in the form definition
* Adds `destring` before every value label, and before the label of each integer and decimal question
* Uses `note x: text`. `note variable x` is not a Stata command, so notes were never applied to plain variables, and the quotes around the text were stored as part of the note
* Takes value label names from the choice lists themselves. They could previously be attached to the wrong list
* Escapes `$` and backticks in labels, which Stata expanded as macro references
* Prefixes `label define` with `cap`, so one list name that Stata rejects no longer discards every label after it
* Skips choice values Stata cannot use as value labels
* Reads the default language literally, so `English (en)` matches its own label column
* Writes the do-file as UTF-8
* Respects `options(ctoclient.verbose = FALSE)`

**cto_connect**
* Rejects a server name containing characters that change the shape of a URL, such as `/`, `?`, `#` or `@`
* Warns when `server` is given as a full host name
* Reports the original HTTP error when the error body has an unexpected shape

**Server datasets**
* `cto_dataset_upload()` validates `id`, and all dataset functions reject an `id` that is not URL safe
* `cto_dataset_download()` and `cto_form_attachment()` no longer let a server-supplied name write outside the requested directory

**Other**
* Requires httr2 (>= 1.2.0). `req_get_url()` and the current form of `req_throttle()` are not available before then, so `cto_connect()` failed outright on older versions
* Paginated endpoints stop if the server stops advancing the cursor
* The test suite runs offline from recorded fixtures rather than only when server credentials are set


# ctoclient 0.1.0

**cto_form_dofile**
* Skips dynamic value labels
* Other minor improvements


# ctoclient 0.0.2

**cto_form_data**
* Ensures all non-selected multi-select options are included when any related binary variables are present
* Replaces missing values with 0 for unselected multi-select binary variables
* Standardizes GPS field names using concise suffixes: lat, long, acc, and alt
* Provides more informative and user-friendly error messages during the tidying process
* Orders select_multiple binary variables according to their defined values
* Other minor improvement


# ctoclient 0.0.1

Initial CRAN Submission

# ctoclient 0.0.0.9000

* Development version.

