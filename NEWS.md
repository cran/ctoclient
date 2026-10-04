# ctoclient 0.2.3

* Removes platform-dependent assertions from the datetime tests, which failed on CRAN's macOS builder although the parser itself was correct


# ctoclient 0.2.2

**cto_form_dofile**
* Drops null fields by their exact name as well as their repeat-suffixed names
* Labels the bare variable alongside its repeat-suffixed copies
* No longer destrings text and audio audit fields, which hold file names
* Leaves a blank line between variables

**cto_form_data**
* Reads export timestamps independently of the system locale

**cto_form_data_attachment**
* Reports download progress


# ctoclient 0.2.1

**cto_form_docx**
* New function writing the deployed form to a Word document for review, with rows colour-coded by field type and a legend

**cto_form_dofile**
* Keeps choice values as the form writes them, so whole codes no longer gain a decimal

# ctoclient 0.2.0

**cto_form_data**

* Correctly handles date-times, geopoints, `select_multiple ... or_other`, `any_selected`, and forms with no submissions.
* Improves robustness when parsing malformed geopoints.

**cto_form_dofile**

* Improves date/date-time conversion, value labels, notes, escaping, and UTF-8 output.
* Respects `ctoclient.verbose` and handles invalid Stata labels more safely.

**cto_connect & datasets**

* Validates URL-safe server and dataset IDs.
* Improves HTTP error reporting and prevents server-supplied filenames from writing outside the requested directory.

**Other**

* Requires `httr2 (>= 1.2.0)`.
* Safely handles stalled pagination.
* Tests now run offline using recorded fixtures.


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

