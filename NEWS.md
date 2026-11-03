# stepmetrics (development version, to become 2.0.0)

Infrastructure to turn stepmetrics into a repository of step-detection
algorithms (branch `issue33_step_detection`). No algorithm is included yet;
each one will come in its own branch.

- `read_raw()` and `raw_header()`: read a whole raw file of the formats GGIR
  reads into one common format, with the gyroscope when recorded and a
  timestamp per sample: Axivity .cwa and .csv, GENEActiv .bin and Parmay
  Matrix .bin (through 'GGIRread'), ActiGraph .gt3x (through 'read.gt3x') and
  ActiLife .csv, and Movisens .bin (through 'unisensR'). Files are always read
  whole, which suits laboratory protocols of a few hours; recordings of
  several days are processed in GGIR with `myfun_steps()`.
- `as_raw()`: wrap a data.frame of raw samples in the same format.
- `register_step_method()`, `step_methods()`: registry of step-detection
  methods with their metadata (channels, sampling rate, locations, licence,
  citation) and a common contract for their core function.
- `detect_steps()`: one row per step with its exact time, side and type.
- `myfun_steps()`: run an acceleration-only method inside GGIR as `myfun`.
- `steps_to_epochs()`: steps per epoch in the format of `readFile()`, to use
  `step.metrics()` with any algorithm.
- `benchmark_steps()`: one-to-one matching against a reference (e.g. video),
  with sensitivity, precision, F1 and count error, overall or per segment.
- 'GGIRread' and 'data.table' added to Imports; 'read.gt3x' and 'unisensR' in
  Suggests (needed only for those formats); 'GGIR' in Suggests (for tests).

# stepmetrics 1.1.0

- Features: Added support for idloc = NULL, i.e., the entire file basename is used as ID.
- Fix: Resolved file matching failure for GGIR output when IDs (now GGIR-identified ID is used)

# stepmetrics 1.0.3

- Tests: hardened timestamp checks to be robust across platforms (Windows, macOS ARM, Linux). 
  Added cross-platform ISO-8601 parser that correctly handles `Z` and `hh:mm` offsets.
- CI: added macOS ARM (Apple Silicon) checks via R-hub v2 to reproduce CRAN’s environment.

# stepmetrics 1.0.2

- Fix: preserve local clock time when parsing timestamps from CSV/AGD. 
  Previously, times could shift on systems using a different TZ (e.g., UTC). 
  Timestamps are now consistently emitted in ISO-8601 with the local offset.
- Tests: make `readFile()` tests timezone-stable.
- Docs: clarify timezone behavior in `?readFile`.

# stepmetrics 1.0.1
(CRAN release date: 2025-09-16)

- DESCRIPTION: quote brand/product names ('ActiGraph', 'Fitbit', 'GGIR') per CRAN policy.

# stepmetrics 1.0.0

- Added steps as well as minutes in cadence bands (`get_cadence_bands()`).
- Added MPA, VPA, and MVPA step counts (`*_steps` variables).
- Improved documentation across all core functions.
- Expand README with further details on the package and new badges.
- Updated DESCRIPTION (authors, funding, URLs, license).
- test-readFile now is skipped if package `RSQLite` is missing.
- First CRAN release of **stepmetrics**.


# stepmetrics 0.1.3

(Github-only release date: 2025-09-05)
-   Update IDs definition removing .RData to match current GGIR versions.
-   Improve peak cadence test.
-   Improve extraction of wear time when using GGIR-processed data.
-   Improve identification of day indices when there are time gaps. #17
-   Improved flexibility to handle Fitbit time stamps. #13

# stepmetrics 0.1.2

(Github-only release date: 2023-04-24)
-   Expanded functionality to work with ActiGraph csv files.
-   Expanded functionality to work with GGIR output.
-   Improved test coverage.
-   Improved verbose functionality.

# stepmetrics 0.1.1

(Github-only release date: 2023-03-15)
-   Improved test coverage.
-   Added github actions.

# stepmetrics 0.1.0 

(Github-only release date: 2023-03-12)
-   First working version of the package.
-   Added a `NEWS.md` file to track changes to the package.
