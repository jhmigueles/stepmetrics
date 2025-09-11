# stepmetrics 1.0.1
(CRAN release date: 2025-?-?)

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
