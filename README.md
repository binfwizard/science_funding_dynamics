# Science Funding Dynamics: Grant Funding Decision Patterns (FY 2018 - 2025) 
Interactive R Shiny dashboard to visualize grant funding patterns over the last 8 years.

## Accessing the Raw Data (Not Included in Repository)

The raw PrimeAwardSummaries CSV files (FY2018–FY2025) are too large to include in this repository (>300MB each).

You can download them directly from USAspending.gov using the following link:
[[list links here](https://www.usaspending.gov/search)]

Place all downloaded CSV files for each year (2018 - 2025) in the folder:

`data/raw/`

The scripts in `scripts/data_cleaning.Rmd` will automatically detect and load the files.
