# Science Funding Dynamics: Grant Funding Decision Patterns (FY 2018 - 2025) 
Interactive R Shiny dashboard to visualize grant funding patterns over the last 8 years.

## Accessing the Raw Data (Not Included in Repository)

The raw PrimeAwardSummaries and TranscationSummaries CSV files (FY2018–FY2025) are too large to include in this repository (>300MB each).

You can download them directly from OneDrive using the following hyperlink:
[Science_Funding_Dynamics Raw Data](https://gtvault-my.sharepoint.com/:f:/g/personal/kkeith9_gatech_edu/EjOozESntY5Eh2FB5hlDtBEBbnkm01hC9seD6VPezGCTqg?e=EMDJt0)

Place all downloaded CSV files for each year (2018 - 2025) in the folder:

`data/raw/`

The script in `scripts/data_cleaning.Rmd` will automatically detect, load, and merge the files.
