# Science Funding Dynamics: Grant Funding Decision Patterns (FY 2018 - 2025)
Interactive R Shiny dashboard to visualize grant funding patterns over the last 8 years.

## Accessing the Data (Not Included in Repository)

The raw PrimeAwardSummaries and cleaned grants_analysis CSV files (FY2018–FY2025) are too large to include in this repository (>300MB each).

You can download them directly from OneDrive using the following hyperlink:
[science_funding_dynamics_data](https://gtvault-my.sharepoint.com/:f:/g/personal/kkeith9_gatech_edu/EjOozESntY5Eh2FB5hlDtBEBbnkm01hC9seD6VPezGCTqg?e=EMDJt0)

In the `science_funding_dynamics_data` OneDrive folder, there should be two folders that match the naming in the `data` folder of the GitHub: `raw` and `processed`. Please place the OneDrive data into its corresponding GitHub folder once cloning the repository. 

The script in `scripts/cleaning.Rmd` will automatically detect, load, and merge the eight PrimeAwardSummaries files, and `scripts/analysis.Rmd` will automatically detect and load `grants_analysis.csv`.
