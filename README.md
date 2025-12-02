# Science Funding Dynamics: Grant Funding Decision Patterns (FY 2018 - 2025)
This project investigates how U.S. federal science grant activity evolved between fiscal years 2018-2025, with a focus on three key hypotheses: (1) yearly funding levels, (2) overall funding volume, and (3) cancellation rates. These years span the pre-COVID period, COVID-19 pandemic, and the recent 2024 presidential transition, providing a unique context for examining how federal research funding behavior responds to large-scale national events.

Using an interactive Shiny dashboard, the project visualizes grant distributions, cancellation patterns, agency-level differences, and financial trends over time. Statistical tests, including chi-square tests, Kruskal-Wallis tests, effect size estimation (Cramer's V), and Cochran-Armitage trend tests, are used to evaluate whether cancellation rates and funding volumes changed significantly across the study period. Together, these tools support an exploratory analysis of how stable (or unstable) federal grant allocations have been during periods of economic and administrative uncertainty.

## Dataset Description
The dataset consists of prime award summary records for all federal assistance grants issued between FY 2018–2025, downloaded directly from [USASpending.gov](https://www.usaspending.gov/search), the official open data source for U.S. federal award information. The data was filtered specifically for **NIH (HHS), NSF, DOE, NASA, USDA, EPA, and DoD agencies** and includes:
* **Grant metadata**: agency name, sub-agency, award identifiers
* **Financial information**: obligated funding, non-federal contributions, outlays
* **Timeline data**: award dates, performance period dates
* **Derived variables**: fiscal year, cancellation indicator, log-transformed funding
* **Administrative attributes** such as assistance type and recipient organization

All records include complete financial reporting through 2025, with cancellations defined as awards whose “total obligated amount” became negative (net de-obligation). The dataset excludes loans and non-research assistance programs to focus specifically on science-related federal expenditures.

## Accessing the Data (Not Included in Repository)

The raw PrimeAwardSummaries and cleaned grants_analysis CSV files (FY2018-FY2025) are too large to include in this repository (>300MB each).

You can download them directly from OneDrive using the following hyperlink:
[science_funding_dynamics_data](https://gtvault-my.sharepoint.com/:f:/g/personal/kkeith9_gatech_edu/EjOozESntY5Eh2FB5hlDtBEBbnkm01hC9seD6VPezGCTqg?e=EMDJt0)

In the `science_funding_dynamics_data` OneDrive folder, there should be two folders that match the naming in the `data` folder of the GitHub: `raw` and `processed`. Please place the OneDrive data into its corresponding GitHub folder once cloning the repository (see instructions below). 

The script in `scripts/cleaning.Rmd` will automatically detect, load, and merge the eight PrimeAwardSummaries files, and `scripts/analysis.Rmd` will automatically detect and load `grants_analysis.csv`.

## Installation and Running the Dashboard 

### Clone the repository 
```bash
git clone https://github.com/binfwizard/science_funding_dynamics.git
cd science_funding_dynamics
```
### Required R version
* R 4.3+ recommended
* RStudio optional but helpful

### Install required R packages 
Install all necessary packages:
```r
# data manipulation and cleaning
install.packages(c(
  "tidyverse",
  "lubridate",
  "janitor",
  "here",
  "readr"))

# statistical tests and effect sizes
install.packages(c(
  "car",         
  "rcompanion",  
  "lsr",         
  "DescTools"))

# visualization
install.packages(c(
  "ggplot2",
  "plotly",
  "scales"))

# shiny and dashboard components
install.packages(c(
  "shiny",
  "DT"))
))
```
### Load the data 
Ensure `grants_analysis.csv` is located in the `/data/processed` folder before running the dashboard. Make sure `app.R` correctly points to this directory using `read_csv()` or `here()`.

### Run the dashboard
From within the project directory, run:

```r
library(shiny)
runApp("dashboard")
```
Or open `app.R` in RStudio and click **Run App**.

The dashboard will open at:

```r
http://127.0.0.1:4188
```
RECOMMENDED: For the best viewing experience, maximize the application window so that the dashboard’s layout and visual elements render properly.

## Data Sources and Citations 

Primary source dataset: 
* USAspending.gov (2025). Prime Award Summaries: Fiscal Years 2018–2025. Retrieved from https://www.usaspending.gov.

