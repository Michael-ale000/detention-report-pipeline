# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based pipeline that processes ICE detention facility statistics (from bi-weekly Excel releases) into interval ADP charts and parameterized HTML reports published at [detentionreports.com](https://detentionreports.com/). The methodology calculates "Interval ADP" (a backward-moving average) to correct for distortions in ICE's cumulative-bed-based reported ADP.

## Five-Part Workflow (from README)

1. **Interval ADP charts** — run `interval_adp.r`, then `interval_adp_chartmaker.r`
2. **Individualized subpopulation ADP** — run `Individual_Internal_ADP.r`
3. **Render HTML reports** — run `facilities_reporting.r` (Quarto rendering loop using `ice_detention_reporting.qmd`)
4. **Update metadata** (`index.json`)
5. **Push to GitHub** (HTML files must be pushed in groups of ≤13 via GitHub Desktop)

## Running Scripts

This is an RStudio project (`ice_detention_reports.Rproj`). Open that file to set the working directory correctly. All scripts use relative paths from the project root.

Run scripts via RStudio console or `Rscript`:
```r
source("interval_adp.r")
source("interval_adp_chartmaker.r")
source("Individual_Internal_ADP.r")
source("facilities_reporting.r")
```

To render a single facility report for testing:
```r
quarto::quarto_render(
  "ice_detention_reporting.qmd",
  execute_params = list(Name = "FACILITY NAME IN ALL CAPS", Latitude = 31.79, Longitude = -106.37)
)
```

## Key File Roles

| File | Role |
|---|---|
| `interval_adp.r` | Core data pipeline — reads all `.xlsx` from `./DetentionFacilities/`, computes `output_clean`, `all_clean`, and `final` (interval ADP data frame) |
| `detloc_processing_2025-12-08.r` | **Sourced by** `interval_adp.r`; maps facility Name/City/Address to DETLOC codes; depends on `output_clean` already being in the environment |
| `interval_adp_chartmaker.r` | Sources `interval_adp.r` + the three `one_facility_*.r` helpers; generates PNGs for all facilities and handles special-case facilities manually |
| `one_facility_plot.r` | Function `one_facility_plot()` — renders a single facility's interval ADP bar+line chart; saves to `plots2026_02_05/` |
| `one_facility_scatter_plot.r` | Function `one_facility_scatter_plot()` — ALOS vs. Interval ADP scatter; saves to `plots7/` |
| `one_facility_table.r` | Helper that prepares per-facility data frames for chart input |
| `Individual_Internal_ADP.r` | Same loading logic as `interval_adp.r` but produces `IntervalAdpForIndividualColumn_*.xlsx` breaking ADP into gender/criminality subcolumns |
| `facilities_reporting.r` | Quarto rendering loop using `pwalk(reports, quarto_render)`; reads from most recent xlsx in `./DetentionFacilities/` |
| `ice_detention_reporting.qmd` | Parameterized Quarto report template (params: `Name`, `Latitude`, `Longitude`); reads `IntervalAdpForIndividualColumn_*.xlsx` and the most recent `FY26_detentionStats*.xlsx` |

## Touchpoints to Update Each Data Release

When a new xlsx is released, update these hardcoded values across scripts:

- **`interval_adp_chartmaker.r`**: `touchpoint_date <- "YYYY-MM-DD"` and `as_of_date <- "Month DD, YYYY"`
- **`one_facility_plot.r`**: x-axis `limits` upper bound, subtitle date string, and output folder name (`plots2026_02_05/`)
- **`one_facility_scatter_plot.r`**: similar date references
- **`facilities_reporting.r`**: `path` to the new xlsx file
- **`ice_detention_reporting.qmd`**: path to `IntervalAdpForIndividualColumn_*.xlsx`, path to `fy26_detention_output_*.csv`, path to `FY26_detentionStats*.xlsx`, and the "Data as of" date in the header
- **Output folder**: create a new `plots<YYYY_MM_DD>/` folder and update references

## Data Architecture

- **`./DetentionFacilities/`** — all historical bi-weekly xlsx files (FY23–FY26); `interval_adp.r` reads all of them at once
- **`./src/fy26_detention_output_2025-11-22.csv`** — lat/long lookup table for mapping; new facilities need manual lat/long added here
- **`detloc_2025-04-13.csv`** — master DETLOC code reference file
- **`contracts.csv`** — contract PDF metadata for the report contract section
- **`IntervalAdpForIndividualColumn_*.xlsx`** — output of `Individual_Internal_ADP.r`; used as input by the Quarto report

## Known Data Issues and Special Cases

- **`Pull Date == "2025-07-07"`** is explicitly excluded in `interval_adp.r` (bad data).
- Several facilities have known blank/irregular values for specific pull dates and are filtered out with explicit `filter(!(...))` lines in `interval_adp.r`.
- **Clinton County Jail** exists in both NY (Plattsburgh) and IN; requires the `filter(State == "IN/NY")` comment blocks to be uncommented manually when rendering that facility.
- **DETLOC code overrides**: both `detloc_processing_2025-12-08.r` and `interval_adp.r` contain `case_when` blocks for facilities where the automated join produces wrong or missing codes. Add new overrides there.
- The 2025-07-21 pull date for `Dilley` requires manual handling (`one_facility_plot_for_dilley()`).

## R Package Dependencies

Core packages: `tidyverse`, `openxlsx`, `readxl`, `janitor`, `stringr`, `rlang`, `rjson`, `here`, `ggplot2`, `scales`, `patchwork`, `extrafont`, `cowplot`, `magick`, `quarto`, `leaflet`, `DT`, `knitr`, `kableExtra`, `purrr`, `lubridate`
