# Global Heritage Explorer

**Explore the geographic distribution of UNESCO World Heritage Sites alongside selected non-UNESCO cultural and heritage points of interest.**

An interactive data-exploration application built with **R Shiny**, **Leaflet**, **ggplot2**, and **sf**. It combines a UNESCO World Heritage dataset with selected regional points-of-interest datasets to make geographic patterns easier to inspect.

**[Open the Shiny app](https://statylerdaley.shinyapps.io/UNESCO-Shiny-App/)** · [View the source code](https://github.com/daley-tyler/global-heritage-explorer)

> The deployed app loaded on 30 October 2026. The Shiny deployment keeps its original URL independently of the GitHub repository name. It still shows the earlier interpretation text; the corrected source in this repository needs a separate deployment. This availability check does not validate every live interaction.

## Features

- **Interactive map:** Explore site locations using selectable geographic regions and separate UNESCO/non-UNESCO layers.
- **Category filters:** Filter UNESCO locations by cultural, natural, or mixed classification; filter other points of interest by museum, archaeological site, monument, memorial, ruins, or attraction.
- **Regional comparisons:** Compare the counts of included sites by region using side-by-side bar charts.
- **Country rankings:** View the top 15 countries by UNESCO site count within selected regions.
- **UNESCO distributions:** Explore inscription-year and site-area distributions with histograms.

## Technology

| Component | Tools |
| --- | --- |
| User interface | R, Shiny, shinythemes |
| Mapping | Leaflet |
| Data preparation | tidyverse, sf |
| Charts | ggplot2 (via tidyverse) |
| Deployment | shinyapps.io configuration included in the repository |

## Run locally

1. Install a supported version of [R](https://cran.r-project.org/).
2. Open this repository's root directory in R or RStudio.
3. Install dependencies:

   ```r
   install.packages(c("shiny", "shinythemes", "leaflet", "tidyverse", "sf"))
   ```

4. Launch the application from the repository root:

   ```r
   shiny::runApp(".")
   ```

The repository includes `unesco_heritage_data.RData`, which `app.R` loads at startup. If you remove or regenerate that file, the app cannot start until it has been recreated.

**System note:** Installing `sf` may require platform-specific geospatial libraries or binary packages.

## Validation

From the repository root, check that both R scripts parse:

```sh
Rscript --vanilla -e 'parse(file = "app.R"); parse(file = "data_prep.R")'
```

Then launch the app and exercise the map layers, region and category filters,
region/country charts, and both distribution choices. Parsing alone does not
validate reactive behavior or browser rendering. Map tiles require internet access.

There is currently no committed automated test suite or dependency lockfile.
The package installation command above installs the available package versions;
it does not recreate a pinned environment.

## Project structure

```text
.
├── app.R                       # Shiny user interface and server logic
├── data_prep.R                 # Source shapefile processing and normalization
├── unesco_heritage_data.RData   # Preprocessed datasets used by the app
└── rsconnect/                  # Deployment metadata
```

## Data and methodology

The bundled snapshot contains **1,248 UNESCO records** and **4,310 comparison
points** (5,558 combined records). UNESCO inscription years range from 1978 to
2025; the original download date is not recorded. One UNESCO record has missing
coordinates and is omitted from the map, while the charts still count it.
These are record counts, not independently deduplicated counts of unique sites.

- **UNESCO sites:** A UNESCO World Heritage source shapefile (`whc001.shp`) supplies location, category, inscription year, country, region, and reported site area.
- **Other points of interest:** Selected MapCruzin regional shapefiles supply places tagged as museums, archaeological sites, monuments, memorials, ruins, or attractions.
- **Processing:** `data_prep.R` normalizes fields, groups regions, extracts map coordinates, and saves the combined R dataset loaded by the app.

The preprocessing script expects the original source shapefiles in these directories, which are **not included** in the repository:

```text
whc001/whc001.shp
africa-points-shape/points.shp
asia-points-shape/points.shp
aus-oceania-points-shape/points.shp
so-america-points-shape/points.shp
```

If regenerating data, acquire each complete shapefile set (including its companion files), inspect its schema, coordinate reference system, and usage terms, back up the bundled `.RData` file, and then run `source("data_prep.R")` from the project root. This overwrites the bundled snapshot. Exact source versions and download dates are not recorded, so reproducing it byte for byte is not currently possible. The script extracts coordinates without explicitly reprojecting them; verify longitude/latitude in a Leaflet-compatible coordinate system before using replacement sources.

**Original data sources**

- [UNESCO World Heritage data](https://data.unesco.org/explore/dataset/whc001/export/?sort=date_inscribed)
- [MapCruzin shapefile archive](https://mapcruzin.com/download-shapefile/)

## Interpretation and limitations

This is an **exploratory visualization**, not a complete global census of all heritage places or a causal analysis of UNESCO designation decisions.

- The non-UNESCO comparison points are drawn from selected locations and point-of-interest tags, and coverage differs by region. No comparison points are included for Europe/North America; an absent comparison is not evidence that no heritage places exist there.
- The UI label "South America" also groups UNESCO records from "Latin America and the Caribbean". Oceania is assigned using a fixed country-name pattern. These are the script's regional groupings, not standardized geographic boundaries.
- These points have not been independently checked against the UNESCO list, so some labels may overlap or be incomplete.
- Country and inscription-year analyses focus on UNESCO records because corresponding fields are absent from the non-UNESCO data.
- The map filters and chart filters are independent. The area histogram includes only positive, non-missing values and uses a log10 horizontal scale.
- The included `.RData` file is a snapshot. It is not automatically refreshed from the underlying sources.
- Differences in counts may reflect underlying data coverage and selection rules, not necessarily designation bias.

## Future improvements

- Add a documented data-refresh and validation workflow.
- Add automated checks for missing coordinates, duplicate locations, and classification consistency.
- Add optimized map rendering for large point collections; the interface currently warns that enabling the Asia comparison layer may be slow.
- Add screenshots and a short walkthrough of the interface.

## Usage and attribution

Refer to each upstream data provider for its applicable data licenses and attribution requirements. A separate source-code license has not yet been selected for this repository.
