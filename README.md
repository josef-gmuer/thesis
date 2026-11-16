# Thesis Code

**Removal of EU Trade Protection on Chinese Solar Panels: Firm Exit, Market Structure and Deployment**
Josef Gmür, MA Thesis, University of St. Gallen

`analysis.R` contains all code used in the thesis. It runs top to bottom in four labelled sections: setup, data downloads, construction of the analysis panels, and the results in thesis order. Each table is printed and saved to `output/` under its table number.

## Requirements

R (≥ 4.2) and the packages `tidyverse`, `countrycode`, `fixest`, `httr2`, `jsonlite`, `eurostat`, `pxweb` and `readxl`.

## Data

The data is not included. The script downloads the public sources into `raw/` on first run, and skips any file already in `raw/`:

| Source | Data | API key |
|---|---|---|
| UN Comtrade | Chinese exports, HS 854140, 854150, 854142, 854143; UK total trade in the same codes | **Required** |
| Eurostat | Prodcom (DS-059358), Comext (DS-045409), GDP per capita, electricity prices, renewable share, population, exchange rates | Not needed |
| IRENASTAT | Installed solar PV capacity | Not needed |

### Comtrade API key

The only key needed is a free UN Comtrade subscription key:

1. Register at <https://comtradedeveloper.un.org>.
2. Subscribe to the free **comtrade - v1** product.
3. Copy the primary key from your profile.

Before running the script, enter the key in the R console:

```r
Sys.setenv(COMTRADE_API_KEY = "your-key-here")
```

Two inputs cannot be downloaded:

- `uk_manual_supplement.csv`: UK GDP per capita and renewable share, compiled from World Bank and UK government statistics (Section 3.3)
- `orbis.xlsx`: ORBIS export (Bureau van Dijk), licensed and not redistributable. Firms in the 15 sample countries with any NACE code in Table A.3, matched with the keyword search described in Section 3.2.

Eurostat and Comtrade revise their data, so a new download may differ slightly from the figures in the thesis.

## Running

Set the working directory to the folder containing `analysis.R` and run `source("analysis.R")`. Set `B_BOOT` and `N_PERM` lower at the top of the script for a quick run.
