# Real-time estimation of the time-varying transmission advantage of Omicron in England using S-Gene Target Status as a Proxy

[![Zenodo](https://zenodo.org/badge/435670197.svg)](https://zenodo.org/badge/latestdoi/435670197)

In this work, we use S-gene target failure (SGTF) as a proxy of variant status combined with reported case counts to explore the evidence for changes in transmission advantage over time for the Omicron variant. If present this could indicate the impact of immune escape, bias in SGTF data or differences in the populations within which the variants are circulating. We also report estimates for growth rates by variant and overall, case counts overall and by variant for a 14 day forecast window assuming constant future growth, the date at which Omicron will become dominant in England and in each NHS region, and the estimated cumulative percentage of the population with a reported Omicron case.

Estimates are updated daily as new data is released but the summary of the situation is updated less frequently and so may not match current estimates. All data (both raw data and estimates) are available [here](https://github.com/epiforecasts/omicron-sgtf-forecast/tree/main/data) and the report should be fully reproducible. Reports and data as available at the time of release are available from the [release page](https://github.com/epiforecasts/omicron-sgtf-forecast/releases).

**Real-time report: [as html](https://epiforecasts.io/omicron-sgtf-forecast/summary), [or pdf](https://epiforecasts.io/omicron-sgtf-forecast/summary.pdf)**

## Key files and folders

Folder/File | Purpose
---|---
[`writeup`](writeup/) | Summary and paper as `Rmarkdown` documents.
[`R`](R/) | R functions used in the analysis and for evaluation.
[`data`](data/) | Input data and summarised output generated by steps in the analysis.
[`scripts`](scripts/) | R scripts used to run parts of the analysis.
[`.devcontainer`](.devcontainer/) | Resources for reproducibility using `vscode` and `docker`.

## Dependencies

All dependencies can be installed using the following, 

```r
remotes::install_dev_deps()
```

If `cmdstanr` has not been installed previously then stan may need to be installed. This can be done using the following,

```r
cmdstanr::install_cmdstan()
```

## Access observations and estimates

```r
source(here::here("R", "load-local-data.R"))
get_available_dates()
get_latest_date()
get_local_data(date = <date>)
load_results(date = <date>)
```

## Update estimates

To update the data and estimates run the following. Note this will update estimates regardless of if the data has been updated. 

```bash
bash bin/update-regional-estimates.sh
```