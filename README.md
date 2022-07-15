# Real-time estimation of Omicron and Delta transmission dynamics in England

## Analyses

### Real-time estimation of the time-varying transmission advantage of Omicron in England using S-Gene Target Status as a Proxy

[![Zenodo](https://zenodo.org/badge/435670197.svg)](https://zenodo.org/badge/latestdoi/435670197)

In this work, we use S-gene target failure (SGTF) as a proxy of variant status combined with reported case counts to explore the evidence for changes in transmission advantage over time for the Omicron variant. If present this could indicate the impact of immune escape, bias in SGTF data or differences in the populations within which the variants are circulating. We also report estimates for growth rates by variant and overall, case counts overall and by variant for a 14 day forecast window assuming constant future growth, the date at which Omicron will become dominant in England and in each NHS region, and the estimated cumulative percentage of the population with a reported Omicron case.

Estimates are updated daily as new data is released but the summary of the situation is updated less frequently and so may not match current estimates. All data (both raw data and estimates) are available [here](https://github.com/epiforecasts/omicron-sgtf-forecast/tree/main/data) and the report should be fully reproducible. Reports and data as available at the time of release are available from the [release page](https://github.com/epiforecasts/omicron-sgtf-forecast/releases). See our [news file](https://github.com/epiforecasts/omicron-sgtf-forecast/blob/main/NEWS.md) for details of what updates were made when.

**Real-time report: [as html](https://epiforecasts.io/omicron-sgtf-forecast/summary), [or pdf](https://epiforecasts.io/omicron-sgtf-forecast/summary.pdf)**

### Estimation of the test to test distribution as a proxy for generation interval distribution for the Omicron variant in England

### Background

Early estimates from South Africa indicated that the Omicron COVID-19 variant may be both more transmissible and have greater immune escape than the previously dominant Delta variant. The rapid turnover of the latest epidemic wave in South Africa as well as initial evidence from contact tracing and household infection studies has prompted speculation that the generation time of the Omicron variant may be shorter in comparable settings than the generation time of the Delta variant.

### Methods

We estimated daily growth rates for the Omicron and Delta variants in each UKHSA region from the 23rd of November to the 23rd of December 2021 using surveillance case counts by date of specimen and S-gene target failure status with an autoregressive model that allowed for time-varying differences in the transmission advantage of the Delta variant where the evidence supported this. By assuming a gamma distributed generation distribution we then estimated the generation time distribution and transmission advantage of the Omicron variant that would be required to explain this time varying advantage. We repeated this estimation process using two different prior estimates for the generation time of the Delta variant first based on household transmission and then based on its intrinsic generation time. 

### Results

Visualising our growth rate estimates provided initial evidence for a difference in generation time distributions. Assuming a generation time distribution for Delta with a mean of 2.5-4 days (90% credible interval) and  a standard deviation of 1.9-3 days we estimated a shorter generation time distribution for Omicron with a mean of 1.5-3.2 days and a standard deviation of 1.3-4.6 days. This implied a transmission advantage for Omicron in this setting of 160%-210% compared to Delta. We found similar relative results using an estimate of the intrinsic generation time for Delta though all estimates increased in magnitude due to the longer assumed generation time.

### Conclusions

We found that a reduction in the generation time of Omicron compared to Delta was able to explain the observed variation over time in the transmission advantage of the Omicron variant. However, this analysis cannot rule out the role of other factors such as differences in the populations the variants were mixing in, differences in immune escape between variants or bias due to using the test to test distribution as a proxy for the generation time distribution.

**Paper: [as html](https://epiforecasts.io/omicron-sgtf-forecast/generation-time), [or pdf](https://epiforecasts.io/omicron-sgtf-forecast/generation-time.pdf)**

## Key files and folders

Folder/File | Purpose
---|---
[`writeup`](writeup/) | Summaries and papers as `Rmarkdown` documents.
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