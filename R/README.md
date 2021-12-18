
### Utils

[load-data.R](./load-data.R): load case data (saved in “data/private”),
and add row totals. Data are all test-positive cases in England by
specimen date.

[build-models.R](./build-models.R): wrapper around
`forecast.vocs::forecast()` to fit to multiple variant relationships and
save output.

[load-parameters.R](./load-parameters.R): list model parameters to use
in `build_models()`. Model parameters are:

    ## $voc_label
    ## [1] "Omicron"
    ## 
    ## $voc_scale
    ## [1] 0.21 0.20
    ## 
    ## $strains
    ## [1] 2
    ## 
    ## $r_init
    ## [1] 0.00 0.25
    ## 
    ## $scale_r
    ## [1] 1
    ## 
    ## $r_step
    ## [1] 7
    ## 
    ## $overdispersion
    ## [1] TRUE
    ## 
    ## $timespan
    ## [1] 1
    ## 
    ## $horizon
    ## [1] 1
