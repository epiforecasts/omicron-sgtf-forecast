#!/bin/bash

# update dependencies
Rscript -e "devtools::install_dev_deps()"

# update data
Rscript scripts/update-regional-data.R

# update estimates
Rscript scripts/update-regional-estimates.R