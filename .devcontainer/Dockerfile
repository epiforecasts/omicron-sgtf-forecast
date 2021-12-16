# R version: 4, 4.1, 4.0
FROM ghcr.io/epiforecasts/forecast.vocs:main

COPY DESCRIPTION /tmp/package/DESCRIPTION
RUN Rscript -e "devtools::install_deps('/tmp/package', dependencies = TRUE)"