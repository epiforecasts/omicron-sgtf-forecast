# Data

Processed input data and model outputs stored in dated sub-folders.

Folder | Purpose
---|---
[`estimates/sgtf`](estimates/sgtf/) | Posterior estimates, fit diagnostics, and model comparison metrics for models estimating the transmission advantage of Omicron vs non-Omicron using SGTF status in dated sub-folders. These estimates can be most easily accessed using the tooling provided in `R/load-local-data.R`.
[`estimates/bias`](estimates/bias/) | Posterior estimates, and fit diagnostic for models estimating the potential sampling bias for S-gene status in dated sub-folders. These estimates can be most easily accessed using the tooling provided in `R/load-local-data.R/`.
[public`](public/) | SGTF and reported cases data by specimen date downloaded from public sources saved in dated files using `scripts/update-regional-data.R`.
