# Lab-5

## Data Download

```r
devtools::install_github("shaiq681/CTScraped")

library(CTScraped)

APIResponse <- download_clinical_trials(search_query = "covid19 AND (new york OR italy)")

```
