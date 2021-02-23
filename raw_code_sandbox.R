# Code sandbox
# Testing and refining the analysis in preparation for creating a Markdown document
# Script initiated on 2021-02-23, BL 


#### Description ####
# —————————————————————————————————— 


#### Security ####
# —————————————————————————————————— 

# Before running the notebook, 
# * the user must load a `json` file containing the BigQuery API key into the local directory `/content/...`
# * the user must load a `json` file containing the Google Maps API key into the local directory `/content/...`
# 
# Keeping these keys out of this notebook is important for security.

#### Tools ####
# ——————————————————————————————————

# Package and library installation
# ————————————————————————————————————————
# Divide package and library installs into separate chunks to speed loading 
# when adding new resources "on the fly". This saves time when slow-loading 
# resources aren't needed for a particular task in a notebook or Markdown document. 

# Quick-loading resources
packages_needed = c("tidyverse", "knitr", "colorspace", "rjson", "vegan") 
packages_installed = packages_needed %in% rownames(installed.packages())

if (any(! packages_installed))
  install.packages(packages_needed[! packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}

# Big R Query (slow loading)
packages_needed = c("bigrquery") # comma delimited vector of package names
packages_installed = packages_needed %in% rownames(installed.packages())

if (any(! packages_installed))
  install.packages(packages_needed[! packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}

# GG map, current version GitHub (slow loading)
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)
# mapping
library("ggmap")

# API keys
# ————————————————————————————————————————
# API keys are pulled from local resources and are not available in the hosted environment.
# Users must have API keys for Google Big Query and Google Maps

# Google Maps API (local file)
mapKey <- fromJSON(file = "/Users/blarkin/Egnyte/Private/blarkin/ΩMiscellaneous/R_global/R_globalKeys.json")$mapKey
register_google(key = mapKey)

# Big Query API Key (local file)
bq_auth(path = "/content/mpg-data-warehouse-api_key-master.json")
Sys.setenv(BIGQUERY_TEST_PROJECT = "mpg-data-warehouse")
billing <- bq_test_project()
