#' ---
#' title: "Vegetation Survey at MPG Ranch: Efficiency and Recommendations"
#' author: "Beau Larkin"
#' date: "2021-03-06"
#' output: 
#'   github_document:
#'     toc: true
#'     toc_depth: 2
#' ---
#' 
#' # Description
#' On February 27, 2020, Rebecca Durham, Craig Jourdonnais, Beau Larkin, Dean Pearson, Phil Ramsey, 
#' and Mike McTee began a discussion about whether our vegetation survey methods need revision. 
#' After that meeting, we identified several needs for additional investigation and discussion.
#' * [link](https://docs.google.com/document/d/1yCLPai5r4z5nxyimmbRF-I2Gugzn_jgBFV2wpemR0IM/edit?usp=sharing) to meeting notes from 2020-02-27
#' * [link](https://docs.google.com/document/d/11Ec4hUrRYN9f24CrEPM5Ui3ec9SzbcOIuwKdRoc0v7A/edit?usp=sharing) to report on proposed revisions to vegetation survey methods
#' 
#' This document is intended to curate the code, analysis, and results presented in the report on 
#' proposed revisions to vegetation methods.
#' 
#' # Resources
#' ## Package and library installation

## Quick-loading resources
packages_needed = c("tidyverse", "knitr", "rjson", "vegan", "plotrix")
packages_installed = packages_needed %in% rownames(installed.packages())
if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}

## Big R Query (slow loading)
packages_needed = c("bigrquery") # comma delimited vector of package names
packages_installed = packages_needed %in% rownames(installed.packages())
if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}

#' ## API keys
#' API keys for data access are pulled from local resources and are not available in the hosted environment. Code not shown here.

#+ BQ_api_keys,echo=FALSE
bq_auth(path = paste0(getwd(), "/mpg-data-warehouse-api_key-master.json"), cache = NULL)
Sys.setenv(BIGQUERY_TEST_PROJECT = "mpg-data-warehouse")
billing <- bq_test_project()

#' ## Global functions and styles
## Load text file of styles from Google Drive
source(fromJSON(file = paste0(getwd(), "/R_globalKeys.json"))$stylesKey)

## Calculating the 95% CI will aid plotting later
## Uses `plotrix`
ci_95 = function(x){std.error(x) * qnorm(0.975)}

#' # Source data
#' 
#' ## Point-intercept species data
#' Make raw data available locally by pulling from the MPG Data Warehouse
#' and then pre-process to create two objects that will be joined with metadata and used for analysis
spe_pull_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_point_intercept_gridVeg.gridVeg_point_intercept_vegetation`
  WHERE year = 2016
  "
spe_pull_bq <- bq_project_query(billing, spe_pull_sql)
spe_pull_tb <- bq_table_download(spe_pull_bq)
spe_pull_df <- as.data.frame(spe_pull_tb) %>% glimpse()
#' Note that in `spe_pull_df`, with 200 intercepts per grid point, the total number of records indicated here should not be possible. 
#' Under investigation, I found that 7 grid points contain only 199 records, so no correction to the data is possible. 
#' For this analysis, a small number of missing records should not affect the interpretation.
#'
#' ### Species data must be transformed to long-form to enable filtering
spe_df <-
  spe_pull_df %>%
  select(grid_point, transect_point, starts_with("intercept")) %>%
  pivot_longer(starts_with("intercept"),
               names_to = "intercept",
               values_to = "key_plant_species") %>%
  # replace_na(list(key_plant_species = 360)) %>%
  drop_na() %>% 
  glimpse()

#' ### Height data must be stripped from species data frame to use separately
ht_df <-
  spe_pull_df %>%
  select(grid_point, transect_point, height_intercept_1) %>%
  glimpse()

#' ## Point-intercept ground cover data
#' Wrangling will be handled later during analysis
grcov_pull_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_point_intercept_gridVeg.gridVeg_point_intercept_ground`
  WHERE year = 2016
  "
grcov_pull_bq <- bq_project_query(billing, grcov_pull_sql)
grcov_pull_tb <- bq_table_download(grcov_pull_bq)
grcov_pull_df <-
  as.data.frame(grcov_pull_tb) %>%
  select(grid_point, transect_point, intercept_ground_code) %>%
  glimpse()

#' ## Quadrat-based species data
#' The quadrat data comes from the survey known as "YVP". It is used here to demonstrate
#' how many species may be recovered by combining point-intercept and quadrat methods.
qspe_pull_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_fixed_plot_yvp.yvp_vegetation_cover`
  WHERE (plot_loc <> 'N' OR plot_loc IS NULL)
  AND date BETWEEN '2017-01-01' AND '2017-12-31'
  "
qspe_pull_bq <- bq_project_query(billing, qspe_pull_sql)
qspe_pull_tb <- bq_table_download(qspe_pull_bq)
qspe_pull_df <- as.data.frame(qspe_pull_tb) %>% glimpse()

#' ## Vegetation species metadata
#' This dataset contains plant functional groups and other metadata associated with plant species.
spe_meta_sql <-
  "
  SELECT key_plant_species, key_plant_code, plant_native_status, plant_life_cycle, plant_life_form, plant_name_sci, plant_name_common
  FROM `mpg-data-warehouse.vegetation_species_metadata.vegetation_species_metadata`
  "
spe_meta_bq <- bq_project_query(billing, spe_meta_sql)
spe_meta_tb <- bq_table_download(spe_meta_bq)
spe_meta_df <- as.data.frame(spe_meta_tb) %>% glimpse()

#' ## Grid point metadata
#' This dataset contains habitat types and spatial metadata for grid points. 
gp_meta_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.grid_point_summaries.location_position_classification`
  "
gp_meta_bq <- bq_project_query(billing, gp_meta_sql)
gp_meta_tb <- bq_table_download(gp_meta_bq)
gp_meta_df <- as.data.frame(gp_meta_tb) %>% glimpse()

#' ### Vector of grid points in grassland
#' This is an additional grid point metadata item. It is useful for quickly filtering 
#' grid points to grassland habitats without having to use `left_join()` to associate 
#' metadata with vegetation data. 
grass_pts <- gp_meta_df %>% 
  filter(type3_vegetation_indicators == "uncultivated grassland native or degraded") %>% 
  pull(grid_point)

