#' ---
#' title: "Effect of sampling date on cover estimate"
#' author: "Beau Larkin"
#' date: "2021-12-06"
#' output:
#'   github_document:
#'     toc: true
#'     toc_depth: 2
#' ---
#'
#' # Description
#' This is an addendum to the vegetation sampling guidance report produced in early 2021.
#' The purpose here is to produce some graphics and supporting summaries about how
#' vegetation cover changes over the course of a sesason, and how we can handle, reduce, 
#' or otherwise manage it with surveys at MPG Ranch.
#' 
#' # Resources
#' 
#' ## Packages, libraries, and functions
#' 
#' Packages and multiple data sources must be added to the local environment before knitting 
#' this notebook. 

#+ install_1,message=FALSE
# Quick-loading resources
packages_needed = c("tidyverse", "knitr", "rjson", "plotrix", "colorspace", "devtools")
packages_installed = packages_needed %in% rownames(installed.packages())
if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}

#+ install_2,include=FALSE
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)   

#+ install_3,message=FALSE
# Big R Query
# ggmap package installed from GitHub using `devtools` (not shown)
packages_needed = c("bigrquery", "ggmap") # comma delimited vector of package names
packages_installed = packages_needed %in% rownames(installed.packages())
if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}

#' ## API keys
#' API keys for data access are pulled from local resources and are not available in the hosted environment. Code not shown here.

#+ BQ_api_keys,echo=FALSE
bq_auth(
  path = paste0(getwd(), "/mpg-data-warehouse-api_key-master.json"),
  cache = NULL
)
Sys.setenv(BIGQUERY_TEST_PROJECT = "mpg-data-warehouse")
billing <- bq_test_project()

#+ Google_maps_key,echo=FALSE
mapKey <- fromJSON(file = paste0(getwd(), "/R_globalKeys.json"))$mapKey
register_google(key = mapKey)

#' ## Global functions and styles: `theme_bgl`
# Load text file from local working directory
source(paste0(getwd(), "/styles.txt"))

#' ## Data
#' Survey metadata
#+ survey_metadata, echo = FALSE
meta_sql <- 
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_point_intercept_gridVeg.gridVeg_survey_metadata`
  "
meta_bq <- bq_project_query(billing, meta_sql)
meta_tb <- bq_table_download(meta_bq)
meta_df <- as.data.frame(meta_tb)

#' Plant species and cover data from point-intercept surveys in 2011-12, 2016, and 2021.
#' Plant data are joined with survey metadata to filter the data to the survey periods with the 
#' greatest effort (2011-12, 2016, and 2021). Data are simplified and summarized to show sums of 
#' cover in plant functional groups at each date of annual surveys. 
#+ plant_cover_data, echo = FALSE
cvr_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_gridVeg_summaries.gridVeg_foliar_cover_all`
  WHERE type3_vegetation_indicators = 'uncultivated grassland native or degraded'
  "
cvr_bq <- bq_project_query(billing, cvr_sql)
cvr_tb <- bq_table_download(cvr_bq)
cvr_df <- 
  as.data.frame(cvr_tb) %>% 
  filter(survey_sequence %in% c("2011-12", "2016", "2021"), 
         plant_native_status %in% c("native", "nonnative"), 
         plant_life_cycle %in% c("annual", "perennial"), 
         plant_life_form %in% c("forb", "graminoid")) %>% 
  select(survey_ID, year, grid_point, plant_name_common, plant_native_status, plant_life_cycle, plant_life_form, intercepts_pct) %>% 
  left_join(meta_df %>% select(survey_ID, date), by = "survey_ID") %>% 
  group_by(grid_point, year, date, plant_native_status, plant_life_cycle, plant_life_form) %>% 
  summarize(cvr_pct = sum(intercepts_pct), .groups = "drop")
