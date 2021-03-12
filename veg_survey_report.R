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
#' 
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
#' T code "NV" or "no vegetation" is present and may be treated as a species unless 
#' removed or treated differently. NV should be removed for species richness calculations, but should be retained
#' for functional group cover calculations. 
#' 
#' In the imported data, many NA values exist in intercept hits 2-4. These aren't coded "NV". These NA values
#' are treated differently depending on how the species data are used, and so are left in the data frame for now.
spe_df <-
  spe_pull_df %>%
  select(grid_point, transect_point, starts_with("intercept")) %>%
  pivot_longer(starts_with("intercept"),
               names_to = "intercept",
               values_to = "key_plant_species") %>%
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





#' # Survey efficiency: species richness
#' ## Data wrangling
#' The species data must be transformed into samples-species matrices for each grid point. `Split()` 
#' facilitates this by separating each grid point into a separate list object. The resulting list is then
#' passed to a function with `lapply()` to create the samples-species matrices. 
#' 
#' Where `key_plant_code = NV`, 
#' special treatment must be considered to prevent the species accumulation to consider that "NV" is a species. 
#' NA values are recoded to "NV", and after pivoting each table to a samples-species matrix, the column "NV" is
#' simply removed. This answers the immediate need to prevent including "NV" with species, but it also 
#' preserves the number of rows in each list object (200 rows in all but seven cases). This will make sure that 
#' species rarefaction reaches to the actual species richness detected at each grid point. If "NV" rows were simply
#' filtered out of the original long-form dataset, many grid points would have fewer than 200 rows, and 
#' the richness would cease accumulating before the actual richness measured was reached.

spe_list<-
  spe_df %>%
  replace_na(list(key_plant_species = 360)) %>% 
  select(-intercept) %>%
  left_join(spe_meta_df %>% select(key_plant_species, key_plant_code), by = "key_plant_species") %>%
  select(-key_plant_species) %>%
  mutate(detected = 1) %>%
  split(paste0("gp_", factor(spe_mat_df$grid_point)))

spe_mat_list <-
  lapply(spe_list, function(x) {
    data.frame(
      x %>%
        pivot_wider(
          names_from = key_plant_code,
          values_from = detected,
          values_fn = min,
          values_fill = 0
        ) %>%
        arrange(transect_point) %>%
        select(-NV,-grid_point),
      row.names = 1
    )
  })

#' Next, the list is passed to another function with `lapply()`, this time to calculate the species
#' rarefaction on each samples-species matrix. Species rarefaction is predicted at known sampling
#' efforts, which correspond with the number of pin drops. The vector `sample_points` controls the 
#' desired rarefaction of species richness data.

sample_points <- c(200, 160, 120, 100, 80, 40)

spe_fun = function(x) {
  data.frame(
    sample_points = factor(sample_points),
    pred = specaccum(x, method = "rarefaction") %>% predict(., newdata = sample_points)
  )
}

## Rarefy the species data (this step takes a few minutes).
spe_pred <-
  lapply(spe_mat_list, spe_fun) %>%
  bind_rows(.id = "id") %>%
  group_by(id) %>%
  mutate(pred_pct = (pred / max(pred)) * 100) %>%
  ungroup() %>%
  separate(id, into = c(NA, "grid_point"), sep = "_", remove = FALSE)

# Subset richness data to a small set of grid_points to use as examples of accumulation curves
rows_spe_pred <-
  spe_pred %>% drop_na() %>% filter(sample_points == 200)

example_rows <-
  trunc(dim(rows_spe_pred)[1] * c(1 / dim(rows_spe_pred)[1], 0.25, 0.50, 0.75, 1.00))

example_gp <-
  rows_spe_pred %>%
  arrange(pred) %>%
  slice(example_rows) %>%
  pull(id)

example_curves <-
  data.frame(
    c(1:200),
    specaccum(spe_mat_list[[example_gp[1]]])$richness,
    specaccum(spe_mat_list[[example_gp[2]]])$richness,
    specaccum(spe_mat_list[[example_gp[3]]])$richness,
    specaccum(spe_mat_list[[example_gp[4]]])$richness,
    specaccum(spe_mat_list[[example_gp[5]]])$richness
  )

names(example_curves) <- c("sample_points", example_gp)


# Find species in YVP but not GV

qspe_filter <-
  qspe_pull_df %>% 
  select(grid_point, subplot, key_plant_code) %>% 
  anti_join(spe_mat_df, by = c("grid_point", "key_plant_code")) %>% 
  mutate(detected = 1)
  
qspe_list <-  
  split(qspe_filter, paste0("gp_", factor(qspe_filter$grid_point)))

qspe_mat_list <-
  lapply(qspe_list, function(x) {
    data.frame(
      x %>%
        pivot_wider(
          names_from = key_plant_code,
          values_from = detected,
          values_fn = min,
          values_fill = 0
        ) %>%
        arrange(subplot) %>%
        select(-grid_point),
      row.names = 1
    )
  })

# Create vectors of predicted richness for desired number of subplots
subplots <- c(10, 8, 6, 4, 2)

qspe_fun = function(x) {
  data.frame(
    subplots = factor(subplots),
    pred = specaccum(x, method = "rarefaction") %>% predict(., newdata = subplots)
  )
}

# This is where the accumulations at desired points is calculated
qspe_pred <-
  lapply(qspe_mat_list, qspe_fun) %>%
  bind_rows(.id = "id") %>%
  group_by(id) %>%
  mutate(pred_pct = (pred / max(pred)) * 100) %>%
  ungroup() %>%
  separate(id,
           into = c(NA, "grid_point"),
           sep = "_",
           remove = FALSE)

# Results
# ——————————————————————————————————

ggplot(spe_pred %>% drop_na(), aes(x = sample_points, y = pred_pct)) +
  geom_boxplot(fill = "gray80") +
  labs(title = "All grid points") +
  theme_bgl

ggplot(
  spe_pred %>% drop_na() %>% filter(
    type3_vegetation_indicators == "uncultivated grassland native or degraded"
  ),
  aes(x = sample_points, y = pred_pct)
) +
  geom_boxplot(fill = "gray80") +
  labs(title = "Uncultivated grassland grid points") +
  theme_bgl

ggplot(
  data = example_curves %>% pivot_longer(-sample_points, names_to = "grid_pt"),
  aes(x = sample_points, y = value, group = grid_pt)
) +
  geom_vline(xintercept = 100) +
  geom_line(aes(linetype = grid_pt)) +
  scale_x_continuous(breaks = c(0, sample_points)) +
  theme_bgl

# YVP species to add
ggplot(qspe_pred, aes(x = subplots, y = pred)) +
  geom_boxplot()

qspe_pred


