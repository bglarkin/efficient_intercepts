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

# API keys
# ————————————————————————————————————————
# API keys are pulled from local resources and are not available in the hosted environment.
# Users must have API key for Google Big Query

# Big Query API Key (local file)
bq_auth(path = "/content/mpg-data-warehouse-api_key-master.json")
Sys.setenv(BIGQUERY_TEST_PROJECT = "mpg-data-warehouse")
billing <- bq_test_project()

# Global functions and styles
# ————————————————————————————————————————
# Load supplemental.R, a text file from Google Drive.
# This file contains theme and style elements for plotting.
source("https://drive.google.com/uc?id=1EYkUWlqsH6g-rqiu27QQsUvl4KaNPzzt") 


#### Source data ####
# ——————————————————————————————————

# Point-intercept species data
# ————————————————————————————————————————
# Make raw data available locally by pulling from the MPG Data Warehouse (`gpint_pull_df`), 
# and then pre-process to create two objects that will be joined with metadata and used for analysis:
# 1. Species detections in long-form with selected fields only: `gpint_spe_df`
# 1. `NA` values must be replaced with `360`, the code for "no vegetation", to preserve matrix dimensions and accurately reflect sampling effort in rarefaction analysis. These will be removed after transformation to a sparse matrix using `select()`
# 1. Height data with selected fields only: `gpint_ht_df`

spe_pull_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_point_intercept_gridVeg.gridVeg_point_intercept_vegetation`
  WHERE year = 2016
  "
spe_pull_bq <- bq_project_query(billing, spe_pull_sql)
spe_pull_tb <- bq_table_download(spe_pull_bq)
spe_pull_df <- as.data.frame(spe_pull_tb) %>% glimpse()
# Note that in `spe_pull_df`, with 200 intercepts per grid point, the total number of records indicated here should not be possible. Under investigation, I found that 7 grid points contain only 199 records, so no correction to the data is possible. For this analysis, a small number of missing records should not affect the interpretation.

# 1. Transform species detections to long form
# Replace NA values with 360
spe_df <- 
  spe_pull_df %>% 
  select(grid_point, transect_point, starts_with("intercept")) %>% 
  pivot_longer(starts_with("intercept"), names_to = "intercept", values_to = "key_plant_species") %>% 
  replace_na(list(key_plant_species = 360)) %>% 
  glimpse()

# 2. Produce df of height data
ht_df <-
  spe_pull_df %>% 
  select(grid_point, transect_point, height_intercept_1) %>% 
  glimpse()


# Point-intercept ground cover data
# ————————————————————————————————————————
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


# Vegetation species metadata
# ————————————————————————————————————————
spe_meta_sql <-
  "
  SELECT key_plant_species, key_plant_code, plant_native_status, plant_life_cycle, plant_life_form, plant_name_sci, plant_name_common
  FROM `mpg-data-warehouse.vegetation_species_metadata.vegetation_species_metadata`
  "
spe_meta_bq <- bq_project_query(billing, spe_meta_sql)
spe_meta_tb <- bq_table_download(spe_meta_bq)
spe_meta_df <- as.data.frame(spe_meta_tb) %>% glimpse()


# Grid point metadata
# ————————————————————————————————————————
gp_meta_sql <- 
  "
  SELECT *
  FROM `mpg-data-warehouse.grid_point_summaries.location_position_classification`
  "
gp_meta_bq <- bq_project_query(billing, gp_meta_sql)
gp_meta_tb <- bq_table_download(gp_meta_bq)
gp_meta_df <- as.data.frame(gp_meta_tb) %>% glimpse()




#### Species richness ####
# ——————————————————————————————————
# UNDER DEVELOPMENT
# Create analysis dataframes by joining data with metadata
# Test analysis scripts for efficacy

# Data wrangling
# ————————————————————————————————————————
# objects for use in the loop (revise annotation)
grid_points <- sort(unique(spe_df$grid_point))
spe_mat_list <- vector(mode = "list", length = length(grid_points))
names(spe_mat_list) = c(paste0("gp_", grid_points))

# species data with alpha codes and without intercept field
spe_mat_df <-
  spe_df %>% 
  select(-intercept) %>% 
  left_join(spe_meta_df %>% select(key_plant_species, key_plant_code), by = "key_plant_species") %>% 
  select(-key_plant_species) %>% 
  mutate(detected = 1) %>% 
  glimpse()

# Create list objects for each grid point and transform objects to species-samples matrices
for (i in 1:length(grid_points)) {
  # filter spe_mat_df to individual grid points and pivot to a species-samples matrix
  spe_mat_temp_df <-
    data.frame(
      spe_mat_df %>% 
        filter(grid_point == grid_points[i]) %>% 
        pivot_wider(names_from = key_plant_code, values_from = detected, values_fn = min, values_fill = 0) %>% 
        arrange(transect_point) %>% 
        select(-NV, -grid_point),
      row.names = 1
    )
  # store filtered data as list object
  spe_mat_list[[i]] <-
    assign(
      paste0("gp_", grid_points[i]),
      spe_mat_temp_df
    )
}

# Create vectors of predicted richness for desired number of intercept sample points
sample_points <- c(200, 160, 120, 100, 80, 40)

spe_fun = function(x) {
  data.frame(
    sample_points = factor(sample_points),
    pred = specaccum(x, method = "exact") %>% predict(., newdata = sample_points)
  )
}

# This is where the accumulations at desired points is calculated
spe_pred <- 
  lapply(spe_mat_list, spe_fun) %>% 
  bind_rows(.id = "id") %>% 
  group_by(id) %>% 
  mutate(pred_pct = (pred / max(pred)) * 100) %>% 
  ungroup() %>% 
  separate(id, into = c(NA, "grid_point"), sep = "_", remove = FALSE) %>% 
  left_join(gp_meta_df %>% select(grid_point, type3_vegetation_indicators) %>% mutate(grid_point = as.character(grid_point)), by = "grid_point")


# Subset richness data to a small set of grid_points to use as examples of accumulation curves
rows_spe_pred <- spe_pred %>% drop_na() %>% filter(sample_points == 200)

example_rows <- trunc(dim(rows_spe_pred)[1] * c(1 / dim(rows_spe_pred)[1], 0.25, 0.50, 0.75, 1.00))

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


# Results
# ——————————————————————————————————

ggplot(spe_pred %>% drop_na(), aes(x = sample_points, y = pred_pct)) +
  geom_boxplot(fill = "gray90") +
  labs(title = "All grid points") +
  theme_bgl

ggplot(spe_pred %>% drop_na() %>% filter(type3_vegetation_indicators == "uncultivated grassland native or degraded"), aes(x = sample_points, y = pred_pct)) +
  geom_boxplot(fill = "gray90") +
  labs(title = "Uncultivated grassland grid points") +
  theme_bgl

ggplot(data = example_curves %>% pivot_longer(-sample_points, names_to = "grid_pt"),
       aes(x = sample_points, y = value, group = grid_pt)) +
  geom_vline(xintercept = 100) +
  geom_line() +
  scale_x_continuous(breaks = c(0, sample_points)) +
  theme_bgl 






#### Ground cover ####
# ——————————————————————————————————

# Choose ground cover types to keep
grcov_common <-
  grcov_pull_df %>% 
  count(intercept_ground_code) %>% 
  mutate(pct_rank = percent_rank(n) %>% round(., 2)) %>% 
  arrange(-pct_rank) %>% 
  filter(pct_rank >= 0.50)
grcov_common %>% 
  kable(format = "pandoc")
grcov_common_codes <-
  grcov_common$intercept_ground_code

p_285 <-
  grcov_pull_df %>%  
  filter(grid_point == 285) %>% 
  mutate(detected = 1) %>% 
  glimpse() 



# Data wrangling
# ————————————————————————————————————————


## bootstrap approach
b <- 1000
gp <- sort(unique(grcov_pull_df$grid_point))
samp_200_list = vector(mode = "list", length = b)
samp_160_list = vector(mode = "list", length = b)
samp_120_list = vector(mode = "list", length = b)
samp_100_list = vector(mode = "list", length = b)
samp_80_list = vector(mode = "list", length = b)
samp_40_list = vector(mode = "list", length = b)
grcov_sample_list = vector(mode = "list", length = length(gp))



for (i in 1:length(gp)) {
  grcov_temp_df <-
    grcov_pull_df %>% 
    filter(grid_point == gp[i]) %>% 
    mutate(detected = 1)
  for (j in 1:b) {
    samp_200_list[[j]] <-
      sample_n(grcov_temp_df, size = 200, replace = TRUE) %>%
      group_by(grid_point, intercept_ground_code) %>%
      summarize(pct = sum(detected) / 2, .groups = "drop") %>% 
      mutate(sampled_points = 200) %>% 
      ungroup()
    samp_160_list[[j]] <-
      sample_n(grcov_temp_df, size = 160, replace = TRUE) %>%
      group_by(grid_point, intercept_ground_code) %>%
      summarize(pct = sum(detected) / 1.6, .groups = "drop") %>% 
      mutate(sampled_points = 160) %>% 
      ungroup()
    samp_120_list[[j]] <-
      sample_n(grcov_temp_df, size = 120, replace = TRUE) %>%
      group_by(grid_point, intercept_ground_code) %>%
      summarize(pct = sum(detected) / 1.2, .groups = "drop") %>% 
      mutate(sampled_points = 120) %>% 
      ungroup()
    samp_100_list[[j]] <-
      sample_n(grcov_temp_df, size = 100, replace = TRUE) %>%
      group_by(grid_point, intercept_ground_code) %>%
      summarize(pct = sum(detected) / 1, .groups = "drop") %>% 
      mutate(sampled_points = 100) %>% 
      ungroup()
    samp_80_list[[j]] <-
      sample_n(grcov_temp_df, size = 80, replace = TRUE) %>%
      group_by(grid_point, intercept_ground_code) %>%
      summarize(pct = sum(detected) / 0.8, .groups = "drop") %>% 
      mutate(sampled_points = 80) %>% 
      ungroup()
    samp_40_list[[j]] <-
      sample_n(grcov_temp_df, size = 40, replace = TRUE) %>%
      group_by(grid_point, intercept_ground_code) %>%
      summarize(pct = sum(detected) / 0.4, .groups = "drop") %>% 
      mutate(sampled_points = 40) %>% 
      ungroup()
  }
  samp_temp_df <- 
    bind_rows(samp_200_list, samp_160_list, samp_120_list, samp_100_list, samp_80_list, samp_40_list) %>% 
    group_by(grid_point, sampled_points, intercept_ground_code) %>%
    summarize(boot_mean = mean(pct), boot_se = sd(pct), .groups = "drop") %>% 
    ungroup()
  grcov_sample_list[[i]] <- samp_temp_df
}

str(grcov_sample_list)



# This only works for one grid point, a test for now.
bind_rows(grcov_sample_list) %>% 
  filter(grid_point == 2) %>% 
  mutate(upr = boot_mean + boot_se, lwr = boot_mean - boot_se) %>% 
  ggplot(aes(x = sampled_points, y = boot_mean, group = intercept_ground_code)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = intercept_ground_code), alpha = 0.1) +
  geom_line(aes(color = intercept_ground_code)) +
  scale_x_continuous(breaks = c(40, 80, 100, 120, 160, 200)) +
  scale_color_discrete_sequential(name = "", palette = "Hawaii") +
  scale_fill_discrete_sequential(name = "", palette = "Hawaii") +
  theme_bgl
