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
packages_needed = c("tidyverse",
                    "knitr",
                    "colorspace",
                    "rjson",
                    "vegan",
                    "parallel")
packages_installed = packages_needed %in% rownames(installed.packages())

if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}

# Big R Query (slow loading)
packages_needed = c("bigrquery") # comma delimited vector of package names
packages_installed = packages_needed %in% rownames(installed.packages())

if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
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
# Replace NA values with 360 STOP!
# Don't replace NA values; this leads to a strong overabundance of NV detections!
spe_df <-
  spe_pull_df %>%
  select(grid_point, transect_point, starts_with("intercept")) %>%
  pivot_longer(starts_with("intercept"),
               names_to = "intercept",
               values_to = "key_plant_species") %>%
  # replace_na(list(key_plant_species = 360)) %>%
  drop_na() %>% 
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


# Quadrat species data
# ————————————————————————————————————————
# Quadrat-based species richness from 2017 (yvp)

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
  # store filtered data as list object
  spe_mat_list[[i]] <-
    assign(paste0("gp_", grid_points[i]),
           spe_mat_temp_df)
}

# Create vectors of predicted richness for desired number of intercept sample points
sample_points <- c(200, 160, 120, 100, 80, 40)

spe_fun = function(x) {
  data.frame(
    sample_points = factor(sample_points),
    pred = specaccum(x, method = "rarefaction") %>% predict(., newdata = sample_points)
  )
}

# This is where the accumulations at desired points is calculated
spe_pred <-
  lapply(spe_mat_list, spe_fun) %>%
  bind_rows(.id = "id") %>%
  group_by(id) %>%
  mutate(pred_pct = (pred / max(pred)) * 100) %>%
  ungroup() %>%
  separate(id,
           into = c(NA, "grid_point"),
           sep = "_",
           remove = FALSE) %>%
  left_join(
    gp_meta_df %>% select(grid_point, type3_vegetation_indicators) %>% mutate(grid_point = as.character(grid_point)),
    by = "grid_point"
  )


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
yvp_spe_mat_df <- qspe_pull_df %>% 
  select(grid_point, subplot, key_plant_code) %>% 
  anti_join(spe_mat_df, by = c("grid_point", "key_plant_code")) %>% 
  mutate(detected = 1) %>% 
  glimpse()

# Control variables  for use in the loop
yvp_grid_points <- sort(unique(yvp_spe_mat_df$grid_point))
yvp_spe_mat_list <- vector(mode = "list", length = length(yvp_grid_points))
names(yvp_spe_mat_list) = c(paste0("gp_", yvp_grid_points))

# Create list objects for each grid point and transform objects to species-samples matrices
for (i in 1:length(yvp_grid_points)) {
  # filter yvp_spe_mat_df to individual grid points and pivot to a species-samples matrix
  yvp_spe_mat_temp_df <-
    data.frame(
      yvp_spe_mat_df %>%
        filter(grid_point == yvp_grid_points[i]) %>%
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
  # store filtered data as list object
  yvp_spe_mat_list[[i]] <-
    assign(paste0("gp_", yvp_grid_points[i]),
           yvp_spe_mat_temp_df)
}

# Create vectors of predicted richness for desired number of subplots
subplots <- c(10, 8, 6, 4, 2)

yvp_spe_fun = function(x) {
  data.frame(
    subplots = factor(subplots),
    pred = specaccum(x, method = "rarefaction") %>% predict(., newdata = subplots)
  )
}

# This is where the accumulations at desired points is calculated
yvp_spe_pred <-
  lapply(yvp_spe_mat_list, yvp_spe_fun) %>%
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
ggplot(yvp_spe_pred, aes(x = subplots, y = pred)) +
  geom_boxplot()

yvp_spe_pred


#### Moving averages example ####
# ——————————————————————————————————

gr_cumean_df <-
  grcov_pull_df %>%
  filter(
    grid_point %in% c(12, 20, 22, 180, 181, 184, 202, 203, 205, 212, 246),
    intercept_ground_code %in% c("S", "BV", "L", "M")
  ) %>%
  mutate(
    detected = 1,
    intercept_ground_code = recode(
      intercept_ground_code,
      S = "soil",
      BV = "basal veg",
      L = "litter",
      M = "moss"
    )
  ) %>%
  rename(ground_code = intercept_ground_code) %>%
  as_tibble() %>%
  glimpse()

n_pt <- length(unique(gr_cumean_df$grid_point))
n_gr <- length(unique(gr_cumean_df$ground_code))

gr_samp_df <- data.frame(
  point = rep(rep(1:200, n_pt), n_gr),
  samp_point = rep(rep(sample(1:200, replace = FALSE), n_pt), n_gr),
  grid_point = rep(rep(unique(
    gr_cumean_df$grid_point
  ), each = 200), n_gr),
  ground_code = rep(unique(gr_cumean_df$ground_code), each = n_pt * 200),
  transect_point = rep(rep(
    c(
      paste0("N", 1:50),
      paste0("E", 1:50),
      paste0("S", 1:50),
      paste0("W", 1:50)
    ), n_pt
  ), n_gr)
) %>% as_tibble()

gr_samp_df %>%
  left_join(gr_cumean_df,
            by = c("grid_point", "ground_code", "transect_point")) %>%
  mutate(detected = case_when(is.na(detected) ~ 0, TRUE ~ as.numeric(detected))) %>%
  arrange(grid_point, ground_code, samp_point) %>%
  group_by(grid_point, ground_code) %>%
  mutate(cummean_detected_pct = cummean(detected) * 100) %>%
  ggplot(aes(x = samp_point, y = cummean_detected_pct, group = grid_point)) +
  geom_step(size = 0.2) +
  facet_wrap(vars(ground_code), scales = "free_y") +
  labs(x = "sampled transect point", y = "percent cover") +
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

grcov_pull_df %>% glimpse()
n_points <- length(unique(grcov_pull_df$grid_point))
grcov_list <-
  split(grcov_pull_df %>% select(-transect_point),
        factor(grcov_pull_df$grid_point))


grcov_boot <- function(pts) {
  lapply(grcov_list, function(x, pts) {
    slice_sample(x, n = pts * 1000, replace = TRUE)
  }, pts = pts) %>%
    bind_rows() %>%
    mutate(detected = 1, boot_run = rep(rep(1:1000, each = pts), n_points)) %>%
    group_by(grid_point, boot_run, intercept_ground_code) %>%
    summarize(pct_detected = sum(detected) / pts * 100,
              .groups = "drop") %>%
    group_by(grid_point, intercept_ground_code) %>%
    summarize(
      boot_pct_mean = mean(pct_detected),
      boot_pct_se = sd(pct_detected),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(sampled_n = factor(pts))
}

grcov_boot_df <-
  bind_rows(
    grcov_boot(20),
    grcov_boot(40),
    grcov_boot(80),
    grcov_boot(100),
    grcov_boot(120),
    grcov_boot(160),
    grcov_boot(200)
  ) %>%
  glimpse()

ggplot(grcov_boot_df,
       aes(x = sampled_n, y = boot_pct_se, group = grid_point)) +
  geom_line() +
  facet_wrap(vars(intercept_ground_code))
ggplot(grcov_boot_df,
       aes(x = sampled_n, y = boot_pct_mean, group = grid_point)) +
  geom_line() +
  facet_wrap(vars(intercept_ground_code))

#### ground cover figure ####
grcov_boot_df_200 <- grcov_boot_df %>%
  filter(sampled_n == 200) %>%
  rename(boot_pct_mean_200 = boot_pct_mean) %>%
  select(-boot_pct_se,-sampled_n)

grcov_boot_df_adj <-
  grcov_boot_df %>%
  left_join(grcov_boot_df_200,
            by = c("grid_point", "intercept_ground_code")) %>%
  mutate(boot_pct_mean_adj = boot_pct_mean - boot_pct_mean_200)

ggplot(
  grcov_boot_df_adj %>% filter(
    intercept_ground_code %in% c("BG", "BV", "G", "L", "LIC", "M", "R", "S"),
    sampled_n != 20
  ),
  aes(x = sampled_n, y = boot_pct_mean_adj)
) +
  geom_line(
    aes(y = boot_pct_mean_adj + boot_pct_se, group = grid_point),
    color = "gray80",
    size = 0.05
  ) +
  geom_line(
    aes(y = boot_pct_mean_adj - boot_pct_se, group = grid_point),
    color = "gray80",
    size = 0.05
  ) +
  geom_boxplot(outlier.size = 0.6) +
  labs(title = "Pct ground cover") +
  facet_wrap(vars(intercept_ground_code)) +
  theme_bgl

# What are differences in SE for each group?
grcov_boot_df_adj %>%
  group_by(sampled_n) %>%
  summarize(se_max = max(boot_pct_se), .groups = "drop") %>%
  filter(sampled_n %in% c(40, 80, 100, 120, 160, 200)) %>%
  ungroup() %>%
  pivot_wider(names_from = sampled_n,
              values_from = se_max,
              names_prefix = "se_samp_") %>%
  kable(format = "pandoc", caption = "Ground cover SE values")

# Need to compare basal veg with YVP; something is really going on
# Maybe just mention some numbers, this should support using both quadrats and points
# during surveys.


#### Height ####
# ——————————————————————————————————

ht_df %>% glimpse()
# Height data includes many NAs. These happen when no vegetation is detected at a point
ht_na <-
  ht_df %>% group_by(grid_point) %>% filter(is.na(height_intercept_1)) %>% count(name = "count_NA") %>% arrange(-count_NA)
ggplot(ht_na, aes(x = count_NA)) +
  geom_histogram(binwidth = 1,
                 center = 0.5,
                 closed = "left") +
  geom_vline(xintercept = 40)
# NA could be NA or 0. If question is about the height of existing mature vegetation, NA is preferred
# If question is about the average height of the surface of vegetation, 0 is preferred
# Calculate summaries for both scenarios

# Height of existing vegetation (NA are kept as NA)
# Excluding NA values could introduce bias into bootstrap sample
# Filter to points that have fewer than 40 NA values, then use 160 as the maximum number of points for resampling
# This prevents over-sampling because few points have >160 measurements
# Use a vector of grid_points with < 40 NA to filter the data
ht_gp_filter <- ht_na %>% filter(count_NA < 40) %>% pull(grid_point)
ht_df_filter <-
  ht_df %>% filter(grid_point %in% ht_gp_filter) %>% drop_na()
n_points_ht_filter <- length(ht_gp_filter)
ht_list_filter <-
  split(ht_df_filter %>% select(-transect_point),
        factor(ht_df_filter$grid_point))
ht_list_filter %>% bind_rows() %>% ggplot(aes(x = height_intercept_1)) + geom_histogram(binwidth = 1,
                                                                                        center = 0.5,
                                                                                        closed = "left")
# Split function returned identical data (not shown)

ht_boot_filter <- function(pts) {
  lapply(ht_list_filter, function(x, pts) {
    slice_sample(x, n = pts * 1000, replace = TRUE)
  }, pts = pts) %>%
    bind_rows() %>%
    mutate(boot_run = rep(rep(1:1000, each = pts), n_points_ht_filter)) %>%
    group_by(grid_point, boot_run) %>%
    summarize(ht_mean = mean(height_intercept_1),
              .groups = "drop") %>%
    group_by(grid_point) %>%
    summarize(
      ht_boot_mean = mean(ht_mean),
      ht_boot_se = sd(ht_mean),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(sampled_n = factor(pts))
}

ht_boot_filtered <-
  bind_rows(
    ht_boot_filter(20),
    ht_boot_filter(40),
    ht_boot_filter(80),
    ht_boot_filter(100),
    ht_boot_filter(120),
    ht_boot_filter(160)
  )
ggplot(ht_boot_filtered,
       aes(x = sampled_n, y = ht_boot_se, group = grid_point)) +
  geom_line()
ggplot(ht_boot_filtered,
       aes(x = sampled_n, y = ht_boot_mean, group = grid_point)) +
  geom_line()

#### height figure NA exclude ####
# 160 is max sampled_n because few points have >160 height measurements
ht_boot_filtered_160 <- ht_boot_filtered %>%
  filter(sampled_n == 160) %>%
  rename(ht_boot_mean_160 = ht_boot_mean) %>%
  select(-ht_boot_se,-sampled_n)

ht_boot_filtered_adj <-
  ht_boot_filtered %>%
  left_join(ht_boot_filtered_160, by = c("grid_point")) %>%
  mutate(boot_ht_mean_adj = ht_boot_mean - ht_boot_mean_160)

ggplot(ht_boot_filtered_adj, aes(x = sampled_n, y = boot_ht_mean_adj)) +
  geom_line(
    aes(y = boot_ht_mean_adj + ht_boot_se, group = grid_point),
    color = "gray80",
    size = 0.05
  ) +
  geom_line(
    aes(y = boot_ht_mean_adj - ht_boot_se, group = grid_point),
    color = "gray80",
    size = 0.05
  ) +
  geom_boxplot(outlier.size = 0.6) +
  labs(title = "Height with NULL values excluded") +
  theme_bgl

# What are differences in SE for each group?
ht_boot_filtered_adj %>%
  group_by(sampled_n) %>%
  summarize(se_max = max(ht_boot_se), .groups = "drop") %>%
  filter(sampled_n %in% c(40, 80, 100, 120, 160)) %>%
  ungroup() %>%
  pivot_wider(names_from = sampled_n,
              values_from = se_max,
              names_prefix = "se_samp_") %>%
  kable(format = "pandoc", caption = "Height with NULL values excluded")


# Height of vegetation surface (NA = 0)
# Use all points
# Replace NA with 0
# As with ground cover, 7 grid points are incomplete (199 values). This will be ignored.
n_points_ht <- length(unique(ht_df$grid_point))
ht_list <-
  split(ht_df %>% select(-transect_point) %>% replace_na(list(height_intercept_1 = 0)),
        factor(ht_df$grid_point))
ht_list %>% bind_rows() %>% ggplot(aes(x = height_intercept_1)) + geom_histogram(binwidth = 1,
                                                                                 center = 0.5,
                                                                                 closed = "left")


ht_boot <- function(pts) {
  lapply(ht_list, function(x, pts) {
    slice_sample(x, n = pts * 1000, replace = TRUE)
  }, pts = pts) %>%
    bind_rows() %>%
    mutate(boot_run = rep(rep(1:1000, each = pts), n_points_ht)) %>%
    group_by(grid_point, boot_run) %>%
    summarize(ht_mean = mean(height_intercept_1),
              .groups = "drop") %>%
    group_by(grid_point) %>%
    summarize(
      ht_boot_mean = mean(ht_mean),
      ht_boot_se = sd(ht_mean),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(sampled_n = factor(pts))
}

ht_boot_df <-
  bind_rows(
    ht_boot(20),
    ht_boot(40),
    ht_boot(80),
    ht_boot(100),
    ht_boot(120),
    ht_boot(160),
    ht_boot(200)
  )
ggplot(ht_boot_df, aes(x = sampled_n, y = ht_boot_se, group = grid_point)) +
  geom_line()
ggplot(ht_boot_df,
       aes(x = sampled_n, y = ht_boot_mean, group = grid_point)) +
  geom_line()

#### height figure NA replaced with 0 ####
ht_boot_df_200 <- ht_boot_df %>%
  filter(sampled_n == 200) %>%
  rename(ht_boot_mean_200 = ht_boot_mean) %>%
  select(-ht_boot_se,-sampled_n)

ht_boot_df_adj <-
  ht_boot_df %>%
  left_join(ht_boot_df_200, by = c("grid_point")) %>%
  mutate(boot_ht_mean_adj = ht_boot_mean - ht_boot_mean_200)

ggplot(ht_boot_df_adj, aes(x = sampled_n, y = boot_ht_mean_adj)) +
  geom_line(
    aes(y = boot_ht_mean_adj + ht_boot_se, group = grid_point),
    color = "gray80",
    size = 0.05
  ) +
  geom_line(
    aes(y = boot_ht_mean_adj - ht_boot_se, group = grid_point),
    color = "gray80",
    size = 0.05
  ) +
  geom_boxplot(outlier.size = 0.6) +
  labs(title = "Height with NULL vals set to zero") +
  theme_bgl

# What are differences in SE for each group?
ht_boot_df_adj %>%
  group_by(sampled_n) %>%
  summarize(se_max = max(ht_boot_se), .groups = "drop") %>%
  filter(sampled_n %in% c(40, 80, 100, 120, 160)) %>%
  ungroup() %>%
  pivot_wider(names_from = sampled_n,
              values_from = se_max,
              names_prefix = "se_samp_") %>%
  kable(format = "pandoc", caption = "Height with NULL vals set to zero")



#### Pct cover in functional groups ####
# ——————————————————————————————————
# fg for functional groups

# DON'T FILTER THE FUNCTIONAL GROUPS HERE OR THEY WILL ADD TO 100%

spe_df %>% glimpse()
spe_meta_df %>% glimpse()

fg_df <- spe_df %>%
  left_join(spe_meta_df, by = "key_plant_species") %>%
  select(grid_point,
         key_plant_code,
         plant_native_status,
         plant_life_cycle,
         plant_life_form) # %>%
  # filter(
  #   plant_native_status %in% c("native", "nonnative"),
  #   plant_life_cycle %in% c("annual", "biennial", "perennial"),
  #   plant_life_form %in% c("graminoid", "forb", "shrub")
  # )

fg_df %>% group_by(grid_point,
                   plant_native_status,
                   plant_life_cycle,
                   plant_life_form) %>%
  count() %>%
  ggplot(aes(x = n / 2)) +
  geom_histogram(
    aes(fill = plant_native_status, color = plant_native_status),
    binwidth = 10,
    center = 5,
    closed = "left",
    alpha = 0.7,
    position = "identity"
  ) +
  facet_grid(rows = vars(plant_life_cycle),
             cols = vars(plant_life_form)) +
  labs(x = "pct_cover") +
  theme_bw()

# Bootstrap functions and variables
n_points_fg <- length(unique(fg_df$grid_point))
fg_list <- split(fg_df, factor(fg_df$grid_point))

fg_boot <- function(pts) {
  lapply(fg_list, function(x, pts) {
    slice_sample(x, n = pts * 1000, replace = TRUE)
  }, pts = pts) %>%
    bind_rows() %>%
    mutate(detected = 1,
           boot_run = rep(rep(1:1000, each = pts), n_points_fg)) %>%
    group_by(grid_point,
             boot_run,
             plant_native_status,
             plant_life_cycle,
             plant_life_form) %>%
    summarize(pct = sum(detected) / pts * 100, .groups = "drop") %>%
    group_by(grid_point,
             plant_native_status,
             plant_life_cycle,
             plant_life_form) %>%
    summarize(
      boot_pct_mean = mean(pct),
      boot_pct_se = sd(pct),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(sampled_n = factor(pts))
}

fg_boot_mean <-
  bind_rows(
    fg_boot(5),
    fg_boot(10),
    fg_boot(20),
    fg_boot(40),
    fg_boot(80),
    fg_boot(100),
    fg_boot(120),
    fg_boot(160),
    fg_boot(200)
  ) %>%
  glimpse()

fg_boot_mean %>%
  filter(
    plant_native_status %in% c("native", "nonnative"),
    plant_life_cycle %in% c("annual", "biennial", "perennial"),
    plant_life_form %in% c("graminoid", "forb", "shrub")
  ) %>%
  ggplot(aes(
    x = sampled_n,
    y = boot_pct_se,
    group = interaction(grid_point, plant_native_status)
  )) +
  geom_line(aes(color = plant_native_status)) +
  facet_grid(rows = vars(plant_life_cycle),
             cols = vars(plant_life_form)) +
  labs(x = "pct_cover") +
  theme_bgl

fg_boot_mean %>%
  filter(
    plant_native_status %in% c("native", "nonnative"),
    plant_life_cycle %in% c("annual", "biennial", "perennial"),
    plant_life_form %in% c("graminoid", "forb", "shrub")
  ) %>%
  ggplot(aes(
         x = sampled_n,
         y = boot_pct_mean,
         group = interaction(grid_point, plant_native_status)
       )) +
  geom_line(aes(color = plant_native_status)) +
  facet_grid(rows = vars(plant_life_cycle),
             cols = vars(plant_life_form)) +
  labs(x = "pct_cover") +
  theme_bgl

fg_boot_mean %>%
  filter(
    grid_point %in% c(1:10),
    plant_native_status %in% c("native", "nonnative"),
    plant_life_cycle %in% c("annual", "biennial", "perennial"),
    plant_life_form %in% c("graminoid", "forb", "shrub")
  ) %>%
  ggplot(aes(x = sampled_n, y = boot_pct_mean)) +
  geom_line(
    aes(
      y = boot_pct_mean + boot_pct_se,
      group = interaction(grid_point, plant_native_status)
    ),
    color = "gray50",
    size = 0.5
  ) +
  geom_line(
    aes(
      y = boot_pct_mean - boot_pct_se,
      group = interaction(grid_point, plant_native_status)
    ),
    color = "gray50",
    size = 0.5
  ) +
  geom_boxplot(aes(fill = plant_native_status)) +
  facet_grid(rows = vars(plant_life_cycle),
             cols = vars(plant_life_form)) +
  theme_bgl



# Exemplar from one point
# Points 540, 5, 19 are good choices
# would need data from boot runs

pts_all_fg <-
  fg_df %>%
  mutate(detected = 1) %>%
  group_by(grid_point,
           plant_native_status,
           plant_life_cycle,
           plant_life_form) %>%
  filter(
    plant_life_cycle == "perennial",
    plant_life_form %in% c("forb", "graminoid"),
    plant_native_status %in% c("native", "nonnative")
  ) %>%
  summarize(pct = sum(detected) / 2, .groups = "drop") %>% ungroup() %>%
  left_join(gp_meta_df %>% select(grid_point, type3_vegetation_indicators),
            by = "grid_point") %>%
  filter(type3_vegetation_indicators == "uncultivated grassland native or degraded") %>%
  count(grid_point) %>%
  filter(n == 4) %>%
  pull(grid_point)

fg_df %>%
  filter(grid_point %in% pts_all_fg) %>%
  mutate(detected = 1) %>%
  group_by(grid_point,
           plant_native_status,
           plant_life_cycle,
           plant_life_form) %>%
  filter(
    plant_life_cycle == "perennial",
    plant_life_form %in% c("forb", "graminoid"),
    plant_native_status %in% c("native", "nonnative")
  ) %>%
  summarize(pct = sum(detected) / 2, .groups = "drop") %>% ungroup() %>%
  group_by(grid_point) %>%
  summarize(pct_mean = mean(pct), pct_sd = sd(pct)) %>%
  arrange(-pct_mean, pct_sd) %>% print(n = Inf)

fg_boot_runs <- function(pts) {
  lapply(list(fg_list$`5`), function(x, pts) {
    slice_sample(x, n = pts * 1000, replace = TRUE)
  }, pts = pts) %>%
    bind_rows() %>%
    mutate(detected = 1, boot_run = rep(rep(1:1000, each = pts), 1)) %>%
    group_by(grid_point,
             boot_run,
             plant_native_status,
             plant_life_cycle,
             plant_life_form) %>%
    summarize(pct = sum(detected) / pts * 100, .groups = "drop") %>%
    ungroup() %>%
    mutate(sampled_n = factor(pts))
}

fg_boot_example <-
  bind_rows(
    fg_boot_runs(40),
    fg_boot_runs(80),
    fg_boot_runs(100),
    fg_boot_runs(120),
    fg_boot_runs(160),
    fg_boot_runs(200)
  ) %>%
  glimpse()

ggplot(
  fg_boot_example %>% filter(
    plant_life_cycle == "perennial",
    plant_life_form %in% c("forb", "graminoid"),
    plant_native_status %in% c("native", "nonnative")
  ),
  aes(x = pct)
) +
  geom_histogram(
    aes(fill = plant_native_status, color = plant_native_status),
    binwidth = 1,
    center = 0.5,
    closed = "left",
    alpha = 0.3,
    position = "identity"
  ) +
  facet_grid(rows = vars(sampled_n),
             cols = vars(plant_life_form))

## This is the attempt to show a mean corrected to 200 points
fg_cover_200 <- fg_boot_mean %>%
  filter(sampled_n == 200) %>%
  rename(boot_pct_mean_200 = boot_pct_mean) %>%
  select(-boot_pct_se,-sampled_n)

fg_boot_mean_adj <-
  fg_boot_mean %>%
  left_join(
    fg_cover_200,
    by = c(
      "grid_point",
      "plant_native_status",
      "plant_life_cycle",
      "plant_life_form"
    )
  ) %>%
  mutate(boot_pct_mean_adj = boot_pct_mean - boot_pct_mean_200)

#### func grps figure ####
ggplot(
  fg_boot_mean_adj %>% 
    filter(
      plant_native_status %in% c("native", "nonnative"),
      plant_life_cycle %in% c("annual", "perennial"),
      plant_life_form %in% c("graminoid", "forb", "shrub"),
      !(sampled_n %in% c(5, 10, 20))),
  aes(x = sampled_n, y = boot_pct_mean_adj)
) +
  geom_line(
    aes(
      y = boot_pct_mean_adj + boot_pct_se,
      group = interaction(grid_point, plant_native_status)
    ),
    color = "gray80",
    size = 0.05
  ) +
  geom_line(
    aes(
      y = boot_pct_mean_adj - boot_pct_se,
      group = interaction(grid_point, plant_native_status)
    ),
    color = "gray80",
    size = 0.05
  ) +
  geom_boxplot(aes(fill = plant_native_status), outlier.size = 0.6) +
  facet_grid(rows = vars(plant_life_cycle),
             cols = vars(plant_life_form)) +
  labs(title = "Pct cover in functional groups") +
  theme_bgl

# Another way to look at the same thing
fg_boot_mean_adj %>%
  filter(
    plant_native_status %in% c("native", "nonnative"),
    plant_life_cycle %in% c("annual", "perennial"),
    plant_life_form %in% c("graminoid", "forb", "shrub"),
    !(sampled_n %in% c(5, 10, 20)) 
  ) %>% 
  group_by(sampled_n,
           plant_native_status,
           plant_life_cycle,
           plant_life_form) %>%
  summarize(
    se_min = min(boot_pct_se),
    mean_50 = quantile(boot_pct_mean_adj, probs = 0.50),
    se_max = max(boot_pct_se),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = sampled_n, y = mean_50, group = plant_native_status)) +
  geom_ribbon(
    aes(
      ymin = mean_50 - se_max,
      ymax = mean_50 + se_max,
      color = plant_native_status
    ),
    fill = "gray80",
    alpha = 0.2
  ) +
  geom_line(aes(color = plant_native_status)) +
  geom_point(aes(color = plant_native_status)) +
  facet_grid(rows = vars(plant_life_cycle),
             cols = vars(plant_life_form)) +
  theme_bgl
# native status might not be important???

# What are differences in SE for each group?
fg_boot_mean_adj %>%
  filter(
    plant_native_status %in% c("native", "nonnative"),
    plant_life_cycle %in% c("annual", "perennial"),
    plant_life_form %in% c("graminoid", "forb", "shrub") 
  ) %>% 
  group_by(sampled_n,
           plant_native_status,
           plant_life_cycle,
           plant_life_form) %>%
  summarize(se_max = max(boot_pct_se), .groups = "drop") %>%
  filter(sampled_n %in% c(40, 80, 100, 120, 160, 200) &
           plant_life_cycle != "biennial") %>%
  ungroup() %>%
  pivot_wider(names_from = sampled_n,
              values_from = se_max,
              names_prefix = "se_samp_") %>%
  kable(format = "pandoc", caption = "SE in functional groups")




#### Effect of downsampling on model ####
# ——————————————————————————————————

fg_boot_mean %>% 
  filter(sampled_n == 200) %>% 
  filter(
      plant_native_status %in% c("native", "nonnative"),
      plant_life_cycle %in% c("annual", "perennial"),
      plant_life_form %in% c("graminoid", "forb", "shrub")
    ) %>% 
  group_by(grid_point) %>% 
  summarize(sum_pct_cvr = sum(boot_pct_mean), .groups = "drop") %>% 
  ggplot(aes(x = sum_pct_cvr)) +
  geom_histogram()


