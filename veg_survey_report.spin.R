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
#' * [link](https://docs.google.com/document/d/1UfM6ueUNCGtW0-PY8FUWc7vRSBwrFTfDtZdI5gUv35s/edit?usp=sharing) to report on proposed revisions to vegetation survey methods
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
bq_auth(
  path = paste0(getwd(), "/mpg-data-warehouse-api_key-master.json"),
  cache = NULL
)
Sys.setenv(BIGQUERY_TEST_PROJECT = "mpg-data-warehouse")
billing <- bq_test_project()

#' ## Global functions and styles: `theme_bgl`
## Load text file from local working directory
source(paste0(getwd(), "/styles.txt"))

## Calculating the 95% CI will aid plotting later
## Uses `plotrix`
ci_95 = function(x) {
  std.error(x) * qnorm(0.975)
}

## Number of resamples desired for bootstrapping
B <- 1000

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
#' The code "NV" or "no vegetation" may be treated as a species unless removed or treated differently.
#' NV should be removed for species richness calculations, but should be retained
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
#'
#' ## Data wrangling and analysis
#'
#' ### Point-intercept data
#'
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

spe_list <-
  spe_df %>%
  replace_na(list(key_plant_species = 360)) %>%
  select(-intercept) %>%
  left_join(spe_meta_df %>% select(key_plant_species, key_plant_code), by = "key_plant_species") %>%
  select(-key_plant_species) %>%
  mutate(detected = 1) %>%
  split(paste0("gp_", factor(spe_df$grid_point)))

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
        select(-NV, -grid_point),
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
  separate(id,
           into = c(NA, "grid_point"),
           sep = "_",
           remove = FALSE)

#' ### Quadrat data
#' The rarefaction of species data from point-intercept surveys can be compared with a similar
#' analysis of quadrat-based survey data. Quadrat methods have detected different suites of species than
#' point-intercept methods have [report](https://docs.google.com/document/d/1UfM6ueUNCGtW0-PY8FUWc7vRSBwrFTfDtZdI5gUv35s/edit?usp=sharing),
#' suggesting that it may be advisable to combine quadrats with point-intercepts. To investigate this, a
#' rarefaction is also performed with the quadrat data (`qspe`), but before rarefying, the species are filtered
#' to include only those which were detected by quadrats but not by point-intercepts. The results will show how
#' many species might be added to point-intercept surveys by the number of quadrats added.

## The code here closely follows the previous code for point-intercept data,
## but because some column names and other variables differ slightly,
## the functions are repeated with slight edits insetad of being handled
## programatically.
qspe_filter <-
  qspe_pull_df %>%
  select(grid_point, subplot, key_plant_species, key_plant_code) %>%
  anti_join(spe_df, by = c("grid_point", "key_plant_species")) %>%
  select(-key_plant_species) %>%
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

## Rarefy species for desired number of quadrats
quads <- c(10, 8, 6, 4, 2)

qspe_fun = function(x) {
  data.frame(
    quads = factor(quads),
    pred = specaccum(x, method = "rarefaction") %>% predict(., newdata = quads)
  )
}

## This is where the accumulations at desired points is calculated
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
## 13 rows of predicted values are missing because (presumably) some quadrats detected
## no species that weren't also detected by point-intercept surveys

#' ## Results
#'
#' ### Example rarefaction curves
#'
#' The following code and produces rarefaction curves from five grid points, representing the
#' quantiles of total species richness across the survey. The figure provides an example of
#' the range of species accumulation at points across the ranch.

## Subset richness data to five grid points with a range of species richness
rows_spe_pred <-
  spe_pred %>% drop_na() %>% filter(sample_points == 200)
example_rows <-
  trunc(dim(rows_spe_pred)[1] * c(1 / dim(rows_spe_pred)[1], 0.25, 0.50, 0.75, 1.00))
example_gp <-
  rows_spe_pred %>%
  arrange(pred) %>%
  slice(example_rows) %>%
  pull(id)

## Filter the example data from `spe_mat_list`
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


## The figure shows a range of species accumulation across the surveyed grid points
#+ species_accum_examples
ggplot(
  data = example_curves %>% pivot_longer(-sample_points, names_to = "grid_pt"),
  aes(x = sample_points, y = value, group = grid_pt)
) +
  geom_line(aes(linetype = grid_pt)) +
  scale_linetype(name = "grid point") +
  scale_x_continuous(breaks = c(0, sample_points)) +
  labs(x = "point intercepts (n)", y = "species (n)") +
  theme_bgl

#' ### Species richness vs. survey effort
#'
#' Species richness in the point-intercept survey data ranges from 2 to 55.
#' Species richness predicted by rarefaction declines with a decreasing number of intercept points
#' measured at a survey location. The decline is approximately linear from 200 to 80
#' intercept points, and then falls more steeply after a point of inflection at 80 intercept
#' points. For this figure, species richness is shown as a percent of the total for each grid
#' point to allow a comparison of all grid points on one figure.

## Figure showing percent of total richness at all grid points
#+ species_rarefaction_pct
ggplot(spe_pred %>%
         drop_na(),
       aes(x = sample_points, y = pred_pct)) +
  geom_boxplot(fill = "gray90", outlier.color = "gray20") +
  labs(x = "point intercepts (n)", y = "species (pct of total)") +
  theme_bgl

#' At 100 point intercepts, predicted median species richness drops 18.3% (table)
spe_pred %>%
  group_by(sample_points) %>%
  summarize(
    median_pct_of_total = median(pred_pct, na.rm = TRUE) %>% round(., 1),
    .groups = "drop"
  ) %>%
  rename(point_intercepts = sample_points) %>%
  kable(format = "pandoc")

#' With the data filtered to only uncultivated grassland habitat, the pattern is similar, so it isn't shown here.
#' The decrease in species richness with rarefaction to fewer point intercepts is similar in trend and magnitude.

#' ### Contribution of quadrats to point-intercept data
#'
#' The quadrat survey can potentially add more species than were missed by point-intercept methods at
#' rarefaction to 100 point intercepts.
#' With some caveats (data from 2017 instead of 2016, surveys occurred on different days-of-the-year, etc.),
#' it appears that adding four 1x1 meter frames to a point-intercept survey would likely add six to possibly
#' a dozen species.

# Rarefaction of quadrat data
#+ species_rarefaction_pct_yvp
ggplot(qspe_pred, aes(x = quads, y = pred)) +
  geom_boxplot(fill = "gray90", outlier.color = "gray20") +
  labs(x = "quadrats (n)", y = "species (n)") +
  theme_bgl

#' In real numbers the consequence of this would be to essentially offset the number of species lost when
#' reducing the number of point intercepts from 200 to 100. The median species richness at 200 point intercepts
#' is 20, and at 100 point intercepts, rarefied species richness is 16. If four quadrats can add four or more species
#' to each survey location, richness detected will be maintained. Other potential advantages may exist wtih
#' the addition of quadrats to the survey protocol, and these will be discussed later.

spe_pred %>% filter(sample_points %in% c(200, 100)) %>%
  group_by(sample_points) %>%
  summarize(med_pred = median(pred, na.rm = TRUE))


#' # Ground cover
#'
#' ## Example and motivation
#'
#' Percent cover of vegetative and abiotic ground cover is a primary response variable desired from
#' the point-intercept survey. To explore the efficiency of our methods, an easy first step is
#' to produce a moving average of ground cover in common classes within a few grid points. Here, eleven
#' points were arbitrarily chosen and moving averages of ground cover calculated.

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
  rename(ground_code = intercept_ground_code)

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
)

#+ grcov_cummean_example
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
  labs(x = "point intercepts", y = "percent cover") +
  theme_bgl

#' Cumulative averages in these ground cover classes flatten quickly, reaching a narrow and
#' stable range after about 50 intercept points are measured. Does this mean that we could
#' obtain satisfactory data with fewer intercept points?


#' ## Bootstrapped means and CIs of ground cover
#'
#' A resampling approach is used to create bootstrapped means and confidence intervals
#' of percent ground cover.
#'
#' ### Data wrangling and analysis
#'
#' The ground cover data are split into a list with one list element for each grid point.
#' Then, the grid point data are resampled to a selected number of intercepts in a
#' function, using `lapply()`.

grcov_list <-
  split(grcov_pull_df %>% select(-transect_point),
        factor(grcov_pull_df$grid_point))

grcov_boot <- function(pts, B) {
  lapply(grcov_list, function(x, pts, B) {
    slice_sample(x, n = pts * B, replace = TRUE)
  }, pts = pts, B = B) %>%
    bind_rows() %>%
    mutate(detected = 1, boot_run = rep(rep(1:B, each = pts), length(grcov_list))) %>%
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
    grcov_boot(40, B),
    grcov_boot(80, B),
    grcov_boot(100, B),
    grcov_boot(120, B),
    grcov_boot(160, B),
    grcov_boot(200, B)
  )

#' ### Results
#'
#' Similar to the species richness rarefaction, an inflection point appears at 80 point intercepts. Above
#' that, the confidence interval decreases in a linear fashion to its value at 200 point intercepts.
#' With fewer than 80 point intercepts, the confidence interval increases rapidly.

grcov_boot_summary <-
  grcov_boot_df %>%
  filter(intercept_ground_code %in% c("BG", "BV", "G", "L", "LIC", "M", "R", "S", "WDS")) %>%
  group_by(intercept_ground_code, sampled_n) %>%
  summarize(
    boot_mean = mean(boot_pct_mean),
    boot_ci = max(boot_pct_se) * qnorm(0.975),
    ci_lwr = case_when(boot_mean - boot_ci > 0 ~ boot_mean - boot_ci, TRUE ~ 0),
    .groups = "drop"
  )

#+ grcov_boot_summary
ggplot(grcov_boot_summary,
       aes(x = sampled_n, y = boot_mean, group = intercept_ground_code)) +
  geom_ribbon(
    aes(ymin = ci_lwr, ymax = boot_mean + boot_ci),
    fill = "gray90",
    color = "gray20",
    size = 0.4
  ) +
  geom_line(aes(y = boot_mean), color = "gray20", size = 0.8) +
  labs(x = "point intercepts (n)", y = "percent cover") +
  facet_wrap(vars(intercept_ground_code), scales = "free_y") +
  theme_bgl

#' Confidence intervals range from about about 6 to 7.5 with 200 point intercepts used in the
#' bootstrap resample. The numbers can change a little if the resampling is repeated, but the
#' magnitude and range won't change much. With resampling down to 100 point intercepts, the
#' confidence interval increases by ~30-50 percent.

grcov_boot_summary %>%
  mutate(boot_ci = round(boot_ci, 2)) %>%
  select(-boot_mean,-ci_lwr) %>%
  pivot_wider(names_from = sampled_n,
              values_from = boot_ci,
              names_prefix = "ci_samp_") %>%
  select(intercept_ground_code, ci_samp_200, ci_samp_100) %>%
  mutate(pct_change = (((
    ci_samp_100 - ci_samp_200
  ) / ci_samp_200)) %>% round(., 2)) %>%
  kable(format = "pandoc")


#' # Vegetation height
#'
#' ## Example and motivation
#'
#' Cumulative average height also flattens quickly in the raw data, but evidence of spatial
#' structure also exists, with some cumulative averages trending up or (less often) down
#' as values are added. Much of the volatility in these averages dissipates after about
#' 50 point intercepts, again prompting questions about whether quality data could be obtained
#' with fewer point intercepts measured.

#+ height_cummean_example
ht_df %>%
  filter(grid_point %in% c(12, 20, 22, 180, 181, 184, 202, 203, 205, 212, 246)) %>%
  drop_na() %>%
  group_by(grid_point) %>%
  summarize(
    ht_cumean = cummean(height_intercept_1),
    pt = seq(1, n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = pt, y = ht_cumean, group = grid_point)) +
  geom_step(size = 0.2) +
  labs(x = "point intercepts", y = "height (in)") +
  theme_bgl

#' ## Data Wrangling
#'
#' #' Height measurement contains many NA values. These occur when no vegetation is detected at a point
#' intercept. How these NA values are treated depends on the information needed from these data.
#' If the desired output concerns change in height of existing vegetation, then removing NA values
#' might make sense so that the average of height in sparse vegetation isn't dragged down by many zero values
#' But, if an average height of the vegetation surface is needed, changing the NAs to zeros probably makes sense
#' Here, the NAs are treated as missing because it is assumed that this will be the dominant use case.

ht_list <-
  split(ht_df %>% select(-transect_point), factor(ht_df$grid_point))

ht_boot <- function(pts, B) {
  lapply(ht_list, function(x, pts, B) {
    slice_sample(x, n = pts * B, replace = TRUE)
  }, pts = pts, B = B) %>%
    bind_rows() %>%
    mutate(boot_run = rep(rep(1:B, each = pts), length(ht_list))) %>%
    group_by(grid_point, boot_run) %>%
    summarize(ht_mean = mean(height_intercept_1, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(grid_point) %>%
    summarize(
      ht_boot_mean = mean(ht_mean, na.rm = TRUE),
      ht_boot_se = sd(ht_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(sampled_n = factor(pts))
}

ht_boot_df <-
  bind_rows(
    ht_boot(40, B),
    ht_boot(80, B),
    ht_boot(100, B),
    ht_boot(120, B),
    ht_boot(160, B),
    ht_boot(200, B)
  )

#' ## Results
#'
#' Again an inflection point appears at 80 point intercepts and the CI increases by ~40%.
#' The consistency in inflection point and change in CIs among different data types is
#' partially reflective of the methods used. In the bootstrap resampling, the same number
#' of values are generated across all of these measurements, so part of what is being tested
#' is the strength of large numbers of data points generally.

ht_boot_summary <-
  ht_boot_df %>%
  group_by(sampled_n) %>%
  summarize(
    ht_boot_mean = mean(ht_boot_mean, na.rm = TRUE),
    ht_boot_ci = max(ht_boot_se, na.rm = TRUE) * qnorm(0.975),
    .groups = "drop"
  )

#+ height_boot_summary
ggplot(ht_boot_summary, aes(x = sampled_n, y = ht_boot_mean, group = 1)) +
  geom_ribbon(
    aes(ymin = ht_boot_mean - ht_boot_ci, ymax = ht_boot_mean + ht_boot_ci),
    fill = "gray90",
    color = "gray20",
    size = 0.4
  ) +
  geom_line(aes(y = ht_boot_mean), color = "gray20", size = 0.8) +
  labs(x = "point intercepts (n)", y = "height (in)") +
  theme_bgl

ht_boot_summary %>%
  mutate(ht_boot_ci = round(ht_boot_ci, 2)) %>%
  select(-ht_boot_mean) %>%
  pivot_wider(names_from = sampled_n,
              values_from = ht_boot_ci,
              names_prefix = "ci_samp_") %>%
  select(ci_samp_200, ci_samp_100) %>%
  mutate(pct_change = (((
    ci_samp_100 - ci_samp_200
  ) / ci_samp_200)) %>% round(., 2)) %>%
  kable(format = "pandoc")


#' # Vegetation cover in functional groups
#' 
#' ## Data wrangling
#' 
#' As before, the cumulative means of cover flattened before 200 point intercepts were reached (not shown). 
#' Missing (`NA`) values are difficult to deal with in the point-intercept data. With four possible
#' hits per point intercept, it would make sense to have 800 possible samples, and at any point 
#' hit where vegetation is not detected, it would be coded to "NV" for no vegetation. That results in 
#' cover values for vegetation that are simply too low to compare with other survey data. Here, the 
#' "NV" codes for hit 1 are kept as placeholders for resampling, but the NULL values are discarded. 

## Correct error with spotted knapweed so that cover for nonnative
## perennial forbs is accurate
spe_meta_df[spe_meta_df$key_plant_code == "CENSTO", 4] <- "perennial"

fg_df <- spe_df %>%
  left_join(spe_meta_df, by = "key_plant_species") %>%
  select(grid_point,
         key_plant_code,
         plant_native_status,
         plant_life_cycle,
         plant_life_form) %>% 
  drop_na()

fg_list <- split(fg_df, factor(fg_df$grid_point))

fg_boot <- function(pts, B) {
  lapply(fg_list, function(x, pts, B) {
    slice_sample(x, n = pts * B, replace = TRUE)
  }, pts = pts, B = B) %>%
    bind_rows() %>%
    mutate(detected = 1,
           boot_run = rep(rep(1:B, each = pts), length(fg_list))) %>%
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

fg_boot_df <-
  bind_rows(
    fg_boot(40, B),
    fg_boot(80, B),
    fg_boot(100, B),
    fg_boot(120, B),
    fg_boot(160, B),
    fg_boot(200, B)
  )

#' ## Results
#' 
#' The CIs show more volatiliy here than was obvious with height or ground cover data,
#' but the inflection at 80 point intercepts is still obvious, and the CIs again increased ~30-40%
#' when downsampling from 200 to 100 point intercepts. In functional groups with very low
#' cover across the ranch, we see different shapes of the bootstrapped means and CIs
#' (with native annual grasses, for example).

fg_boot_summary <-
  fg_boot_df %>%
  filter(
    plant_native_status %in% c("native", "nonnative"),
    plant_life_cycle %in% c("annual", "perennial"),
    plant_life_form %in% c("graminoid", "forb", "shrub")
  ) %>%
  group_by(sampled_n,
           plant_native_status,
           plant_life_cycle,
           plant_life_form) %>%
  summarize(
    boot_mean = mean(boot_pct_mean),
    boot_ci = max(boot_pct_se) * qnorm(0.975),
    ci_lwr = case_when(boot_mean - boot_ci > 0 ~ boot_mean - boot_ci, TRUE ~ 0),
    .groups = "drop"
  )

#+ pfg_boot_summary
ggplot(fg_boot_summary,
       aes(x = sampled_n, y = boot_mean, group = plant_native_status)) +
  geom_ribbon(
    aes(
      ymin = ci_lwr,
      ymax = boot_mean + boot_ci,
      linetype = plant_native_status
    ),
    fill = "gray80",
    color = "gray20",
    size = 0.4,
    alpha = 0.2
  ) +
  geom_line(aes(y = boot_mean, linetype = plant_native_status),
            size = 0.8,
            color = "gray20") +
  labs(x = "point intercepts (n)", y = "percent cover") +
  facet_grid(
    rows = vars(plant_life_cycle),
    cols = vars(plant_life_form),
    scales = "free_y"
  ) +
  scale_linetype(name = "") +
  theme_bgl

fg_boot_summary %>%
  mutate(boot_ci = round(boot_ci, 2)) %>%
  select(-boot_mean,-ci_lwr) %>%
  pivot_wider(names_from = sampled_n,
              values_from = boot_ci,
              names_prefix = "ci_samp_") %>%
  select(plant_native_status,
         plant_life_cycle,
         plant_life_form,
         ci_samp_200,
         ci_samp_100) %>%
  mutate(pct_change = (((
    ci_samp_100 - ci_samp_200
  ) / ci_samp_200)) %>% round(., 2)) %>%
  kable(format = "pandoc")
