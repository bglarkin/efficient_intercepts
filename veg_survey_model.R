#' ---
#' title: "Empirical test of downsampling vegetation data"
#' author: "Beau Larkin"
#' date: "2021-03-16"
#' output:
#'   github_document:
#'     toc: true
#'     toc_depth: 2
#' ---
#'
#' # Description
#' This is the final section of the vegetation methods inquiry. It is broken into a separate
#' section to speed loading and processing time. 
#' 
#' The previous report sections presented rarefaction and bootstrapping methods to 
#' assess the efficiency of our vegetation monitoring protocol. In this section, a sample of 
#' real data is tested for a significant contrast in plant cover among habitat types, 
#' and then these data are downsampled systematically to fewer numbers of vegetation samples,
#' and the tests are repeated. Ostensibly, this report will address the question, "does plant 
#' cover in functional groups vary among restored, diversified, and uncultivated (unrestored) 
#' grassland?" The question is merely a vehicle to facilitate an empirical test of downsampling
#' the vegetaiton data, and the grid points used in the test were chosen to maximize contrasts. 
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

# Calculating the 95% CI will aid plotting later
# Uses `plotrix`
ci_95 = function(x) {
  std.error(x) * qnorm(0.975)
}

#' ## Data
#' Data are loaded here, but the code is redundant with the previous notebook so it is 
#' not shown here. 

#+ spe_df, echo = FALSE
spe_pull_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_point_intercept_gridVeg.gridVeg_point_intercept_vegetation`
  WHERE year = 2016
  "
spe_pull_bq <- bq_project_query(billing, spe_pull_sql)
spe_pull_tb <- bq_table_download(spe_pull_bq)
spe_pull_df <- as.data.frame(spe_pull_tb)
spe_df <-
  spe_pull_df %>%
  select(grid_point, transect_point, starts_with("intercept")) %>%
  pivot_longer(starts_with("intercept"),
               names_to = "intercept",
               values_to = "key_plant_species")

#+ spe_meta_df, echo = FALSE
spe_meta_sql <-
  "
  SELECT key_plant_species, key_plant_code, plant_native_status, plant_life_cycle, plant_life_form, plant_name_sci, plant_name_common
  FROM `mpg-data-warehouse.vegetation_species_metadata.vegetation_species_metadata`
  "
spe_meta_bq <- bq_project_query(billing, spe_meta_sql)
spe_meta_tb <- bq_table_download(spe_meta_bq)
spe_meta_df <- as.data.frame(spe_meta_tb)

#+ gp_meta_df, echo = FALSE
gp_meta_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.grid_point_summaries.location_position_classification`
  "
gp_meta_bq <- bq_project_query(billing, gp_meta_sql)
gp_meta_tb <- bq_table_download(gp_meta_bq)
gp_meta_df <- as.data.frame(gp_meta_tb)


#' # Data wrangling
#' Eleven grid points were chosen from each of three grassland habitat types. 
#' The points were chosen from **uncultivated grassland, restored grassland,** and
#' **forage grass diversification** sites. Points were as close as possible in 
#' proximity and elevation. Vectors with grid point numbers will be used to filter
#' the vegetation cover data. 

# Grid points in each grassland habitat type
uncult_pts <- c(22, 72, 63, 202, 21, 199, 19, 89, 201, 203, 55)
resto_pts <- c(571, 107, 86, 109, 87, 570, 119, 45, 108, 135, 53)
divers_pts <- c(81, 149, 80, 194, 139, 124, 79, 138, 74, 193, 99)

# Assigning integers to transect points will allow faster downsampling later
trans_pts <-
  data.frame(
    direction = c(rep("E", 50), rep("S", 50), rep("W", 50), rep("N", 50)),
    number = rep(1:50, 4),
    pt_int = 1:200
  ) %>%
  mutate(transect_point = paste0(direction, number))

# `pfg_resto_df` is the core dataset for this exploration
pfg_resto_df <-
  spe_df %>%
  drop_na() %>%
  filter(grid_point %in% c(uncult_pts, resto_pts, divers_pts)) %>%
  left_join(spe_meta_df, by = "key_plant_species") %>%
  left_join(trans_pts, by = "transect_point") %>%
  select(grid_point,
         pt_int,
         plant_native_status,
         plant_life_cycle,
         plant_life_form) %>%
  group_by(grid_point,
           pt_int,
           plant_native_status,
           plant_life_cycle,
           plant_life_form) %>%
  count() %>% ungroup() %>%
  left_join(gp_meta_df %>% select(grid_point, type4_indicators_history),
            by = "grid_point") %>%
  mutate(
    habitat = recode(
      type4_indicators_history,
      `uncultivated grassland native or degraded` = "uncultivated",
      `forage grass restoration` = "restored",
      `forage grass diversification` = "diversified"
    )
  ) %>%
  select(-type4_indicators_history) %>%
  glimpse()


#' # Results
#' The map below shows the grid points which were used for this test.

map_data <-
  pfg_resto_df %>%
  distinct(grid_point, habitat) %>%
  left_join(gp_meta_df %>% select(grid_point, lat, long), by = "grid_point")
mpgr_map <- ggmap(get_googlemap(center = c(lon = -114.008, lat = 46.700006),
                                zoom = 13, scale = 2,
                                maptype ='terrain'))     

#+ pfg_test_sites
mpgr_map +
  geom_point(
    data = map_data,
    aes(x = long, y = lat, fill = habitat),
    size = 3,
    shape = 21,
    alpha = 0.7
  ) +
  scale_fill_discrete_sequential(name = "grassland type", palette = "viridis") +
  theme_bgl

#' Downsampling to fewer point intercepts per grid point involves similar operations
#' regardless of how many point intercepts are desired, so a function for this 
#' will save space and reduce errors.
#' 
#' * `d` = divisor to systematically eliminate point intercepts using the modulus (`%%`) function
#' * `pts` = number of point intercepts desired

downsample <- function(d, pts) {
  pfg_resto_df %>%
    filter(pt_int %% d == 0) %>%
  group_by(habitat, grid_point, plant_life_cycle, plant_life_form, plant_native_status) %>%
  summarize(pct_cvr = sum(n) / pts * 100, .groups = "drop") %>% ungroup() %>%
  filter(plant_life_cycle == "perennial",
         plant_life_form %in% c("forb", "graminoid"),
         plant_native_status %in% c("native", "nonnative")
  ) %>%
  select(-plant_life_cycle) %>%
  complete(plant_life_form, plant_native_status, nesting(habitat, grid_point), fill = list(pct_cvr = 0))
}

# Apply the `downsample()` function
pfg_200 <- downsample(1, 200)
pfg_100 <- downsample(2, 100)
pfg_40 <- downsample(5, 40)

# Fit ANOVA models with interaction of all terms
aov_200 <-
  aov(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_200)
aov_100 <-
  aov(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_100)
aov_40 <-
  aov(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_40)

#' ## Model results
#' The ANOVA fit to `aov_200` shows the result using all available data. The data are 
#' balanced and orthogonal. In the model result, all terms are 
#' highly significant, including the three-way interaction. These data violate some assumptions
#' of an ANOVA test, namely normal distributions and constant variance. These violations
#' might make it unwise to draw management conclusions from this test, but ANOVA is robust 
#' enough against violated assumptions to at least allow a comparison of model performance with 
#' differently subsetted data. 
#' 
#' A visual examination of the data
#' makes the interaction obvious. Cover of nonnative grasses is relatively low in 
#' uncultivated sites and intermediate in restored sites, but the situation is reversed
#' with the other functional group combinations. 

summary(aov_200)
post200 <- data.frame(TukeyHSD(aov_200)[[7]]) %>% filter(p.adj < 0.05) %>% rownames_to_column()
#+ pfg_200_boxplot
ggplot(pfg_200, aes(x = habitat, y = pct_cvr)) +
  geom_boxplot(fill = "gray90") +
  facet_grid(
    rows = vars(plant_life_form),
    cols = vars(plant_native_status),
    scales = "free_y"
  ) +
  labs(x = NULL, y = "percent cover") +
  theme_bgl

#' Downsampling to 100 and 40 point intercepts per grid point, we see very little change 
#' to the model and no conclusions would be altered. 

summary(aov_100)
post100 <- data.frame(TukeyHSD(aov_100)[[7]]) %>% filter(p.adj < 0.05) %>% rownames_to_column()
summary(aov_40)
post40 <- data.frame(TukeyHSD(aov_40)[[7]]) %>% filter(p.adj < 0.05) %>% rownames_to_column()

#' Post-hoc analysis with a three-way interaction term is difficult to interpret and beyond the 
#' scope of this report. Interpretation isn't necessary to show that similar performance of 
#' downsampled models carries through to pairwise contrasts. 

post <-
  bind_rows(
    "200" = post200,
    "100" = post100,
    "40" = post40,
    .id = "sampled"
  )
#+ post_hoc_table
pivot_wider(
  post[,-c(3, 4, 5)],
  values_from = p.adj,
  names_from = sampled,
  names_prefix = "pt_int_"
) %>%
  kable(format = "pandoc")

#' With the results of Tukey's HSD post-hoc test filtered to _p_ values less than 0.05 and only 
#' contrasts with three-way interactions considered, little difference is apparent between
#' data sets averaged from 200, 100, or 40 point intercepts. Between data sets with 200 or 100 point
#' intercepts, _p_ values mostly remain in the same order of magnitude. With the data set filtered to 
#' 40 point intercepts, the _p_ values increase by roughly an order of magnitude, again 
#' signaling some increased volatility in means. 

#' Graphically, it appears that the means and confidence intervals at 200 and 100 point 
#' intercepts are very similar. At 40 point intercepts, the mean shows increased volatility
#' and confidence intervals increase. 

pfg_means <-
  bind_rows(
    "40" = pfg_40,
    "100" = pfg_100,
    "200" = pfg_200,
    .id = "pt_int"
  ) %>%
  mutate(pt_int = factor(pt_int, levels = c("40", "100", "200"))) %>%
  group_by(pt_int, habitat, plant_life_form, plant_native_status) %>%
  summarize(
    mean_pct_cvr = mean(pct_cvr),
    ci_pct_cvr = ci_95(pct_cvr),
    .groups = "drop"
  )

#+ pfg_means_and_CIs
ggplot(pfg_means, aes(x = habitat, y = mean_pct_cvr, group = pt_int)) +
  geom_col(aes(fill = pt_int),
           color = "gray20",
           position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_pct_cvr, ymax = mean_pct_cvr + ci_pct_cvr),
    color = "gray20",
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  facet_grid(
    rows = vars(plant_life_form),
    cols = vars(plant_native_status),
    scales = "free_y"
  ) +
  scale_fill_manual(name = "point intercepts",
                    values = c("gray30", "gray50", "gray70")) +
  labs(x = "", y = "percent cover") +
  theme_bgl

#' # Discussion
#' The final report will contain more interpretation, but for now it is important to 
#' restate that this is a crude application of an ANOVA, and a three-way interaction would
#' be difficult to interpret under the best of circumstances. What we can show here is that 
#' the vegetation monitoring data seems to capture differences in plant functional group cover 
#' well at greatly reduced sampling frequencies. 
#' 
#' This doesn't inform what the effect of downsampling would be on analysis of multivariate 
#' plant community data. Presumably, rare species would be lost with downsampling, and the 
#' consequences of that are hard to predict. Also, even with a plan to separate means of 
#' cover in functional groups, it's unlikely that a real case would be as ideal as these test 
#' data are, and assumptions of model fit would need to be better observed. 

