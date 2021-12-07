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
#' vegetation cover changes over the course of a season, and how we can handle, reduce, 
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
packages_needed = c("tidyverse", "knitr", "rjson", "lubridate", "bigrquery", "devtools", "ggmap", "colorspace")
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

#' # Data
#' ## Survey metadata
#+ survey_metadata,echo=TRUE
meta_sql <- 
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_point_intercept_gridVeg.gridVeg_survey_metadata`
  "
meta_bq <- bq_project_query(billing, meta_sql)
meta_tb <- bq_table_download(meta_bq)
meta_df <- as.data.frame(meta_tb)

#' ## Grid point metadata
#+ gpmeta,echo=TRUE
gp_meta_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.grid_point_summaries.location_position_classification`
  "
gp_meta_bq <- bq_project_query(billing, gp_meta_sql)
gp_meta_tb <- bq_table_download(gp_meta_bq)
gp_meta_df <- as.data.frame(gp_meta_tb)

#' ## Any-hit plant cover data
#' Plant species and cover data from point-intercept surveys in 2011-12, 2016, and 2021.
#' Plant data are joined with survey metadata to filter the data to the survey periods with the 
#' greatest effort (2011-12, 2016, and 2021). Data are simplified and summarized to show sums of 
#' cover in plant functional groups at each date of annual surveys. 
#+ plant_cover_data,echo=TRUE
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
  select(survey_ID, survey_sequence, grid_point, plant_name_common, plant_native_status, plant_life_cycle, plant_life_form, intercepts_pct) %>% 
  left_join(meta_df %>% select(survey_ID, date), by = "survey_ID") %>% 
  mutate(doy = yday(date)) %>% 
  group_by(grid_point, survey_sequence, doy, plant_native_status, plant_life_cycle, plant_life_form) %>% 
  summarize(cvr_pct = sum(intercepts_pct), .groups = "drop")

#' ## Top-hit plant cover data
#+ plant_top_cover,echo=TRUE
top_sql <-
  "
  SELECT *
  FROM `mpg-data-warehouse.vegetation_gridVeg_summaries.gridVeg_foliar_cover_top`
  "
top_bq <- bq_project_query(billing, top_sql)
top_tb <- bq_table_download(top_bq)
top_df <- as.data.frame(top_tb) 
#+ plant_spe_metadata,echo=TRUE
spe_meta_sql <-
  "
  SELECT key_plant_species, key_plant_code, plant_native_status, plant_life_cycle, plant_life_form, plant_name_sci, plant_name_common
  FROM `mpg-data-warehouse.vegetation_species_metadata.vegetation_species_metadata`
  "
spe_meta_bq <- bq_project_query(billing, spe_meta_sql)
spe_meta_tb <- bq_table_download(spe_meta_bq)
spe_meta_df <- as.data.frame(spe_meta_tb)
#+ top_cover_data,echo=TRUE
top_cvr_df <- top_df %>% 
  left_join(spe_meta_df, by = "key_plant_species") %>% 
  left_join(meta_df %>% select(survey_ID, date), by = "survey_ID") %>% 
  left_join(gp_meta_df %>% select(grid_point, type3_vegetation_indicators), by = "grid_point") %>% 
  filter(survey_sequence %in% c("2011-12", "2016", "2021"), 
         plant_native_status %in% c("native", "nonnative"), 
         plant_life_cycle %in% c("annual", "perennial"), 
         plant_life_form %in% c("forb", "graminoid"),
         type3_vegetation_indicators == "uncultivated grassland native or degraded") %>% 
  mutate(doy = yday(date)) %>% 
  group_by(grid_point, survey_sequence, doy, plant_native_status, plant_life_cycle, plant_life_form) %>% 
  summarize(top_cvr_pct = sum(top_intercepts_pct), .groups = "drop")

#' I examined point-intercept cover data from 2011-12, 2016, and 2021. These were years with the most extensive 
#' survey efforts. I filtered the data to include only points in uncultivated grassland to reduce the noise
#' associated with restoration activities. The removal of uncultivated grasslands probably obscures activitiy
#' of most of the exotic forage grass plantations, however. 
#' Very little signal and generally low cover was observed with annual
#' plants (not shown), so they were filtered as well. 
#' 
#' ## Survey locations
#' The following map shows locations of points that appear at least once in this data set. The table after the map
#' details the number of points included per year. 
#+ fig_map,echo=TRUE,message=FALSE
mpgr_map <- 
  ggmap(
    get_googlemap(
      center = c(lon = -114.008, lat = 46.700006),
      zoom = 13, 
      scale = 2,
      maptype ='terrain')
  )
mpgr_map +
  geom_point(
    data = cvr_df %>% 
      select(grid_point) %>% 
      distinct() %>% 
      left_join(gp_meta_df %>% select(grid_point, lat, long), by = "grid_point"),
    aes(x = long, y = lat)
  ) +
  theme_void()
#+ table_points_years,echo=TRUE
cvr_df %>% 
  distinct(grid_point, survey_sequence) %>% 
  count(survey_sequence) %>% 
  kable(format = "pandoc")
#' 
#' # Results
#' ## Any-hit cover data
#' Plant cover can vary substantially over a season, but this depends on the group of plants and the year.
#' The 2011-12 season preceded several years of drought, and cover appears generally higher as a result, 
#' with native forbs slowly increasing and nonnative forbs strongly increasing throughout the season. Native perennial
#' grasses were abundant in 2011-12, with a unimodal peak in cover near mid-summer. Cover retracted slightly in 2016 for most 
#' groups, with native forbs declining from mid-summer on and nonnative forbs holding flat. Perennial grasses
#' in 2016 were smaller in 2016 and showed a briefer peak in abundance near the end of June. Nonnative grasses were flat and 
#' similar to 2011-12. 2021 was a shorter season, with much less change in cover over time. Cover was flat 
#' for all functional groups except nonnative perennial grasses, which declined sharply. I suspect that this was 
#' driven more by a difference in species/locations surveyed that an actual decline in cover.  
#' 
#' In any case, variation in cover among locations is much greater than variation across the season.
#' A shorter sampling season might be good for a number of reasons, but we shouldn't worry much about
#' using the data we already have. 
#+ fig_seasonal_cover,echo=TRUE
cvr_df %>% 
  filter(plant_life_cycle == "perennial") %>% 
  ggplot(., aes(x = doy, y = cvr_pct, group = survey_sequence)) +
  facet_grid(cols = vars(plant_native_status), rows = vars(plant_life_form), scales = "free_y") +
  geom_point(aes(color = as.factor(survey_sequence)), alpha = 0.8) +
  geom_smooth(aes(color = as.factor(survey_sequence)), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
  labs(x = "", y = "Any-hit percent cover of perennials", caption = "Lines produced by GAM smoother with default parameters.") +
  scale_color_discrete_qualitative(name = "year", palette = "Harmonic") +
  scale_x_continuous(breaks = c(121, 152, 182, 213, 244), labels = c("May", "Jun", "Jul", "Aug","Sep")) +
  theme_bgl
#+ fig_average_cover,echo=TRUE
cvr_df %>% 
  filter(plant_life_cycle == "perennial") %>% 
  ggplot(., aes(x = survey_sequence, y = cvr_pct)) +
  facet_grid(cols = vars(plant_native_status), rows = vars(plant_life_form), scales = "free_y") +
  geom_boxplot(fill = "gray95") +
  labs(x = "", y = "Any-hit percent cover of perennials") +
  theme_bgl

#' The previous analysis used all hits from the point-intercept data. It could be that top-cover would
#' be more responsive to seasonal change, so let's have a look at that here. 
#' 
#' ## Top-hit cover data
#' Restricting the plant data to top-hit only changes little of the overall pattern, except in most 
#' cases to flatten things out over the season. It's possible this is because the largest-statured plants
#' are most likely to be hit in top-cover, and these aren't as responsive to seasonal change. The exception
#' is with perennial native grasses. With any-hit data, the seasonal curve in 2011-12 is pronounced, but 
#' it is flat with top-hit data. Possibly 2011-12 was a wetter period and smaller grasses were expanding. 
#+ fig_seasonal_top_cover,echo=TRUE
top_cvr_df %>% 
  filter(plant_life_cycle == "perennial") %>% 
  ggplot(., aes(x = doy, y = top_cvr_pct, group = survey_sequence)) +
  facet_grid(cols = vars(plant_native_status), rows = vars(plant_life_form), scales = "free_y") +
  geom_point(aes(color = as.factor(survey_sequence)), alpha = 0.8) +
  geom_smooth(aes(color = as.factor(survey_sequence)), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
  labs(x = "", y = "Top-hit percent cover of perennials", caption = "Lines produced by GAM smoother with default parameters.") +
  scale_color_discrete_qualitative(name = "year", palette = "Harmonic") +
  scale_x_continuous(breaks = c(121, 152, 182, 213, 244), labels = c("May", "Jun", "Jul", "Aug","Sep")) +
  theme_bgl
#+ fig_top_average_cover,echo=TRUE
top_cvr_df %>% 
  filter(plant_life_cycle == "perennial") %>% 
  ggplot(., aes(x = survey_sequence, y = top_cvr_pct)) +
  facet_grid(cols = vars(plant_native_status), rows = vars(plant_life_form), scales = "free_y") +
  geom_boxplot(fill = "gray95") +
  labs(x = "", y = "Top-hit percent cover of perennials") +
  theme_bgl
