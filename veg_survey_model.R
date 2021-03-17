## Quick-loading resources
packages_needed = c("colorspace")
packages_installed = packages_needed %in% rownames(installed.packages())
if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}


# Install separately to save time
if (!requireNamespace("devtools"))
  install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)
# mapping
library("ggmap")

register_google(key = fromJSON(file = paste0(getwd(), "/R_globalKeys.json"))$mapKey)


mpgr_map <-
  ggmap(get_googlemap(
    center = c(lon = -114.008, lat = 46.700006),
    zoom = 13,
    scale = 2,
    maptype = 'terrain'
  ))






low_grass_pts <- c(22, 72, 63, 202, 21, 199, 19, 89, 201, 203, 55)
resto_pts <- c(571, 107, 86, 109, 87, 570, 119, 45, 108, 135, 53)
divers_pts <- c(81, 149, 80, 194, 139, 124, 79, 138, 74, 193, 99)



# Assigning integers to transect points will allow faster filtering later
trans_pts <-
  data.frame(
    direction = c(rep("E", 50), rep("S", 50), rep("W", 50), rep("N", 50)),
    number = rep(1:50, 4),
    pt_int = 1:200
  ) %>%
  mutate(transect_point = paste0(direction, number))


# Data frame `pfg_resto_df` includes 45 points in native and restored grassland to use for a model
# comparing cover in plant functional groups.
# Dplyr function `mutate` doesn't work properly if library `car` is loaded
pfg_resto_df <-
  spe_df %>%
  drop_na() %>%
  filter(grid_point %in% c(low_grass_pts, resto_pts, divers_pts)) %>%
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





# Need vars to control plotting
map_data <-
  pfg_resto_df %>%
  distinct(grid_point, habitat) %>%
  left_join(gp_meta_df %>% select(grid_point, lat, long), by = "grid_point")

# Plot of selected year
mpgr_map +
  geom_point(
    data = map_data,
    aes(x = long, y = lat, fill = habitat),
    size = 5,
    shape = 21,
    alpha = 0.7
  ) +
  scale_fill_discrete_sequential(name = "grassland type", palette = "terrain") +
  theme_bgl














# Function to downsample the point intercept data
# d = divisor to systematically eliminate point intercepts
# pts = number of point intercepts desired
downsample <- function(d, pts) {
  pfg_resto_df %>%
    filter(pt_int %% d == 0)# %>%
  # group_by(habitat, grid_point, plant_life_cycle, plant_life_form, plant_native_status) %>%
  # summarize(pct_cvr = sum(n) / pts * 100, .groups = "drop") %>% ungroup() %>%
  # filter(plant_life_cycle == "perennial",
  #        plant_life_form %in% c("forb", "graminoid"),
  #        plant_native_status %in% c("native", "nonnative")
  # ) %>%
  # select(-plant_life_cycle) %>%
  # complete(plant_life_form, plant_native_status, nesting(habitat, grid_point), fill = list(pct_cvr = 0))
}

pfg_200 <- downsample(1, 200)
pfg_100 <- downsample(2, 100)
pfg_40 <- downsample(5, 40)

# Visuals and models
aov_200 <-
  aov(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_200)
summary(aov_200)
ggplot(pfg_200, aes(x = habitat, y = pct_cvr)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(plant_life_form),
    cols = vars(plant_native_status),
    scales = "free_y"
  )

aov_100 <-
  aov(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_100)
summary(aov_100)
ggplot(pfg_100, aes(x = habitat, y = pct_cvr)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(plant_life_form),
    cols = vars(plant_native_status),
    scales = "free_y"
  )

aov_40 <-
  aov(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_40)
summary(aov_40)
ggplot(pfg_40, aes(x = habitat, y = pct_cvr)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(plant_life_form),
    cols = vars(plant_native_status),
    scales = "free_y"
  )



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
