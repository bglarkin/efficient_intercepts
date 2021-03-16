



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

library(car)

low_grass_pts <- c(199,201,202,203,78,72,73,55,63,19,20,9,21,22,5)
resto_pts <- c(86,87,107,108,109,119,120,135,136,166,147,570,571,45,53)
divers_pts <- c(89,99,122,123,124,138,139,149,192,193,194,74,79,80,81)

view(gp_meta_df %>% filter(grid_point %in% c(low_grass_pts, resto_pts, divers_pts)))


fg_df %>% glimpse()


# Next steps
# rarefy to a balanced design
# use anova and lsmeans or aov and post hoc



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
  group_by(grid_point, pt_int, plant_native_status, plant_life_cycle, plant_life_form) %>% 
  count() %>% ungroup() %>% 
  left_join(gp_meta_df %>% select(grid_point, type4_indicators_history), by = "grid_point") %>% 
  mutate(habitat = recode(type4_indicators_history, 
                          `uncultivated grassland native or degraded` = "grassland",
                          `forage grass restoration` = "restored",
                          `forage grass diversification` = "diversified")) %>% 
  select(-type4_indicators_history) %>% 
  glimpse()


pfg_resto_200_df <-
  pfg_resto_df %>% 
  group_by(habitat, grid_point, plant_life_cycle, plant_life_form, plant_native_status) %>% 
  summarize(pct_cvr = sum(n) / 2) %>% ungroup() %>% 
  filter(plant_life_cycle == "perennial",
         plant_life_form %in% c("forb", "graminoid"),
         plant_native_status %in% c("native", "nonnative")
         ) %>% 
  select(-plant_life_cycle) %>% 
  complete(plant_life_form, plant_native_status, nesting(habitat, grid_point), fill = list(pct_cvr = 0)) %>% 
  glimpse()
  
ggplot(pfg_resto_200_df, aes(x = habitat, y = pct_cvr)) +
  geom_boxplot() +
  facet_grid(rows = vars(plant_life_form), cols = vars(plant_native_status), scales = "free_y")
# evidence of three way interaction, where nonnative graminoids perform differently in grasslands

aov_200 <- Anova(lm(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_resto_200_df), type = "II")

TukeyHSD(aov_200)




pfg_resto_100_df <-
  pfg_resto_df %>% 
  filter(pt_int %% 2 == 0) %>% 
  group_by(habitat, grid_point, plant_life_cycle, plant_life_form, plant_native_status) %>% 
  summarize(pct_cvr = sum(n) / 2) %>% ungroup() %>% 
  filter(plant_life_cycle == "perennial",
         plant_life_form %in% c("forb", "graminoid"),
         plant_native_status %in% c("native", "nonnative")
  ) %>% 
  select(-plant_life_cycle) %>% 
  complete(plant_life_form, plant_native_status, nesting(habitat, grid_point), fill = list(pct_cvr = 0)) %>% 
  glimpse()

ggplot(pfg_resto_100_df, aes(x = habitat, y = pct_cvr)) +
  geom_boxplot() +
  facet_grid(rows = vars(plant_life_form), cols = vars(plant_native_status), scales = "free_y")
# evidence of three way interaction, where nonnative graminoids perform differently in grasslands

aov_100 <- aov(pct_cvr ~ habitat * plant_native_status * plant_life_form, data = pfg_resto_100_df)


