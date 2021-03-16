



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



low_grass_pts <- c(199,201,202,203,78,72,73,55,63,19,20,9,21,22,5)
resto_pts <- c(86,87,107,108,109,119,120,135,136,166,147,570,571,45,53)
divers_pts <- c(89,99,122,123,124,138,139,149,192,193,194,74,79,80,81)

view(gp_meta_df %>% filter(grid_point %in% c(low_grass_pts, resto_pts, divers_pts)))

