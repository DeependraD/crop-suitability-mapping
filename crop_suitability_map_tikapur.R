require(tidyverse)
require(sf)
require(ggspatial)
require(ggsvg)
# require(terra)
require(ggthemes)
require(cowplot)
require(ggrepel)
require(magick)
require(ggimage)

tikapur_poly <- sf::st_read("./tikapur_map/tikapur_map.shp")
tikapur_agriculture_only <- sf::st_read("./tikapur_map/tikapur_multipolygon_agriculturable_only.shp")
ward_poly <- sf::st_read("./tikapur_map/tikapur_wards.shp")
rect_grid <- sf::st_read("./tikapur_rectangular_grid_overlay/tikapur_rectangle_grid_automated.shp")
# rect_grid_crop_suit <- sf::st_read("./tikapur_rectangular_grid_overlay/tikapur_rectangle_grid_crop_suitability_combined.shp")
inter_bound <- sf::st_read("./tikapur_map/tikapur_international_boundary.shp")
province_bound <- sf::st_read("./tikapur_map/tikapur_province_boundary.shp")

plot(st_geometry(rect_grid))

rect_grid %>% st_crs()

grid_attribute <- read_csv("./grid_attribute_data/crop_suitability_combined.csv") %>% 
  mutate(drow = (row+2) %/% 3,
         dcol = (column+2) %/% 3)

grid_attribute <- grid_attribute %>% 
  group_by(drow, dcol) %>% 
  mutate(row = mean(row),
         column = mean(column)) %>% 
  ungroup() %>% 
  group_by(row, column) %>% 
  summarise(common_crops = paste(na.omit(common_crops), collapse = ","), .groups = "keep") %>% 
  separate_rows(common_crops, sep = ",") %>% 
  mutate(common_crops = str_squish(common_crops)) %>% 
  filter(!common_crops == "") %>% 
  count(common_crops) %>% 
  filter(n>1) %>%
  arrange(-n) %>% 
  summarise(common_crops = paste(common_crops, collapse = ", "),
            number_occurances = paste(n, collapse = ", ")) %>% 
  left_join(
    expand_grid(column = 1:47, row = 1:35) %>% 
      mutate(id = row_number()) 
  )

rect_grid_crop_suit <- left_join(rect_grid, grid_attribute)

# tikapur_osm_polys %>% 
#   as_tibble() %>% 
#   select(-geometry) %>% 
#   skimr::skim()
# 
# tikapur_osm_polys %>% 
#   as_tibble() %>% 
#   select(-geometry) %>% 
#   pivot_longer(cols = everything(), names_to = "variable", values_to = "specialized_variable") %>% 
#   group_by(variable, specialized_variable) %>% 
#   count() %>%
#   filter(str_detect(specialized_variable, "forest"))


# landuse = c('forest',
#   'cemetery',  
#   'garages', 
#   'industrial',
#   'military',
#   'recreation_ground',
#   'religious',
#   'acquaculture',
#   'residential')
# aeroway  =  c('aerodrome')
# building = c("college", "commercial", "house", "public", "residential", "school", "warehouse", "yes")


# "landuse" = 'forest' OR
# "landuse" = 'cemetery' OR
# "landuse" = 'garages' OR
# "landuse" = 'industrial' OR
# "landuse" = 'military' OR
# "landuse" = 'recreation_ground' OR
# "landuse" = 'religious' OR
# "landuse" = 'acquaculture' OR
# "landuse" = 'residential' OR
# "building" = 'college' OR
# "building" = 'commercial' OR
# "building" = 'house' OR
# "building" = 'public' OR
# "building" = 'residential' OR
# "building" = 'school' OR
# "building" = 'warehouse' OR
# "building" = 'yes' OR
# "amenity" = 'school' OR
# "amenity" = 'driving_school' OR
# "amenity" = 'hospital' OR
# "amenity" = 'place_of_worship' OR
# "amenity" = 'toilets' OR
# "aeroway" = 'aerodrome' OR
# "name"  =  'Bangaun Community Forest' OR
# "name" = 'Khaireni Mahila Community Forest' OR
# "name" = 'CNRM, Tikapur Fish pond' OR
# "other_tags" =  '"water"=>"lake"' OR
# "other_tags" =  '"water"=>"pond"' OR
# "other_tags" =  '"water"=>"river"' OR
# "other_tags" =  '"healthcare"=>"hospital"' 

# tikapur_student_tracks_crop_centroids_intersect <- map(st_geometry(tikapur_student_tracks_crop_intersect), ~ st_centroid(.x)) %>%
#   st_sfc() %>%
#   st_set_crs("epsg:4326") # CRS of each of the student tracks is "epsg:4326"

bbox_tikapur <- st_bbox(tikapur_poly) + c(-0.0075, -0.001, 0.0075, 0.0075)

tikapur_grid_crop_trim <- rect_grid_crop_suit %>%
  st_intersection(tikapur_poly %>% st_transform("epsg:4326")) %>% 
  st_intersection(tikapur_agriculture_only %>% st_transform("epsg:4326"))

tikapur_grid_centroids <- tikapur_grid_crop_trim %>%
  mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]]))

tikapur_ward_centroids <- ward_poly %>%
  mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]]))

### import svgs
# require(ggsvg)
# 
# banana_svg <- paste(read_lines("./crops/banana.svg"), collapse = "\n")
# cabbage_svg <- paste(read_lines("./crops/cabbage.svg"), collapse = "\n")
# cauliflower_svg <- paste(read_lines("./crops/cauliflower2.svg"), collapse = "\n")
# lentil_svg <- paste(read_lines("./crops/lentil.svg"), collapse = "\n")
# maize_svg <- paste(read_lines("./crops/maize.svg"), collapse = "\n")
# onion_svg <- paste(read_lines("./crops/onion.svg"), collapse = "\n")
# pea_svg <- paste(read_lines("./crops/pea.svg"), collapse = "\n")
# pigeonpea_svg <- paste(read_lines("./crops/pigeonpea.svg"), collapse = "\n")
# potato_svg <- paste(read_lines("./crops/potato.svg"), collapse = "\n")
# soybean_svg <- paste(read_lines("./crops/soybean.svg"), collapse = "\n")
# wheat_svg <- paste(read_lines("./crops/wheat.svg"), collapse = "\n")

# rsvg::rsvg_png(svg = "./crops/banana.svg", file = "./crops/banana.png")

# ## convert all svgs to png
# walk(list.files("./crops/", full.names = TRUE, pattern = ".svg$"),
#     ~rsvg::rsvg_png(.x, file = paste0(tools::file_path_sans_ext(.x), ".png"),
#                     width = 400))

grid_svgs_count_points <- tikapur_grid_centroids %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(id, lat, lon, row, column, common_crops, number_occurances) %>% 
  filter(!is.na(common_crops)) %>% 
  mutate(number_crops = str_count(number_occurances, "\\,")+1) 

# grid_points <- grid_svgs_count_points$number_crops
# lat_new <- as_vector(flatten(map(grid_points, ~rnorm(.x, mean = grid_svgs_count_points$lat, sd = 0.00002))))
# lon_new <- as_vector(flatten(map(grid_points, ~rnorm(.x, mean = grid_svgs_count_points$lon, sd = 0.00002))))

grid_svgs_collate <- grid_svgs_count_points %>% 
  # select(-number_crops) %>% 
  separate_rows(c(common_crops, number_occurances), sep = ", ") %>% 
  mutate(number_occurances = as.numeric(number_occurances)) %>% 
  mutate(common_crops_file = ifelse(is.na(common_crops), NA, paste0("./crops/", common_crops, ".png"))) %>% 
  mutate(lat_new = lat+rnorm(282, 0, 0.00082),
         lon_new = lon+rnorm(282, 0, 0.00082)) %>% 
  group_by(row, column) %>% 
  mutate(lat_new = lat_new + seq(-0.003, 0.003, length.out = n()),
         lon_new = lon_new + seq(-0.0032, 0.0032, length.out = n()))

stat_spatial_identity <- function(
    mapping = NULL, data = NULL, crs = NULL, geom = "point",
    position = "identity", ..., show.legend = NA, inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = ggspatial:::StatSpatialIdentity,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(na.rm = FALSE, crs = crs, ...)
  )
}

transparent <- function(img) {
  magick::image_fx(img, expression = "0.82*a", channel = "alpha")
}

geom_spatial_image <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  ggimage::geom_image(mapping = mapping, image_fun = transparent, data = data, stat = ggspatial:::StatSpatialIdentity, crs = crs, ...)
}


tikapur_cropmap <- ggplot() +
  # geom_sf(data = st_geometry(tikapur_grid_crop_trim), lwd = 0.2, color = "black", linetype = "solid") +
  geom_sf(data = st_geometry(tikapur_poly), lwd = 0.4, color = "grey0") +
  geom_sf(data = st_geometry(ward_poly), lwd = 0.2, color = "grey40") +
  geom_sf(data = st_geometry(province_bound), lwd = 0.4, color = "blue", linetype = "dotted") +
  geom_sf(data = st_geometry(inter_bound), lwd = 0.4, color = "red", linetype = "longdash") +
  stat_spatial_identity(data = tikapur_ward_centroids, aes(x = lon, y = lat, label = ward_id), size = 4, geom = "text_repel") +
  # stat_spatial_identity(data = tikapur_grid_centroids, aes(x = lon, y = lat, label = common_crops),
  #                       size = 1.4, max.overlaps = 30,  nudge_x = 0.0015, force = 1.3, 
  #                       # segment.angle = 180, segment.ncp = 2, segment.shape = 0.3,
  #                       geom = "text_repel") + # could use geom = "text" instead, too.
  # coord_sf(xlim = bbox_tikapur[c(1,3)], ylim = bbox_tikapur[c(2,4)]) +
  annotation_scale(location = "bl") +
  geom_spatial_image(data = grid_svgs_collate,
                     aes(x = lon_new, y = lat_new, image = common_crops_file),
                     size = log((grid_svgs_collate$number_occurances+10)-grid_svgs_collate$number_crops, base = 10)/40) +
  guides(fill="none") + # remove legend
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.55, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab(NULL) +
  ylab(NULL) +
  ggthemes::theme_map() +
  theme(rect = element_blank())

ggsave("crop_suitability_map_tikapur.pdf", plot = tikapur_cropmap,
       device = "pdf", dpi = 300, width = 12, height = 14, units = "in")

pdf_convert("crop_suitability_map_tikapur.pdf", format = "jpg",
            filenames = "crop_suitability_map_tikapur.jpg", dpi = 400)



# Plotting without entire of the tikapur
without_whole_tikapur <- ggplot(st_geometry(tikapur_vector)) +
  # geom_sf(data = st_geometry(tikapur_admin_crop_intersect), aes(fill = tikapur_admin_crop_intersect$crop_name), lwd = 0.3, color = "black") + 
  geom_sf(color = "darkblue", lwd = 0.35) +
  ggrepel::geom_text_repel(data = (tikapur_admin_crop_intersect %>%
                                     mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                            lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]]))), 
                           aes(x=lon, y=lat, label=crop_name), size = 3, max.overlaps = 50, nudge_x = 0.002, force = 2.0) +
  coord_sf(xlim = bbox_tikapur_crop_map[c(1,3)], ylim = bbox_tikapur_crop_map[c(2,4)]) +
  annotation_scale(location = "bl") +
  guides(fill="none") + # remove legend
  xlab(NULL) +
  ylab(NULL) +
  ggthemes::theme_map() +
  theme(rect = element_blank())

# cowplot::ggdraw(without_whole_tikapur) +
#   draw_plot(tikapur_vector, width = 0.26, height = 0.26 * 8/12, 
#             x = 0.65, y = 0.05) +
#   annotation_scale(location = "bl", width_hint = 0.5) +
#   annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.75, "in"),
#                          pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank())

plot_grid(with_whole_tikapur, NULL, without_whole_tikapur,
          labels = NULL, rel_widths = c(1,-0.04,3), nrow = 1) +
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.55, "in"),
                         style = north_arrow_fancy_orienteering)
