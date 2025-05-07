
# Graphics showing the process of the first iteration (breaking Ohio into two equal populations)

library(ggpattern)
library(ggplot2)
library(colorspace)
library(tidyverse)

# load data
example_tdf <- read_csv("example_tdf.csv") %>%
  dplyr::mutate(Geography = as.character(Geography))


# STEP 1: CHOOSE TWO RANDOM DISTRICTS --------------------------------------------------------------
tract1 <- 39023000300
tract2 <- 39093060100


color_palette <- c("dodgerblue3", "firebrick2", "lightgray")

step1_map <- shape_tract_v2 %>%
  dplyr::mutate(col = dplyr::case_when(
    Geography == 39023000300 ~ 1,
    Geography == 39093060100 ~ 2,
    .default = 3
  )) %>%
  dplyr::mutate(col = factor(col, levels = c(1:3)))

map1 <- ggplot2::ggplot(data = step1_map,
                       ggplot2::aes(fill = col)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_manual(values = color_palette) +
  ggplot2::theme_void() +
  labs(title = "STEP 2: Select Two Random Census Tracts")

map1

ggplot2::ggsave("methodology_map_step1.png",
                dpi = 300)

# mapview(step1_map, zcol = "col")

# STEP 2: DISTANCE FROM TRACT1/TRACT2 and DISTFROM -------------------------------------------------
step2_map <- shape_tract_v2 %>%
  dplyr::left_join(example_tdf, by = "Geography") %>%
  dplyr::rename(dist_r = distfrom)

map2_1 <- ggplot2::ggplot(data = step2_map,
                       ggplot2::aes(fill = dist1)) +
  ggplot2::geom_sf() +
  # ggplot2::scale_fill_manual(values = color_palette) +
  ggplot2::theme_void()

map2_1

map2_2 <- ggplot2::ggplot(data = step2_map,
                          ggplot2::aes(fill = dist2)) +
  ggplot2::geom_sf() +
  # ggplot2::scale_fill_manual(values = color_palette) +
  ggplot2::theme_void()

map2_2

map2_3 <- ggplot2::ggplot(data = step2_map,
                          ggplot2::aes(fill = dist_r)) +
  ggplot2::geom_sf() +
  colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0) + 
  # ggplot2::scale_fill_manual(values = color_palette) +
  ggplot2::theme_void() +
  labs(title = "STEP 4: Calculate Relative Distance Dist_r",
       caption = "Note: Enclave census tracts are displayed as gray.")

map2_3

ggplot2::ggsave("methodology_map_step2_3.png",
                dpi = 300)


# STEP 3: IDENTIFY STARTING POINTS ----------------------------------------------------
step3_map <- shape_tract_v2 %>%
  dplyr::left_join(example_tdf, by = "Geography") %>%
  dplyr::rename(dist_r = distfrom) %>%
  dplyr::mutate(col = dplyr::case_when(
    dist_r == min(dist_r, na.rm = TRUE) ~ 1,
    dist_r == max(dist_r, na.rm = TRUE) ~ 2,
    .default = 3
  )) %>%
  dplyr::mutate(col = factor(col, levels = c(1:3)))

map3 <- ggplot2::ggplot(data = step3_map,
                          ggplot2::aes(fill = col)) +
  ggplot2::geom_sf() +
  # colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0) + 
  ggplot2::scale_fill_manual(values = color_palette) +
  ggplot2::theme_void() +
  labs(title = "STEP 5: Calculate Contiguous District Starting Points") +
  ggplot2::theme(legend.position="none")

map3

ggplot2::ggsave("methodology_map_step3.png",
                dpi = 300)

# STEP 4: FIRST EXPANSION --------------------------------------------------------------------------

adj_dist1 <- c(39093057100,39093060200,39093077100,39093093100,39093070500,39093070600,39093070700,
               39093071100,39093070400,39093070902,39093071300)
  
starting_dist1 <- c(39093060100,39093071500,39093070901,39093071400,39093071000,39093070800)
  
adj_dist2 <- c(39023000200,39023000400,39023001200,39023001300,39023003400)

tdf_adj_dist1 <- example_tdf %>%
  dplyr::filter(Geography %in% adj_dist1)

tdf_adj_dist2 <- example_tdf %>%
  dplyr::filter(Geography %in% adj_dist2)

color_palette2 <- c("dodgerblue3", "lightblue", "lightgray")
border_palette <- c("#595959", "green", "gray")

step4_map <- shape_tract_v2 %>%
  dplyr::left_join(example_tdf, by = "Geography") %>%
  dplyr::rename(dist_r = distfrom) %>%
  dplyr::mutate(
    Adjacency = dplyr::case_when(
      Geography %in% starting_dist1 ~ "Starting tracts",
      Geography %in% adj_dist1 ~ "Adjacent tracts",
      .default = "Other tracts"
      ),
    `Qualified dist_r` = dplyr::case_when(
      Geography %in% starting_dist1 | dist_r <= -16 ~ "Qualified",
      .default = "Unqualified"
    )) %>%
  dplyr::mutate(Adjacency = factor(Adjacency, levels = c("Other tracts",
                                                         "Adjacent tracts",
                                                         "Starting tracts")),
                `Qualified dist_r` = factor(`Qualified dist_r`, levels = c("Qualified",
                                                                           "Unqualified"))) %>%
  dplyr::mutate(dist_r = ifelse(dist_r > -11, -11, dist_r))

# step4_map_starting <- step4_map %>%
#   dplyr::filter(Geography %in% starting_dist1)
# step4_map_adj <- step4_map %>%
#   dplyr::filter(Geography %in% adj_dist1)
# 
# map4 <- ggplot2::ggplot() +
#   # ggplot2::coord_sf(xlim = c(41.3, -81.6), ylim = c(42.6, -81.75), clip = "off") +
#   ggplot2::geom_sf(data = step4_map, ggplot2::aes(fill = dist_r)) +
#   # coord_sf(xlim = c(-82.6, -81.85), ylim = c(41, 41.6)) +
#   # scale_fill_continuous(type = "gradient") +
#   colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = -2) +
#   ggplot2::geom_sf(data = step4_map_adj, fill = NA, color = "green") +
#   ggplot2::geom_sf(data = step4_map_starting, fill = "gray") +
#   # coord_sf(xlim = c(-82.55, -81.85), ylim = c(41.05, 41.55)) +
#   coord_sf(xlim = c(-82.45, -81.95), ylim = c(41.15, 41.45)) +
#   # ggplot2::scale_fill_manual(values = color_palette) +
#   # ggpattern::scale_pattern_manual(values = c("Starting tract" = "none", "Adjacent tracts" = "stripe")) +
#                        # guide = guide_legend(override.aes=list(fill=NA))) +
#   # coord_sf(xlim = c(41.3, -84), ylim = c(42.6, -83), expand = FALSE) +
#   ggplot2::theme_void() +
#   ggplot2::labs(title = "STEP 7: Expand the District",
#                 caption = "Note: Census tracts shaded gray are the starting tracts within\n
#                 the district. Tracts outlined in green are adjacent to at least\n
#                 one tract already assigned to the district.")
#   # ggplot2::theme(legend.position="none")
#   # st_crop(map4, c(xmin= 41.3, ymin = -85, xmax = 42.6, ymax = -80))
# #   
# # coord_map(projection = "albers", lat0 = 39, lat1 = 45,
# #           xlim = c(-117,-75), ylim = c(26,49)) +
# # 41.499167, -81.694722
# map4


map4 <- ggplot2::ggplot(data = step4_map) +
  ggplot2::geom_sf(ggplot2::aes(fill = dist_r, color = Adjacency)) +
  colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = -2) +
  # ggplot2::geom_sf(data = step4_map, ggplot2::aes(color = "Adjacency")) +
  ggplot2::scale_color_manual(values = border_palette) +
  geom_sf_pattern(aes(fill = dist_r,
                      color = Adjacency,
                      pattern = `Qualified dist_r`)) +
  scale_pattern_manual(
    values = c(
      Qualified = 'stripe',
      Unqualified = 'none'
    )) +
  # geom_density_pattern(aes(pattern_fill = as.factor(`Qualified dist_r`), pattern_type = as.factor(`Qualified dist_r`)), pattern = 'polygon_tiling', pattern_key_scale_factor = 1.2) +
  # scale_pattern_type_manual(values = c('hexagonal', 'pythagorean')) +
  # ggpattern::scale_pattern_manual(values = c("Starting tract" = "none", "Adjacent tracts" = "stripe")) +
  # ggplot2::geom_sf(data = step4_map_adj, fill = NA, color = "green") +
  # ggplot2::geom_sf(data = step4_map_starting, fill = "gray") +
  coord_sf(xlim = c(-82.425, -81.975), ylim = c(41.175, 41.4225)) +
  ggplot2::theme_void() +
  ggplot2::labs(title = "STEP 7: Expand the District",
                caption = "Note: Census tracts shaded gray are the starting tracts within\n
                the district. Tracts outlined in green are adjacent to at least\n
                one tract already assigned to the district.")

map4

ggplot2::ggsave("methodology_map_step4_test.png",
                dpi = 300)


library(usmap)


us_map(regions = 'states') |> 
  ggplot() +
  geom_sf(aes(fill = full)) +
  theme_minimal(base_size = 18, base_family = 'IBM Plex Mono') +
  theme(legend.position = 'none')

set.seed(2522)
grouped_data <- us_map() |> 
  mutate(
    group = sample(
      c('A', 'B', 'C', 'D'), 
      size = 51, 
      replace = TRUE
    )
  )

grouped_data  |> 
  ggplot() +
  geom_sf_pattern(aes(fill = group)) +
  theme_minimal(base_size = 18, base_family = 'IBM Plex Mono') +
  theme(legend.position = 'none') +
  scale_fill_manual(
    values = c(
      A = '#ef476f',
      B = '#ffd166',
      C = '#06d6a0',
      D = '#118ab2'
    )
  ) 
