library(sf)
library(tidyverse)
library(tmap)

region = "Manchester"

#wards = st_read("../play_survey/DAT/WD_DEC_2025_UK_BFC_-2745845960906252656.gpkg", quiet = TRUE)
LAs = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/LA_boundaries/LA.gpkg",quiet = TRUE)
# buffer to use in metres
buf = 1000

geo_in = filter(LAs,LAD22NM == region)

geo_buf = geo_in |> 
  st_buffer(buf)

tmap_mode("view")

tm_shape(geo_in) +
  tm_polygons(fill_alpha = 0, lwd = 2)

msoa_nm = read.csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv")

msoa_geo = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/msoa_boundaries-v1.0/msoa.gpkg") |> 
  st_transform(27700) |> 
  left_join(msoa_nm, by = c("MSOA21CD" = "msoa21cd")) |> 
  select(msoa21hclnm,geom)

IMD_2025 = st_read("../IMD/dat/LSOA_IMD2025_WGS84_-4854136717238973930.gpkg",quiet = TRUE) |> 
  st_transform(27700)

IMD_cent = st_centroid(IMD_2025) |> 
  st_join(msoa_geo)

IMDs_in = IMD_cent[geo_in,]

IMD_poly = IMD_2025 |> 
  filter(LSOA21CD %in% IMDs_in$LSOA21CD)

IMD_poly$msoa_hcl_nm = IMDs_in$msoa21hclnm

imd_lowest = which(min(IMD_poly$IMDRank) == IMD_poly$IMDRank)

geo_buf = IMD_poly |> 
  st_union() |> 
  st_buffer(buf)

tm_shape(geo_buf)+
  tm_polygons(alpha = 0,lwd = 3)+
  tm_shape(geo_in) +
  tm_polygons(fill_alpha = 0,lwd = 2)+
  tm_shape(IMD_poly)+
  tm_polygons(fill = "IMDRank", 
              fill.scale = tm_scale_continuous(),
              lwd = 0.5)

path = "DAT/greater-manchester.gpkg"

all_play = bind_rows(
  st_read(path, layer = "gis_osm_pois_a_free", quiet = TRUE) |> 
    filter(fclass %in% c("playground", "park", "pitch", "track", "sports_centre", "sports_hall",
                         "swimming_pool", "fitness_centre", "ice_rink", "theme_park", "zoo",
                         "dog_park", "picnic_site", "community_centre", "camp_site", "arts_centre")) |>
    select(fclass, name, geom),
  
  st_read(path, layer = "gis_osm_landuse_a_free", quiet = TRUE) |> 
    filter(fclass %in% c("park", "recreation_ground", "grass", "meadow", "forest")) |>
    select(fclass, name, geom),
  
  st_read(path, layer = "gis_osm_natural_a_free", quiet = TRUE) |> 
    filter(fclass == "beach") |>
    select(fclass, name, geom),
  
  st_read(path, layer = "gis_osm_protected_areas_a_free", quiet = TRUE) |> 
    filter(fclass == "nature_reserve") |>
    transmute(fclass, name = NA, geom)
)


play_in = all_play |> 
  st_transform(27700) |> 
  st_intersection(geo_buf)

os_greenspace = st_read("DAT/opgrsp_gb.gpkg",layer = "greenspace_site", quiet = TRUE) |> 
  select(fclass = function.,name = distinctive_name_1,geom = geometry)

green_in = os_greenspace[geo_buf,]

osm_os = rbind(green_in,play_in) |> 
  group_by(fclass) |>
  summarise(geom = st_union(geom))


tm_shape(geo_buf)+
  tm_polygons(alpha = 0,lwd = 3)+
  tm_shape(osm_os) +
  tm_polygons(fill = "fclass",
              fill.scale = tm_scale_categorical())



library(stats19)

# import crashes and trim to temporal parameters and make spatial
crashes_gb <- get_stats19("5 years", type = "collision") |> 
  filter(collision_year >= 2020 & collision_year <= 2024) |> 
  format_sf()

crashes_in = crashes_gb[geo_buf,] |> 
  mutate(severity = ifelse(collision_severity == "Fatal",1.5,collision_adjusted_severity_serious))

tm_shape(geo_buf)+
  tm_polygons(alpha = 0,lwd = 3)+
  tm_shape(crashes_in)  +
  tm_symbols(
    size = "severity",
    fill = "number_of_casualties",
    col = "black",
    size.scale = tm_scale_continuous(values.range = c(0.6,1.2)),
    fill.scale = tm_scale_intervals(n = 5, values = "tol.incandescent"))


LSOA = IMD_poly[imd_lowest,]


LSOA_1km = LSOA |> 
  select(LSOA21CD,IMDDecil,SHAPE) |> 
  st_buffer(buf)

LSOA_play = st_intersection(osm_os,LSOA_1km) |> 
  mutate(tot_area_m2 = as.numeric(st_area(geom)))

LSOA_crashes = crashes_in[LSOA_1km,] |> 
  filter(collision_severity %in% c("Serious","Fatal"))

LSOA_1km$play_area = sum(LSOA_play$tot_area_m2)/1000000  

LSOA_1km$rd_cas = sum(as.numeric(LSOA_crashes$number_of_casualties))  

tm_shape(LSOA_1km)+
  tm_polygons(alpha = 0,lwd = 1)+
  tm_shape(LSOA)+
  tm_polygons(alpha = 0,lwd = 3)+
  tm_shape(LSOA_play) +
  tm_polygons(fill = "fclass",
              fill.scale = tm_scale_categorical())+
  tm_shape(LSOA_crashes)  +
  tm_symbols(
    fill = "number_of_casualties",
    fill.scale = tm_scale_intervals(n = 5, values = "tol.incandescent"))

lsoa_list = list()
for(l in IMD_poly$LSOA21CD){
  
  LSOA = IMD_poly[IMD_poly$LSOA21CD == l,]
  
  LSOA = select(LSOA, LSOA21CD,LSOA21NM,msoa_hcl_nm, IMDDecil,IMDRank,SHAPE)
  
  LSOA_1km = LSOA |> 
    st_buffer(buf)
  
  LSOA_play = st_intersection(osm_os,LSOA_1km) |> 
    mutate(tot_area_m2 = as.numeric(st_area(geom)))
  
  LSOA_crashes = crashes_in[LSOA_1km,] |> 
    filter(collision_severity %in% c("Serious","Fatal"))
  
  LSOA$play_area = sum(LSOA_play$tot_area_m2)/1000000  
  
  LSOA$rd_cas = sum(as.numeric(LSOA_crashes$number_of_casualties))
  
  lsoa_list[[l]] = LSOA
  whereupto = paste0(round(which(l == IMD_poly$LSOA21CD)/NROW(IMD_poly)*100,1),"% done")
print(whereupto)
  }


lsoa_total = do.call(rbind,lsoa_list)

play_max = max(lsoa_total$play_area)
cas_max = max(lsoa_total$rd_cas)

# both ascending
ps_breaks = seq(0, play_max, length.out = 11)
cra_breaks = seq(0, cas_max, length.out = 11)
bk_labels = as.character(seq(1, 10))


# play score breaks
ps_breaks = seq(0,play_max,length.out = 11)
bk_labels = as.character(seq(1,10))

# crash score breaks
cra_breaks = seq(cas_max,0,length.out = 11)

lsoa_scores = lsoa_total |> 
  dplyr::mutate(play_score = cut(play_area,
                                 breaks = ps_breaks, labels = bk_labels)) |> 
  dplyr::mutate(crash_score = cut(rd_cas,
                                  breaks = cra_breaks, labels = rev(bk_labels))) |> 
  rowwise() |> 
  mutate(total_score = sum(as.numeric(IMDDecil),as.numeric(play_score),as.numeric(crash_score)),
         crash_play_score = sum(as.numeric(play_score),as.numeric(crash_score)))



library(reactable)
library(htmlwidgets)

lsoa_table = lsoa_scores |> 
  st_set_geometry(NULL) |> 
  transmute(`LSOA code` = LSOA21CD,
            `LSOA name` = LSOA21NM,
            `MSOA name` = msoa_hcl_nm,
            `IMD Rank` = IMDRank,
            `Play Area (km2)` = round(play_area,2),
            `Road casualties` = rd_cas,
            `IMD score` = IMDDecil,
            `Play score` = play_score,
            `Crash score` = crash_score,
            `Crash Play score` = crash_play_score,
            `Total score` = round(total_score)) |> 
  dplyr::arrange(desc(`Total score`)) |>
  reactable(
    theme = reactableTheme(
      style = list(fontFamily = "Geist, system-ui, sans-serif", fontSize = "13px"),
      headerStyle = list(fontWeight = 600),
      cellStyle = list(lineHeight = "1.5")
    ),
    defaultColDef = colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      cell = function(value) format(value, nsmall = 1),
      align = "center",
      minWidth = 30,
      headerStyle = list(background = "grey")
    ),
    columns = list(
      name = colDef(minWidth = 80)  # overrides the default
    ),
    bordered = TRUE,
    highlight = TRUE,
    defaultPageSize = 30
  )

htmlwidgets::saveWidget(lsoa_table, paste0("OUT/TAB/manchester_lsoa_table.html"), selfcontained = TRUE)

backgroup_map = "CartoDB.Positron"
tmap_mode("view")
# initial geometry extent to start map off
tm1 = tm_shape(geo_buf) +
  tm_polygons(fill_alpha = 0,
              col_alpha = 0,
              popup.vars = FALSE,
              group = "LAs", 
              group.control = "hide")

geo_type = rev(names(lsoa_scores)[c(1,4,6,7,9,10,12,11)])

cols2use = rev(c("grey", "-brewer.oranges","brewer.pu_rd","brewer.bu_pu","brewer.yl_or_rd","-matplotlib.reds","brewer.yl_or_rd","-tol.incandescent"))

# create function to loop through
tm1 = reduce(geo_type, function(map_obj, ct) {
  
  t_df = select(lsoa_scores,LSOA21NM,msoa_hcl_nm, !!sym(ct))
  
  pal = cols2use[which(ct == geo_type)]
  
  if(ct == "LSOA21CD"){
    alp = 0
  } else {
  alp = 0.6
  }
  print(ct)
  map_obj = map_obj+
    tm_shape(t_df) +
    tm_polygons(fill = tm_vars(ct),fill_alpha = alp,
                fill.scale = tm_scale_continuous(values = pal),
                #fill.legend = tm_legend(show = FALSE),
                lwd = 0.5,
                group = ct,
                group.control = "radio")
  
  map_obj
  
  
}, .init = tm1)

map_title = "Calculated scores for the Local Authority of Manchester"

tm1 <- tm1 + 
  tm_basemap(backgroup_map, group.control = "none")+ 
  tm_title(map_title)+
  # tm_add_legend(
  #   type = "polygons",
  #   labels = names(qpal),
  #   fill = unname(qpal)
  # )+
  tm_view(control.collapse = FALSE,overlay.groups = geo_type[1])

tm1

saveRDS(tm1,"OUT/MAP/manchester_scores.RDS")

# full population data for lsoa areas
lsoa_pop_all = read.csv("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/lsoa21_pop_age_sex_2011_2024.csv")

# for age breaks
age_breaks = c(-1, 5, 10, 14, 18)
age_labels = c("0-5", "6-10", "11-14", "14-18")

pop_male = lsoa_pop_all |> 
  filter(grepl("M", sex_age)) |> 
  mutate(age = gsub("M","", sex_age)) |> 
  dplyr::mutate(age_band = cut(as.numeric(age),
                               breaks = age_breaks, labels = age_labels)) |> 
  group_by(LSOA.2021.Code, age_band) |> 
  summarise(population = sum(X2024,na.rm = TRUE)) |> 
  mutate(sex = "Male") |> 
  filter(!is.na(age_band))

pop_female = lsoa_pop_all |> 
  filter(grepl("F", sex_age)) |> 
  mutate(age = gsub("F","", sex_age)) |> 
  dplyr::mutate(age_band = cut(as.numeric(age),
                               breaks = age_breaks, labels = age_labels)) |> 
  group_by(LSOA.2021.Code, age_band) |> 
  summarise(population = sum(X2024,na.rm = TRUE)) |> 
  mutate(sex = "Female")|> 
  filter(!is.na(age_band))

lsoa_pop = rbind(pop_male,pop_female)

#lsoa_pop = readRDS("OUT/DAT/pop_age_sex.RDS")

lsoa_score_pop = left_join(lsoa_scores,lsoa_pop, by = c("LSOA21CD" = "LSOA.2021.Code"))


backgroup_map = "CartoDB.Positron"

tmap_mode("view")
# initial geometry extent to start map off
tm1 = tm_shape(geo_buf) +
  tm_polygons(fill_alpha = 0,
              col_alpha = 0,
              popup.vars = FALSE,
              group = "LAs", 
              group.control = "hide")

geo_df = expand.grid(age = unique(lsoa_pop$age_band),sex = unique(lsoa_pop$sex)) |> 
  mutate(age_sex = paste(sex,age))


geo_type = geo_df$age_sex
# create function to loop through
tm1 = reduce(geo_type, function(map_obj, ct) {
  
  demo_df = filter(geo_df,age_sex == ct)
  
  pop_df = filter(lsoa_score_pop,age_band == demo_df$age & sex == demo_df$sex)
  
  map_obj = map_obj+
    tm_shape(pop_df) +
    tm_polygons(fill = "population",
                fill.scale = tm_scale_intervals(n = 6, # for n classes 
                                                style = "fixed",    
                                                breaks = seq(0,max(lsoa_score_pop$population),30), # you need n+1 number of breaks
                                                values = "brewer.blues"),
                #fill.legend = tm_legend(show = FALSE),
                lwd = 0.5,
                group = ct,
                group.control = "radio")
  
  map_obj
  
  
}, .init = tm1)

map_title = "18 and under population of LSOA areas split by age and sex"

tm1 <- tm1 + 
  tm_basemap(backgroup_map, group.control = "none")+ 
  tm_title(map_title)+
  # tm_add_legend(
  #   type = "polygons",
  #   labels = names(qpal),
  #   fill = unname(qpal)
  # )+
  tm_view(control.collapse = FALSE,overlay.groups = geo_type[1])

tm1

saveRDS(tm1, paste0("OUT/MAP/manchester_pop_map.RDS"))


summary = lsoa_score_pop |>
  st_set_geometry(NULL) |> 
  group_by(age_band, sex) |>
  summarise(
    weighted_total = weighted.mean(total_score, pop, na.rm = TRUE),
    weighted_play = weighted.mean(as.numeric(play_score), pop, na.rm = TRUE),
    weighted_crash = weighted.mean(as.numeric(crash_score), pop, na.rm = TRUE),
    total_pop = sum(pop, na.rm = TRUE),
    n_regions = n()
  )

M14_18 = filter(summary,age_band == "14-18" & sex == "Male")



summary_table = summary |> 
  transmute(`Age band` = age_band,
            `Sex` = sex,
            `Weighted total` = round(weighted_total,1),
            `Weighted play` = round(weighted_play,1),
            `Weighted crash` = round(weighted_crash,1),
            `Total population` = total_pop) |> 
  dplyr::arrange(desc(`Weighted total`)) |>
  reactable(
    theme = reactableTheme(
      style = list(fontFamily = "Geist, system-ui, sans-serif", fontSize = "13px"),
      headerStyle = list(fontWeight = 600),
      cellStyle = list(lineHeight = "1.5")
    ),
    defaultColDef = colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      cell = function(value) format(value, nsmall = 1),
      align = "center",
      minWidth = 30,
      headerStyle = list(background = "grey")
    ),
    columns = list(
      name = colDef(minWidth = 80)  # overrides the default
    ),
    bordered = TRUE,
    highlight = TRUE,
    defaultPageSize = 30
  )

htmlwidgets::saveWidget(summary_table, paste0("OUT/TAB/summary_table.html"), selfcontained = TRUE)

summary_table


