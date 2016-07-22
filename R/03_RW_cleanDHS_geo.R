# Cleanup geographic dataset for Rwanda DHS data --------------------------
# Laura Hughes, lhughes@usaid.gov
# 10 June 2016
# (c) 2016 via MIT License

# select relevant variables -----------------------------------------------

geo_clean = geo %>% 
  select(cluster_num = DHSCLUST, # should match v001 in DHS main files.
         province = ADM1NAME,
         province_id = ADM1DHS, # should match v024
         district = DHSREGNA,
         district_id = DHSREGCO, # should match shdistrict
         rural = URBAN_RURA,
         lat = LATNUM,
         lon = LONGNUM,
         altitude = ALT_DEM) %>%  # altitude in meters, from SRTM (Shuttle Radar Topography Mission) DEM (Digital Elevation Model). 30 arc-sec resolution (~1 m)
  mutate(rural = ifelse(rural == 'R', 1, # recode rural category to be binary (1 if rural, 0 if urban)
                        ifelse(rural == 'U', 0,
                               NA)))

# load shapefile -----------------------------------------------
# Note: shapefiles came from: http://www.gadm.org/download
# I also put them on the google drive: 
# https://drive.google.com/open?id=0B0cwL9QVH9UKUFpOQ3lBZkR5Snc

rwanda <- readOGR(dsn='GIS/GADM_adm2',layer='RWA_adm2')
rwanda@data$id = rwanda@data$ID_2
rwanda.points <- fortify(rwanda,region='ID_2')
rwanda.adm2 <- plyr::join(rwanda.points,rwanda@data,by='id')
rm(rwanda,rwanda.points)