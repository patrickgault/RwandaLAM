# Cleanup geographic dataset for Rwanda DHS data --------------------------
# Laura Hughes, lhughes@usaid.gov
# 10 June 2016
# (c) 2016 via MIT License


# load raw data -----------------------------------------------------------
source(loadRwDHS.R)


# select relevant variables -----------------------------------------------

geo_clean = geo %>% 
  select(cluster_id = DHSCLUST, # should match v001 in DHS main files.
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