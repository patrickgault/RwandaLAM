# Cleanup household dataset for Rwanda DHS data --------------------------
# Laura Hughes, USAID GeoCenter, lhughes@usaid.gov
# 10 June 2016
# (c) 2016 via MIT License


# basic info --------------------------------------------------------------

hh_clean = hh %>% 
  select(hhid,
         cluster_id = hv001)


# WASH indicators ---------------------------------------------------------
# @cjolley: want to give it a go?
hh_clean = hh_clean
