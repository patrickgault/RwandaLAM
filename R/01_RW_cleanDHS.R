# Cleanup datasets for Rwanda DHS data --------------------------
# Laura Hughes, USAID GeoCenter, lhughes@usaid.gov
# 10 June 2016
# (c) 2016 via MIT License
# 
# This function is the master file for cleaning and systematizing
# data from the 2014/2015 Rwanda DHS for use in a customized analysis by
# USAID's GeoCenter for the USAID | Rwanda mission.

# Overview of functions and operations ------------------------------------
# (0): RAW: load the raw data from the DHS
# (1): CLEAN: this file! Calls submodules to clean and aggregate data.
#      Mostly used for merges to compile data from different sections.
# (2): HH: load in the household module; clean and pull relevant vars
# (3): GEO: load in the geographic coordinates from the cluster and merge
# (4): KIDS: individual data from the children's module on anthropomorphic data
#      and dietary diversity.


# (0) load raw data -----------------------------------------------------------
source(file = 'R/00_RW_loadDHS.R')


# (2) household variables -------------------------------------------------
source('R/02_RW_cleanDHS_hh.R')


# (3) geo -----------------------------------------------------------------
source('R/03_RW_cleanDHS_geo.R')


# (4) children’s module [INDIVIDUAL] --------------------------------------


