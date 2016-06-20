# Initial analysis of WASH-related variables --------------------------
# Craig Jolley, USAID GeoCenter, cjolley.usaid@gmail.com
# 20 June 2016
# (c) 2016 via MIT License

source('R/01_RW_cleanDHS.R')
source('R/simple_plots.R')


##############################################################################
# Explore the WASH-relevant variables in hh (Household-level)
###############################################################################

# hv201 - source of drinking  water
categ_bars(hh_clean,'water_source')
# Most prevalent are protected springs, public taps/standpipes

# hv205 - type of toilet facility
categ_bars(hh_clean,'toilet_type')
# Pit latrines with and without slabs are the most popular

# hv230a - place where hands are washed
categ_bars(hh_clean,'handwashing_site')
# Most common for handwashing site not to be observed in dwelling

# hv230b - presence of water at hand-washing place
mean(hh_clean$handwashing_water,na.rm=TRUE)  #  54.7% have water
adm2_map(hh_clean,'handwashing_water')

# hv232 - items present: soap/detergent
mean(hh_clean$has_soap,na.rm=TRUE)  #  54% have soap
adm2_map(hh_clean,'has_soap')

# water purification methods
multi_var_bars(hh_clean,
               c('water_treat','water_treat_boil','water_treat_bleach',
                 'water_treat_filter','water_treat_solar','water_treat_settle'))
adm2_map(hh_clean,'water_treat')
adm2_map(hh_clean,'water_treat_bleach')
adm2_map(hh_clean,'water_treat_filter') 
# water filters seem only to be popular in the west

# sh106c - frequency of washing water containers in a week
categ_bars(hh_clean,'container_wash')

# sh109aa-c - cleanliness of toilet facility
multi_var_bars(hh_clean,c('toilet_clean_dry','toilet_clean_urine','toilet_clean_flies'))
adm2_map(hh_clean,'toilet_clean_dry')
# the northwest seems to be home to Rwanda's grossest toilets
adm2_map(hh_clean,'toilet_clean_urine')
adm2_map(hh_clean,'toilet_clean_flies')

