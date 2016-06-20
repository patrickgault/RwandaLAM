# Initial analysis of digital-related variables --------------------------
# Craig Jolley, USAID GeoCenter, cjolley.usaid@gmail.com
# 20 June 2016
# (c) 2016 via MIT License

source('R/01_RW_cleanDHS.R')
source('R/simple_plots.R')

adm2_map(hh_clean,'landline')
# Landlines are rare everywhere; highest penetration is around Kigali but
# still only ~1.5%

adm2_map(hh_clean,'mobile')
mean(hh_clean$mobile,na.rm=TRUE)
# 90% of households in greater Kigali have a mobile, compared to 61% nationwide

adm2_map(hh_clean,'computer')
# quite rare everywhere except Kigali

adm2_map(hh_clean,'tv')
# Lots in Kigali; very few elsewhere

adm2_map(hh_clean,'radio')
# Fairly common throughout the country

adm2_map(hh_clean,'bank')
# actually more common than I would have thought in a lot of the country
tmp <- hh_clean[,c('cluster_id','bank','mobile')]
tmp$mobile_no_bank <- as.numeric(tmp$mobile & !tmp$bank)
adm2_map(tmp,'mobile_no_bank')
# possible strong growth areas for mobile money - 30% of households unbanked
# but with mobiles

adm2_map(hh_clean,'electricity')
# Electrification is high in Kigali, generally low elsewhere. This helps
# explain the lack of TVs and computers -- devices outside the capital are
# things like mobiles and radios that can run on batteries.

