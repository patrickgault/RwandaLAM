# Initial analysis of digital-related variables --------------------------
# Craig Jolley, USAID GeoCenter, cjolley.usaid@gmail.com
# 20 June 2016
# (c) 2016 via MIT License

source('R/01_RW_cleanDHS.R')
source('R/simple_plots.R')

###############################################################################
# Simple choropleth maps
###############################################################################

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

# TODO: Would be nice to overlay some maps with VIIRS night lights images.

adm2_map(hh_clean,'wealth')
# So the concentration of some goods in Kigali could have as much to do with
# household wealth as with infrastructure. Separating the two out might
# take some work.

###############################################################################
# What determines who owns a mobile phone?
###############################################################################

# Which assets are most predictive of mobile phone ownership?
assets <- hh_all %>% 
  select(hv205:hv212,hv213:hv215,hv221,hv225,hv227,hv242:hv244,hv246,hv247,
         sh110g,sh118f) %>%
  mutate(
    # single out the most popular types of toilets
    pit_toilet = as.numeric(hv205 > 19 & hv205 < 24),
    no_toilet = as.numeric(hv205 == 31),
    flush_toilet = as.numeric(hv205 < 16),
    # floor materials
    earth_floor = as.numeric(hv213 < 21),
    improved_floor = as.numeric(hv213 < 20),
    # wall materials
    simple_walls = as.numeric(hv214 < 23),
    adobe_walls = as.numeric(hv214 == 23),
    nice_walls = as.numeric(hv213 > 23),
    # roof materials
    simple_roof = as.numeric(hv215 < 31),
    metal_roof = as.numeric(hv215 == 31),
    nice_roof = as.numeric(hv215 > 31)
  ) %>%
  select(hv206:hv212,hv221:nice_roof)

nrow(na.omit(assets)) / nrow(assets)
# Only 22.6% contain no missing values; we'll need to do some imputation here

library(mice)
assets_imp <- mice(assets)

form <-  select(assets,-hv243a) %>% names() %>% 
  paste(collapse=' + ') %>% paste('hv243a ~ ',.,collapse='')

fit_assets <- with(data=assets_imp,exp=glm(as.formula(form),family=binomial(link='logit')))
summary(fit_assets)
# The strongest correlations seem to be with 
#   hv207 - radio (55.3%)
#   hv247 - bank account (47.9%)
#   hv210 - bicycle (14.1%)
#   hv206 - electricity (25.1%)
# Mobile phone ownership is 61.1%

# Which household demographics are most predictive of mobile ownership?

# NOTE: hv106-9 all measure education level with slightly different categories
# Choose the one that gets me the best AIC.

demo <- hh_all %>%
  select(hv243a,hv009:hv014,hv025,hv219,hv220,hv107_01,hv115_01) %>%
  mutate(hv115_01 = as.factor(hv115_01))
nrow(na.omit(demo)) / nrow(demo) # 99.4% complete -- no need for imputation

fit_demo <- glm(hv243a ~ .,family=binomial(link='logit'),data=demo)
summary(fit_demo)
# Which education variable to use?
#  hv106 gets me AIC = 13864
#  hv107 gets me AIC = 10054
#  hv108 gets me AIC = 13543
#  hv109 gets me AIC = 13650

# Strongest correlations:
#    hv025 - urban 
#    hv107_01 - more educated HoH
#    hv014 - fewer kids
#    hv010 - more women in HH
#    hv115_01=5 - not living together
#    hv220 - younger HoH

# The gender piece here is interesting; more adults in the house generally 
# means greater chance of mobile ownership (hv009,10,11), with women having
# a stronger effect than men. Male-headed households are more likely to 
# have one, though (hv219).

###############################################################################
# Export lat/lon and average of each digital indicator (for kriging)
###############################################################################

cluster_gp <- hh_clean %>%
  group_by(cluster_id) %>%
  summarise(landline=mean(landline,na.rm=TRUE),
            mobile=mean(mobile,na.rm=TRUE),
            computer=mean(computer,na.rm=TRUE),
            tv=mean(tv,na.rm=TRUE),
            radio=mean(radio,na.rm=TRUE),
            electricity=mean(electricity,na.rm=TRUE),
            bank=mean(bank,na.rm=TRUE)) %>%
  join(geo_clean,by='cluster_id') %>%
  select(lat,lon,landline:bank)
 
write.csv(cluster_gp,'GIS/mobile-vars.csv',row.names=FALSE)

###############################################################################
# Compare DHS indicators with WB data
###############################################################################

library(WDI)

##### Electricity access ------------------------------------------------------
elec <- WDI(country='RW',indicator='EG.ELC.ACCS.ZS',start=1960,end=2015) %>% 
  na.omit() %>%
  mutate(val=EG.ELC.ACCS.ZS,source='WB') %>%
  select(val,year,source) 

dhs_elec <- loadDHS(breakdown='national',indicators='HC_ELEC_H_ELC',countries='RW') %>%
  select(Value,SurveyYear) %>%
  rename(val=Value,year=SurveyYear) %>%
  mutate(source='DHS')

elec <- rbind(elec,dhs_elec)
  
ggplot(elec,aes(x=year,y=val,group=source,color=source)) +
  geom_point(size=4) +
  geom_line(size=2,alpha=0.6) +
  theme_classic() +
  ylab('Electricity access (%)')

# DHS and WB data roughly agree; with DHS data we can get into more of the
# geographic distribution.

##### Mobile ownership ------------------------------------------------------
mobile <- WDI(country='RW',indicator='IT.CEL.SETS.P2',start=1960,end=2015) %>% 
  na.omit() %>%
  mutate(val=IT.CEL.SETS.P2,source='WB') %>%
  select(val,year,source) %>%
  filter(year > 1998)

dhs_mobile <- loadDHS(breakdown='national',indicators='HC_HEFF_H_MPH',countries='RW') %>%
  select(Value,SurveyYear) %>%
  rename(val=Value,year=SurveyYear) %>%
  mutate(source='DHS')

mobile <- rbind(mobile,dhs_mobile)
ggplot(mobile,aes(x=year,y=val,group=source,color=source)) +
  geom_point(size=4) +
  geom_line(size=2,alpha=0.6) +
  theme_classic() +
  ylab('Mobile ownership (%)')

# These roughly agree, despite the fact that they aren't measuring exactly the
# same thing -- WB data are for subscriptions per 100 people (and therefore 
# could be >100), while DHS are the percentage of surveyed households with 
# mobile phones. The slowdown in DHS growth could be a sign of saturation at 
# the household level, while subscriptions continue to grow.

# TODO: Add in GSMA data as well and see if it agrees

##### Landline ownership ------------------------------------------------------
phone <- WDI(country='RW',indicator='IT.MLT.MAIN.P2',start=1960,end=2015) %>% 
  na.omit() %>%
  mutate(val=IT.MLT.MAIN.P2,source='WB') %>%
  select(val,year,source)

dhs_phone <- loadDHS(breakdown='national',indicators='HC_HEFF_H_NPH',countries='RW') %>%
  select(Value,SurveyYear) %>%
  rename(val=Value,year=SurveyYear) %>%
  mutate(source='DHS')

phone <- rbind(phone,dhs_phone)
ggplot(phone,aes(x=year,y=val,group=source,color=source)) +
  geom_point(size=4) +
  geom_line(size=2,alpha=0.6) +
  theme_classic() +
  ylab('Telephone ownership (%)')

# Not sure what to make of this, except that the numbers here might not be
# super-accurate. The big story, I think, is how low it is relative to the 
# mobile explosion.


