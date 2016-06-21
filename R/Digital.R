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

fitme <- hh_all %>% select(
  cluster_id = hv001,
  wealth=hv271,
  mobile=hv243a,
  urban=hv025,
  hoh_sex=hv219,
  hoh_age=hv220,
  hoh_ed=hv106_01,
  num_people=hv009,
  landline = hv221,         # has landline phone?
  computer = sh110g,        # has computer?
  tv = hv208,               # has television?
  radio = hv207,            # has radio?
  electricity = hv206,      # has electricity?
  bank = hv247,             # has bank account
) %>% mutate(
  urban = ifelse(urban == 1, 1,ifelse(urban == 2, 0,NA))
  )
fitme$district <- join(fitme,geo_clean,by='cluster_id')$district

dist_avg <- fitme %>% 
  group_by(district) %>% 
  summarise(m=mean(mobile,na.rm=TRUE))

dist_avg$q <- cut(dist_avg$m,quantile(dist_avg$m),labels=1:4,include.lowest=TRUE)
fitme$geo_q <- join(fitme,dist_avg,by='district')$q %>% as.numeric()

# Note that this is a little funny, because mobile ownership is probably also
# included in the calculation of the wealth index, so these aren't really
# independent.

ggplot(fitme,aes(wealth,mobile)) +
  geom_jitter(width=0,height=0.4,size=2,color='tomato',alpha=0.05) +
  geom_smooth(method="glm", method.args = list(family = "binomial")) +
  theme_classic()
# Wealthy people definitely are more likely to have a mobile, but there is
# a fair amount of overlap between the two distributions.

glm(mobile ~ wealth + urban + geo_q + hoh_age + hoh_ed + num_people +
      landline + tv + computer + radio + bank + hoh_sex + electricity,
    family=binomial(link='logit'),data=fitme) %>%
  summary()
# AIC = 10985

# Households are much more likely to own a mobile phone if they are
#   Wealthy
#   Located in certain geographic areas
#   Have a young head of household
#   Have an educated head of household
#   Have a female head of household
#   Have more people living there
#   Don't have a landline phone
#   Do have a radio
#   Do have a bank account

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


