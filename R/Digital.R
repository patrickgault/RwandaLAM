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

