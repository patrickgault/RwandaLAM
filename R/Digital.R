# Initial analysis of digital-related variables --------------------------
# Craig Jolley, USAID GeoCenter, cjolley.usaid@gmail.com
# 20 June 2016
# (c) 2016 via MIT License

source('R/01_RW_cleanDHS.R')
source('R/simple_plots.R')

# TODO: What I'd eventually like to see this script become is a standardized
# set of plotting functions that we could apply to any (suitably-cleaned) DHS
# to get a snapshot of digital connectivity in that country.

###############################################################################
# Simple choropleth maps
###############################################################################

adm2_map(hh_clean,'telephone')
# Landlines are rare everywhere; highest penetration is around Kigali but
# still only ~1.5%

adm2_map(hh_clean,'mobile')
# Lowest penetration in the SW
mean(hh_clean$mobile,na.rm=TRUE)
hh_clean$mobile %>% mean(na.rm=TRUE)  # 61% penetration
gap_map(hh_clean %>% filter(sex_head==1),
        hh_clean %>% filter(sex_head==2),
        'mobile',title='Mobile ownership gender gap')
hh_clean %>% mutate(w1=wealth_index==1) %>% adm2_map('w1')
# More of the poorest 20% live in the west
gap_map(hh_clean %>% filter(wealth_index > 1),
        hh_clean %>% filter(wealth_index == 1),
        'mobile',title='Mobile ownership wealth gap')
# Mobile ownership wealth gap seems larger in the east, but that could 
# just be because there are more generally wealthy people around there.
# I wonder if this might be better approached with spatial regression;
# where is the relationship between wealth and mobile ownership strongest?

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

adm2_map(hh_clean,'wealth_score')
# So the concentration of some goods in Kigali could have as much to do with
# household wealth as with infrastructure. Separating the two out might
# take some work.

###############################################################################
# What determines who owns a mobile phone?
###############################################################################

# Which assets are most predictive of mobile phone ownership?
assets <- hh_clean %>% 
  select(toilet_type,electricity,radio,tv,fridge,bike,motorcycle,car,
         floor_material,wall_material,roof_material,telephone,mosquito_net,
         kitchen,mobile,watch,cart,motorboat,livestock,bank,computer,boat,
         wealth_score) %>%
  rename(wealth=wealth_score) %>%
  mutate(
    # single out the most popular types of toilets
    pit_toilet = as.numeric(toilet_type > 19 & toilet_type < 24),
    no_toilet = as.numeric(toilet_type == 31),
    flush_toilet = as.numeric(toilet_type < 16),
    # floor materials
    earth_floor = as.numeric(floor_material < 21),
    improved_floor = as.numeric(floor_material > 20),
    # roof materials
    simple_roof = as.numeric(roof_material < 31),
    metal_roof = as.numeric(roof_material == 31),
    nice_roof = as.numeric(roof_material > 31)
  ) %>%
  select(electricity:car,telephone:nice_roof)

nrow(na.omit(assets)) / nrow(assets)
# Only 24.6% contain no missing values; we'll need to do some imputation here

assets_imp <- mice(assets)

form <-  select(assets,-mobile) %>% names() %>% 
  paste(collapse=' + ') %>% paste('mobile ~ ',.,collapse='')

fit_assets <- with(data=assets_imp,exp=glm(as.formula(form),family=binomial(link='logit')))
summary(fit_assets)
# Including the wealth index lets us ask which other assets mobile owners are
# likely to own, independent of wealth.
# Wealth on its own is the strongest correlation, followed by radios, bicycles,
# in-home kitchens, and bank accounts.
adm2_map(hh_clean,'bike')
# Bicycles, it turns out, are only common in the east.

# For each asset, what is the wealth index level at which people are
# more likely to own one than not to? 

ggplot(assets,aes(wealth,mobile)) +
  geom_jitter(size=4,color='tomato',alpha=0.1,width=0,height=0.4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic() +
  ylab('Has mobile phone')

# Be more systematic
good_assets <- ldply(names(select(assets,-wealth)),function(n) {
  f <- paste(n,' ~ wealth',collapse='') %>% as.formula()
  s <- glm(f,family=binomial(link='logit'),data=assets) %>% summary()
  w50 <- -s$coefficients[1,1]/s$coefficients[2,1]
  in_range <- w50 > min(assets$wealth) & w50 < max(assets$wealth)
  haves <- assets[assets[,n]==1,'wealth']
  have_nots <- assets[assets[,n]==0,'wealth']
  pos <- mean(haves,na.rm=TRUE) > mean(have_nots,na.rm=TRUE)
  data.frame(n=n,in_range=in_range,pos=pos,w50=w50)
}) %>%
  filter(in_range & pos) %>%
  select(n,w50) %>%
  mutate(x=0,w50=w50/1e5) %>%
  arrange(w50)
good_assets

# So it looks like, once people get a little bit of money, the first thing
# they invest in is a corrugated metal roof, and the next is a mobile phone.

y_nudges <- c(0,0,0,0,0,0,-0.04,0.04,0,-0.05,0.05,0.02)
ggplot(good_assets,aes(x=x,y=w50,label=n)) +
  geom_point(size=5,color='gray59') +
  geom_text(hjust=1,nudge_x=-0.03,nudge_y=y_nudges) +
  scale_x_continuous(limits=c(-0.3,1)) +
  theme_classic() +
  theme(axis.title=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank())

# While this kind of visualization is useful within a single year, I'm
# reluctant to compare with 2010, since the wealth indices themselves
# don't necessarily match up well. Instead, just look at adoption rates
# for each of these wealth-level-defining goods.

# TODO: Instead of a vertical plot against nothing, it might be nice
# to combine this with a histogram of wealth levels, so we can see how
# many people are likely to have each asset.

# What has changed since 2010?

hh2010 <- read_dta('Datain/RW_2010_DHS/rwhr61dt/RWHR61FL.DTA')
hh2010_labels = pullAttributes(hh2010) %>% 
  mutate(module = 'hh', rowNum = row_number())
assets2010 <- hh2010 %>% 
  select(hv205:hv212,hv213:hv215,hv221,hv227,hv242:hv244,hv246,hv247,
         sh110g,sh118f,hv271) %>%
  mutate(
    # fix NAs
    electricity = ifelse(hv206==9,NA,hv206),
    radio = ifelse(hv207==9,NA,hv207),
    tv = ifelse(hv208==9,NA,hv208),
    fridge = ifelse(hv209==9,NA,hv209),
    bike = ifelse(hv210==9,NA,hv210),
    motorcycle = ifelse(hv211==9,NA,hv211),
    car = ifelse(hv212==9,NA,hv212),
    telephone = ifelse(hv221==9,NA,hv221),
    mosquito_net = ifelse(hv227==9,NA,hv227),
    kitchen = ifelse(hv242==9,NA,hv242),
    mobile = ifelse(hv243a==9,NA,hv243a),
    watch = ifelse(hv243b==9,NA,hv243b),
    cart = ifelse(hv243c==9,NA,hv243c),
    motorboat = ifelse(hv243d==9,NA,hv243d),
    bank = ifelse(hv247==9,NA,hv247),
    computer = ifelse(sh110g==9,NA,sh110g),
    boat = ifelse(sh118f==9,NA,sh118f),
    # single out the most popular types of toilets
    pit_toilet = as.numeric(hv205 > 19 & hv205 < 24),
    no_toilet = as.numeric(hv205 == 31),
    flush_toilet = as.numeric(hv205 < 16),
    # floor materials
    earth_floor = as.numeric(hv213 < 21),
    improved_floor = as.numeric(hv213 > 20),
    # roof materials
    simple_roof = as.numeric(hv215 < 31),
    metal_roof = as.numeric(hv215 == 31),
    nice_roof = as.numeric(hv215 > 31)
  ) %>%
  rename(wealth=hv271) %>%
  select(electricity:car,telephone:nice_roof) %>%
  removeAttributes()

# Compare adoption rates for 2010 and 2014
# TODO: I'm not sure exactly how to adjust these for sampling weights

adoption_plot <- function(q) {
  plotme <- good_assets %>% select(n) %>%
    plyr::adply(1,function(x) {
      v <- as.character(x$n)
      m10 <- mean(assets2010[hh2010$hv270 %in% q,v],na.rm=TRUE)
      m15 <- mean(assets[hh$hv270 %in% q,v],na.rm=TRUE)
      mutate(x,
        mean_10 = m10,
        mean_15 = m15
      )
    }) %>%
    melt(id.vars=c('n')) %>%
    mutate(x=ifelse(variable=='mean_10',0,1))
  ggplot(plotme,aes(x=x,y=value,group=n,label=n)) +
    geom_point(aes(color=n),size=5) +
    geom_line(aes(color=n),size=3,alpha=0.6) +
    geom_text(hjust=1,nudge_x=-0.03) +
    scale_x_continuous(limits=c(-0.5,1)) +
    theme(title = element_blank(), axis.title = element_blank(), 
          axis.text.x = element_blank(), axis.ticks = element_blank(), 
          axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
          panel.grid = element_blank(), panel.background = element_blank(), 
          plot.background = element_blank(), legend.position = "none")
}

adoption_plot(1:5) # adoption of different assets across all wealth levels

# So connectivity-related assets have increased across the board, but mobiles
# are particularly strong and are now really close to being the most 
# commonly-held asset (out of this set). In comparison, people just aren't 
# buying radios like they did 5 years ago. And who needs a watch?

# What do these patterns of asset ownership look like among the poorest quintile?

adoption_plot(1)

# Government subsidies of metal roofing have done a lot for the poorest
# Rwandans. Mobiles used to be rare among the poorest 20%, but are now
# almost as common as radios. Bank accounts are also suprpsingly common.

adoption_plot(5)

# The wealthiest Rwandans are mighty close to mobile saturation, but things
# like flush toilets are still pretty rare. Biggest change for this group
# since 2010 has been electrification.

# Which household demographics are most predictive of mobile ownership?

# NOTE: hv106-9 all measure education level with slightly different categories
# Choose the one that gets me the best AIC.

demo <- hh_clean %>%
  select(mobile,num_members,num_under5,urban,sex_head,age_head,ed_head,marital_head) %>%
  mutate(married = marital_head == 1,
         peeps_lt4 = num_members < 4,
         no_ed = ed_head == 0,
         primary_ed = ed_head > 0) %>%
  select(-marital_head,-ed_head,-num_members)



fit_demo <- glm(mobile ~ .,family=binomial(link='logit'),data=demo)
summary(fit_demo)


# Strongest correlations:
#    more educated HoH
#    urban 
#    more people in house
#    fewer kids
#    married
#    younger HoH
#    male HoH - not significant!

# How many stay significant when we factor in wealth?
demo_wealth <- cbind(demo,assets[,'wealth']/1e6)
glm(mobile ~ .,family=binomial(link='logit'),data=demo_wealth) %>%
  summary()
# Most correlations still hold up, except for being married. Being urban
# also becomes a lot less significant.

# Compare with 2010 -- how does adoption growth differ among demographics?

demo_2010 <- hh2010 %>%
  select(hv243a,hv009,hv014,hv025,hv219,hv220,hv106_01,hv115_01) %>%
  mutate(married = hv115_01 == 1,
         no_ed = hv106_01 == 0,
         primary_ed = hv106_01 > 0 & hv106_01 < 9,
         peeps_lt4 = hv009 < 4,
         urban = hv025==1) %>%
  select(-hv115_01,-hv106_01,-hv009,-hv025) %>%
  rename(mobile=hv243a,
         num_under5=hv014,
         sex_head=hv219,
         age_head=hv220) %>%
  removeAttributes()

demo_avg <- function(name,select_var,select_val,var,group=NULL) {
  v2015 <- mean(demo[demo[,select_var]==select_val,var],na.rm=TRUE)
  v2010 <- mean(demo_2010[demo_2010[,select_var]==select_val,var],na.rm=TRUE)
  data.frame(name=name,v2010=v2010,v2015=v2015,group=group)
}

demo_adoption <- demo_avg('urban','urban',TRUE,'mobile',1) %>%
  rbind(demo_avg('rural','urban',FALSE,'mobile',1)) %>%
  rbind(demo_avg('< primary ed','no_ed',TRUE,'mobile',2)) %>%
  rbind(demo_avg('>= primary ed','primary_ed',TRUE,'mobile',2)) %>%
  rbind(demo_avg('fewer people','peeps_lt4',TRUE,'mobile',3)) %>%
  rbind(demo_avg('more people','peeps_lt4',FALSE,'mobile',3)) %>%
  rbind(demo_avg('married','married',TRUE,'mobile',4)) %>%
  rbind(demo_avg('unmarried','married',FALSE,'mobile',4)) 

plotme <- melt(demo_adoption,id.vars=c('name','group')) %>%
  mutate(x=ifelse(variable=='v2010',0,1))
plotme$group <- as.factor(plotme$group)
ggplot(plotme,aes(x=x,y=value,group=name,color=group,label=name)) +
  geom_point(size=5) +
  geom_line(size=3) +
  scale_color_brewer(palette='Accent') +
  geom_text(hjust=1,nudge_x=-0.03,color='gray50') +
  scale_x_continuous(limits=c(-0.5,1)) +
  theme(title = element_blank(), axis.title = element_blank(), 
        axis.text.x = element_blank(), axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
        panel.grid = element_blank(), panel.background = element_blank(), 
        plot.background = element_blank(), legend.position = "none")

# Adoption has grown for all demographics; the largest gaps are due to
# income and education.

###############################################################################
# Show adoption rates for a single asset across wealth quintiles.
###############################################################################

wm2010 <- hh2010 %>%
  select(hv243a,hv270) %>%
  group_by(hv270) %>%
  summarise(m2010=mean(hv243a,na.rm=TRUE))
wm <- hh_all %>%
  select(hv243a,hv270) %>%
  group_by(hv270) %>%
  summarise(m2015=mean(hv243a,na.rm=TRUE)) %>%
  join(wm2010,by='hv270') %>% 
  rename(wquint=hv270) %>%
  mutate(m2020 = 2*m2015 - m2010) %>%
  melt(id.vars='wquint') %>%
  mutate(x=ifelse(variable=='m2010',2010,
                  ifelse(variable=='m2015',2015,2020)))
wm[wm$value > 1,'value'] <- 1 # growth stops at 100%

ggplot(wm,aes(x=x,y=value,group=wquint,color=wquint)) +
  geom_point(size=5) +
  geom_line(size=3) +
  scale_color_distiller(palette='Spectral') +
  scale_x_continuous(breaks=c(2010,2015,2020)) +
  ggtitle('Mobile ownership by wealth quintile (projected)') +
  theme(axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
        panel.grid = element_blank(), panel.background = element_blank(), 
        plot.background = element_blank(), legend.position = "none")
# Ownership goes up across all wealth quintiles, but it appears the poorest 20%
# are being somewhat left behind; everyone else looks on track to converge at
# very high saturation shortly after 2020.
# World Bank data suggests that the growth in the number of mobile 
# subscriptions has been linear since about 2007; continued linear growth out
# to 2020 (at least among populations not near saturation) seems reasonable.

# TODO: make the lines thin/dotted between 2015 and 2020

# Choropleth map of changes in asset adoption rates

# I don't have access to the GPS dataset for 2010, so I'll have to get
# locations from hh_2010. There must be a more elegant way to do this.
key10 <- attr(hh2010$shdistr,'labels')
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
cap_names10 <- sapply(names(key10),simpleCap)
dist_2010 <- cap_names10[match(hh2010$shdistr,key10)] %>% as.vector()

tmp2010 <- data.frame(district=dist_2010,mobile=assets2010$mobile) %>%
  group_by(district) %>%
  summarise(m2010 = mean(mobile,na.rm=TRUE)) %>%
  mutate(district=as.character(district))
dist_mobile <- data.frame(cluster_num=hh_all$hv001,mobile=hh_all$hv243a) %>%
  join(geo_clean[,c('cluster_num','district')],by='cluster_num') %>%
  group_by(district) %>%
  summarize(m2015 = mean(mobile,na.rm=TRUE)) %>%
  mutate(district=as.character(district)) %>%
  join(tmp2010,by='district') %>%
  mutate(change = m2015/m2010,
         NAME_2 = as.character(district))
 
plotme <- join(rwanda.adm2,dist_mobile,by='NAME_2')
ggplot(plotme) +
  aes(long,lat,group=group,fill=change) +
  geom_polygon() +
  geom_path(color='gray45') +
  coord_equal() +
  scale_fill_continuous(low='ivory',high='olivedrab') +
  ggtitle("Increase in mobile adoption: 2010-2015") +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
        panel.grid = element_blank(), panel.background = element_blank(), 
        plot.background = element_blank())
  
# It would be nice to be able to calculate kriged rasters for bouth 2015 and
# 2010 and calculate a ratio or difference between them, but that would 
# require the 2010 coordinates.

###############################################################################
# Export lat/lon and average of each digital indicator (for kriging)
###############################################################################

cluster_gp <- hh_clean %>%
  group_by(cluster_num) %>%
  summarise(telephone=mean(telephone,na.rm=TRUE),
            mobile=mean(mobile,na.rm=TRUE),
            computer=mean(computer,na.rm=TRUE),
            tv=mean(tv,na.rm=TRUE),
            radio=mean(radio,na.rm=TRUE),
            electricity=mean(electricity,na.rm=TRUE),
            bank=mean(bank,na.rm=TRUE)) %>%
  join(geo_clean,by='cluster_num') %>%
  select(lat,lon,telephone:bank)
 
write.csv(cluster_gp,'GIS/mobile-vars.csv',row.names=FALSE)

###############################################################################
# Compare DHS indicators with WB data
###############################################################################

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


