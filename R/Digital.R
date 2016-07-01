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

adm2_map(hh_clean,'wealth')
# So the concentration of some goods in Kigali could have as much to do with
# household wealth as with infrastructure. Separating the two out might
# take some work.

###############################################################################
# What determines who owns a mobile phone?
###############################################################################

# Which assets are most predictive of mobile phone ownership?
# TODO: Depending on how the code to put together hh_clean ends up looking,
#       it might not be necessary to create a new data frame here; I could 
#       just use hh_clean.
assets <- hh_all %>% 
  select(hv205:hv212,hv213:hv215,hv221,hv227,hv242:hv244,hv246,hv247,
         sh110g,sh118f) %>%
  mutate(
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
  select(hv206:hv212,hv221:nice_roof)

nrow(na.omit(assets)) / nrow(assets)
# Only 24.6% contain no missing values; we'll need to do some imputation here

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

# For each asset, what is the wealth index level at which people are
# more likely to own one than not to? Do these rankings change between
# 2010 and 2014?

assets$wealth <- hh_all$hv271
ggplot(assets,aes(wealth,hv243a)) +
  geom_jitter(size=4,color='tomato',alpha=0.1,width=0,height=0.4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic() +
  ylab('Has mobile phone')

# Be more systematic
good_assets <- ldply(names(select(assets,-wealth)),function(n) {
  label <- n
  if (n %in% hh_labels$var) {
    label <- hh_labels[hh_labels$var==n,'varDescrip']
  }
  f <- paste(n,' ~ wealth',collapse='') %>% as.formula()
  s <- glm(f,family=binomial(link='logit'),data=assets) %>% summary()
  w50 <- -s$coefficients[1,1]/s$coefficients[2,1]
  in_range <- w50 > min(assets$wealth) & w50 < max(assets$wealth)
  haves <- assets[assets[,n]==1,'wealth']
  have_nots <- assets[assets[,n]==0,'wealth']
  pos <- mean(haves,na.rm=TRUE) > mean(have_nots,na.rm=TRUE)
  data.frame(label=label,n=n,in_range=in_range,pos=pos,w50=w50)
}) %>%
  filter(in_range & pos) %>%
  select(label,n,w50) %>%
  mutate(x=0,w50=w50/1e5) %>%
  arrange(w50)
good_assets

# So it looks like, once people get a little bit of money, the first thing
# they invest in is a corrugated metal roof, and the next is a mobile phone.

y_nudges <- c(0,0,0,0,0,0,-0.04,0.04,0,-0.05,0.05,0.02)
ggplot(good_assets,aes(x=x,y=w50,label=label)) +
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
# TODO: Maybe we should copy the initial data cleaning code so that we
# get a 2010 data frame that is cleaned according to (almost) the same
# protocol and we're not loading in new data here.

# TODO use the na_if() function in the new dplyr.

library(haven)
hh2010 <- read_dta('Datain/RW_2010_DHS/rwhr61dt/RWHR61FL.DTA')
hh2010_labels = pullAttributes(hh2010) %>% 
  mutate(module = 'hh', rowNum = row_number())
assets2010 <- hh2010 %>% 
  select(hv205:hv212,hv213:hv215,hv221,hv227,hv242:hv244,hv246,hv247,
         sh110g,sh118f,hv271) %>%
  mutate(
    # fix NAs
    hv206 = ifelse(hv206==9,NA,hv206),
    hv207 = ifelse(hv207==9,NA,hv207),
    hv208 = ifelse(hv208==9,NA,hv208),
    hv209 = ifelse(hv209==9,NA,hv209),
    hv210 = ifelse(hv210==9,NA,hv210),
    hv211 = ifelse(hv211==9,NA,hv211),
    hv212 = ifelse(hv212==9,NA,hv212),
    hv221 = ifelse(hv221==9,NA,hv221),
    hv227 = ifelse(hv227==9,NA,hv227),
    hv242 = ifelse(hv242==9,NA,hv242),
    hv243a = ifelse(hv243a==9,NA,hv243a),
    hv243b = ifelse(hv243b==9,NA,hv243b),
    hv243c = ifelse(hv243c==9,NA,hv243c),
    hv243d = ifelse(hv243d==9,NA,hv243d),
    hv247 = ifelse(hv247==9,NA,hv247),
    sh110g = ifelse(sh110g==9,NA,sh110g),
    sh118f = ifelse(sh118f==9,NA,sh118f),
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
  select(hv206:hv212,hv221:nice_roof) %>%
  removeAttributes()

# Compare adoption rates for 2010 and 2014
# TODO: I'm not sure exactly how to adjust these for sampling weights

adoption_plot <- function(q) {
  plotme <- good_assets %>% select(label,n) %>%
    adply(1,function(x) {
      v <- as.character(x$n)
      mutate(x,
        mean_10 = mean(assets2010[hh2010$hv270 %in% q,v],na.rm=TRUE),
        mean_15 = mean(assets[hh$hv270 %in% q,v],na.rm=TRUE)
      )
    }) %>%
    melt(id.vars=c('label','n')) %>%
    mutate(x=ifelse(variable=='mean_10',0,1))
  ggplot(plotme,aes(x,value,group=label,label=label)) +
    geom_point(aes(color=label),size=5) +
    geom_line(aes(color=label),size=3,alpha=0.6) +
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
# like flush toilets are still pretty rare.

# Which household demographics are most predictive of mobile ownership?

# NOTE: hv106-9 all measure education level with slightly different categories
# Choose the one that gets me the best AIC.

demo <- hh_all %>%
  select(hv243a,hv009,hv014,hv025,hv219,hv220,hv106_01,hv115_01) %>%
  mutate(married = hv115_01 == 1) %>%
  select(-hv115_01)

fit_demo <- glm(hv243a ~ .,family=binomial(link='logit'),data=demo)
summary(fit_demo)


# Strongest correlations:
#    hv106_01 - more educated HoH
#    hv025 - urban 
#    hv009 - more people in house
#    hv014 - fewer kids
#    hv115_01==1 married
#    hv220 - younger HoH
#    hv219 - male HoH - not significant!

# Compare with 2010 -- how does adoption growth differ among demographics?

demo_2010 <- hh2010 %>%
  select(hv243a,hv009,hv014,hv025,hv219,hv220,hv106_01,hv115_01) %>%
  mutate(married = hv115_01 == 1,
         no_ed = hv106_01 == 0,
         primary_ed = hv106_01 > 0 & hv106_01 < 9,
         peeps_lt4 = hv009 < 4) %>%
  select(-hv115_01) %>%
  select(-hv106_01) %>%
  removeAttributes()
# TODO: Move this up to original definition of demo
demo <- demo %>%
  mutate(peeps_lt4 = hv009 < 4,
         no_ed = hv106_01 == 0,
         primary_ed = hv106_01 > 0) %>%
  select(-hv106_01)


demo_avg <- function(name,select_var,select_val,var,group=NULL) {
  v2015 <- mean(demo[demo[,select_var]==select_val,var],na.rm=TRUE)
  v2010 <- mean(demo_2010[demo_2010[,select_var]==select_val,var],na.rm=TRUE)
  data.frame(name=name,v2010=v2010,v2015=v2015,group=group)
}

demo_adoption <- demo_avg('urban','hv025',1,'hv243a',1) %>%
  rbind(demo_avg('rural','hv025',2,'hv243a',1)) %>%
  rbind(demo_avg('< primary ed','no_ed',TRUE,'hv243a',2)) %>%
  rbind(demo_avg('>= primary ed','primary_ed',TRUE,'hv243a',2)) %>%
  rbind(demo_avg('fewer people','peeps_lt4',TRUE,'hv243a',3)) %>%
  rbind(demo_avg('more people','peeps_lt4',FALSE,'hv243a',3)) %>%
  rbind(demo_avg('married','married',TRUE,'hv243a',4)) %>%
  rbind(demo_avg('unmarried','married',FALSE,'hv243a',4)) 

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

# TODO: Refactor this as a function that can create a slopeplot like this
# for other assets, not just mobile adoption.

# TODO: choropleth map of changes in asset adoption rates


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


