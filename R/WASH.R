source('R/01_RW_cleanDHS.R')

require("rgdal") 
require("maptools")
require(plyr)
require(dplyr)
require(ggplot2)
library(llamar)

###############################################################################
# Bar plot for categorical variables
###############################################################################
categ_bars <- function(df,var) {
  t <- table(df[,var]) %>% as.data.frame(.,stringsAsFactors=FALSE)
  t$Var1 <- as.integer(t$Var1)
  lab <- attr(df[1,var],'label')
  if (is.null(lab)) {
    lab <- t$Var1
    names(lab) <- as.character(lab)
  }
  t$label <- names(lab)[match(t$Var1,lab)]
  t[is.na(t$label),'label'] <- as.character(t[is.na(t$label),'Var1'])
  print(t)
  ggplot(t,aes(x=label,y=Freq)) +
    geom_bar(stat='identity') + 
    coord_flip()   
}

###############################################################################
# Show the 'yes' values of several yes/no variables all in one place
###############################################################################
multi_var_bars <- function(df,vars) {
  lab <- sapply(vars, function(x) attr(df[,x],'label')) %>%
    as.character()
  plotme <- data.frame(var=vars,label=lab,
                   val=sapply(vars,function(x) mean(df[,x]==1,na.rm=TRUE)))
  ggplot(plotme,aes(x=label,y=val)) +
    geom_bar(stat='identity') + 
    coord_flip() 
}

###############################################################################
# Admin-2 choropleth maps
###############################################################################
adm2_map <- function(df,x) {
  tmp <- df[,c('cluster_id',x)]
  names(tmp)[2] <- 'val'
  adm2_avg <- tmp %>% 
    join(geo_clean,by='cluster_id') %>%
    group_by(district) %>%
    summarise(val=mean(val,na.rm=TRUE)) %>%
    mutate(NAME_2=as.character(district)) 
  plotme <- join(rwanda.adm2,adm2_avg,by='NAME_2')
  ggplot(plotme) +
    aes(long,lat,group=group,fill=val) +
    geom_polygon() +
    geom_path(color='gray45') +
    coord_equal() +
    scale_fill_continuous(low='ivory',high='olivedrab') +
    ggtitle(attr(df[,x],'label')) +
    theme(axis.title = element_blank(), 
          axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
          panel.grid = element_blank(), panel.background = element_blank(), 
          plot.background = element_blank())
}

# TODO: Need a good way of mapping categorical variables, either with small
# multiples or majority coloring.

###############################################################################
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

