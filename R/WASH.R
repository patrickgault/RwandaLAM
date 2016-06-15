source('R/01_RW_cleanDHS.R')

###############################################################################
# Simple visualization functions
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

# hv232 - items present: soap/detergent
mean(hh_clean$has_soap,na.rm=TRUE)  #  54% have soap

# water purification methods
multi_var_bars(hh_clean,
               c('water_treat','water_treat_boil','water_treat_bleach',
                 'water_treat_filter','water_treat_solar','water_treat_settle'))

# sh106c - frequency of washing water containers in a week
categ_bars(hh_clean,'container_wash')

# sh109aa-c - cleanliness of toilet facility
multi_var_bars(hh_clean,c('toilet_clean_dry','toilet_clean_urine','toilet_clean_flies'))

