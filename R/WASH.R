source('loadRwDHS.R')

###############################################################################
# Simple visualization functions
###############################################################################
categ_bars <- function(df,labels,var) {
  t <- table(df[,var]) %>% as.data.frame(.,stringsAsFactors=FALSE)
  t$Var1 <- as.integer(t$Var1)
  lab <- labels[labels$var == var,'varValues'][[1]]
  t$label <- names(lab)[match(t$Var1,lab)]
  t[is.na(t$label),'label'] <- as.character(t[is.na(t$label),'Var1'])
  print(t)
  ggplot(t,aes(x=label,y=Freq)) +
    geom_bar(stat='identity') + 
    coord_flip()   
}


multi_var_bars <- function(df,labels,vars) {
  lab <- sapply(vars, function(x) labels[labels$var==x,'varDescrip']) %>%
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
# hv201 - source of drinking (and non-drinking) water
categ_bars(hh,hh_labels,'hv201')
# Most prevalent are protected springs, public taps/standpipes, unprotected 
# springs

# hv205 - type of toilet facility
categ_bars(hh,hh_labels,'hv205')
# Pit latrines with and without slabs are the most popular

# hv230a - place where hands are washed
categ_bars(hh,hh_labels,'hv230a')
# Most common for handwashing site not to be observed in dwelling

# hv230b - presence of water at hand-washing place
mean(hh$hv230b,na.rm=TRUE)  #  54.7% have water

# hv232 - items present: soap/detergent
mean(hh$hv232,na.rm=TRUE)  #  54% have soap

# hv235 - location of source for water
categ_bars(hh,hh_labels,'hv235')
# This one is probably too uniform to be very useful

# hv237 (a-f) - water purification methods
multi_var_bars(hh,hh_labels,
               c('hv237','hv237a','hv237b','hv237c','hv237d','hv237e','hv237f'))

# hv225 - share toilet with other households
mean(hh$hv225,na.rm=TRUE)  #  23.3% share

# hv238 - number of households sharing toilet
categ_bars(hh,hh_labels,'hv238')
# among those who share, most share with only one other household

# sh106c - frequency of washing water containers in a week
categ_bars(hh,hh_labels,'sh106c')

# sh109aa-c - cleanliness of toilet facility
multi_var_bars(hh,hh_labels,c('sh109aa','sh109ab','sh109ac'))
