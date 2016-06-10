source('loadRwDHS.R')

###############################################################################
# I find myself needing this a lot
###############################################################################
get_label <- function(df,labels,var,value) {
  lab <- labels[labels$var == var,'varValues'][[1]]
  names(lab)[which(lab==value)]
}

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

###############################################################################
# Explore the WASH-relevant variables in kids
###############################################################################

# v116 - type of toilet facility
categ_bars(kids,kids_labels,'v116')
# same basic story as in hh

# v113 - source of drinking water
categ_bars(kids,kids_labels,'v113')

# v465 - disposal of youngest child's stool
categ_bars(kids,kids_labels,'v465')

###############################################################################
# Correlations with height-age z-scores?
###############################################################################

tmp <- kids_clean[,c('v113','hw5')]
tmp[tmp==9998] <- NA
tmp <- na.omit(tmp)

df <- ddply(tmp,'v113',summarise,avg_z=mean(hw5))
df$label <- sapply(df$v113, function(x) get_label(kids,kids_labels,'v113',x))
df$pval <- sapply(df$v113, function(x) 
  t.test(tmp[tmp$v113==x,'hw5'],tmp[tmp$v113!=x,'hw5'])$p.value)
df$sig <- df$pval < 0.05/nrow(df)
ggplot(df,aes(x=label,y=avg_z)) +
  geom_bar(stat='identity',aes(fill=sig)) + 
  geom_hline(yintercept = mean(tmp$hw5)) +
  coord_flip()

# So kids who get their water from a protected or 
# unprotected spring are more stunted than average, while
# those who have it piped into their yard or dwelling
# are less stunted than average.
