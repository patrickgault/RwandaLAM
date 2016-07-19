# Simple plotting functions for data exploration --------------------------
# Craig Jolley, USAID GeoCenter, cjolley.usaid@gmail.com
# 20 June 2016
# (c) 2016 via MIT License

library(maptools)
library(plyr)
library(dplyr)
library(ggplot2)
library(llamar)

###############################################################################
# Bar plot for categorical variables
###############################################################################
categ_bars <- function(df,my_var) {
  t <- table(df[,my_var]) %>% as.data.frame(.,stringsAsFactors=FALSE)
  t$Var1 <- as.integer(t$Var1)
  #lab <- attr(df[1,var],'label')   # No longer works
  tmp <- hh_labels_tokeep %>% 
    filter(renamedVar==my_var) %>%
    na.omit() %>%
    join(hh_labels,by='var')
  lab <- tmp[1,'varValues'][[1]] 
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
# TODO: This doesn't work anymore now that we've lost labels
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
adm2_map <- function(df,x,low_color='ivory',high_color='olivedrab') {
  tmp <- df[,c('cluster_num',x)]
  names(tmp)[2] <- 'val'
  adm2_avg <- tmp %>% 
    join(geo_clean,by='cluster_num') %>%
    group_by(district) %>%
    dplyr::summarise(val=mean(val,na.rm=TRUE)) %>%
    mutate(NAME_2=as.character(district)) 
  plotme <- join(rwanda.adm2,adm2_avg,by='NAME_2')
  ggplot(plotme) +
    aes(long,lat,group=group,fill=val) +
    geom_polygon() +
    geom_path(color='gray45') +
    coord_equal() +
    scale_fill_continuous(low=low_color,high=high_color) +
    ggtitle(attr(df[,x],'label')) +
    theme(axis.title = element_blank(), 
          axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
          panel.grid = element_blank(), panel.background = element_blank(), 
          plot.background = element_blank())
}

# TODO: Need a good way of mapping categorical variables, either with small
# multiples or majority coloring.