# Simple plotting functions for data exploration --------------------------
# Craig Jolley, USAID GeoCenter, cjolley.usaid@gmail.com
# 20 June 2016
# (c) 2016 via MIT License

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
make_adm2_avg <- function(df,x) {
  tmp <- df[,c('cluster_num',x)]
  names(tmp)[2] <- 'val'
  tmp %>% 
    plyr::join(geo_clean,by='cluster_num') %>%
    group_by(district) %>%
    dplyr::summarise(val=mean(val,na.rm=TRUE)) %>%
    mutate(NAME_2=as.character(district))   
}

make_map <- function(avg_df,low_color,high_color,title='') {
  plotme <- plyr::join(rwanda.adm2,avg_df,by='NAME_2')
  ggplot(plotme) +
    aes(long,lat,group=group,fill=val) +
    geom_polygon() +
    geom_path(color='gray45') +
    coord_equal() +
    scale_fill_continuous(low=low_color,high=high_color) +
    ggtitle(title) +
    theme(axis.title = element_blank(), 
          axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
          panel.grid = element_blank(), panel.background = element_blank(), 
          plot.background = element_blank())
}

adm2_map <- function(df,x,low_color='ivory',high_color='olivedrab') {
  adm2_avg <- make_adm2_avg(df,x)
  make_map(adm2_avg,low_color,high_color)
}

###############################################################################
# Map showing the gap in one particular indicator between two data frames
###############################################################################
gap_map <- function(df1,df2,x,low_color='ivory',high_color='indianred4',
                    title='') {
  avg1 <- make_adm2_avg(df1,x) %>% dplyr::rename(val1=val)
  avg2 <- make_adm2_avg(df2,x) %>% dplyr::rename(val2=val)
  j <- join(avg1,avg2,by='NAME_2') %>%
    mutate(val=val1-val2) %>% 
    select(NAME_2,val)
  make_map(j,low_color,high_color,title)
}
  

# TODO: Need a good way of mapping categorical variables, either with small
# multiples or majority coloring.