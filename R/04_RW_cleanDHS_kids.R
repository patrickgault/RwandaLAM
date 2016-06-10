## Extracts household variables using phrase in hh_labels

#mutate(kids_clean,kidid=paste(trimws(kids_clean$caseid),kids_clean$midx))

#kids_clean$kidid<-paste(trimws(kids_all$caseid),kids_all$midx)

#kids_clean = kids_all %>% 
 # select(caseid = as.character(kids_labels$var[kids_labels$varDescrip=="case identification"])) 

## All of the variables and what they should be renamed to
var_subset<-c(
   "caseid = case identification",
   "wealth_index = wealth index",
   "cluster_number = cluster number")

var_subset_descrip<-gsub(".*= ", "", var_subset)
var_subset_rename<-gsub(" =.*", "", var_subset)

## Check loop if it exists & if there are duplicates
if(var_subset_descrip)

data_all<-kids_all
data_labels<-kids_labels

kids_clean<-data_all[as.character(data_labels$var[data_labels$varDescrip %in% var_subset_descrip])]
names(kids_clean)<-var_subset_rename

