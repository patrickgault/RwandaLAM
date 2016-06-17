# Cleanup women's dataset for Rwanda DHS data --------------------------
# Nada Petrovic, USAID PPL & GeoCenter, npetrovic@usaid.gov
# 16 June 2016
# (c) 2016 via MIT License

## Reads in excel spreadsheet that states which variables to keep
## from kids data set
women_labels_tokeep<-read_excel('Excel/women_labels_tokeep.xlsx')

## Relabels "NA" values (ie variables that have not been decided on yet) as 0
## so that they are not selected. From the Excel spreadsheet pulls the list 
## of variables to keep and what they should be renamed.
women_labels_tokeep$Keep[is.na(women_labels_tokeep$Keep)]<-0
data_subset_vars<-women_labels_tokeep$var[women_labels_tokeep$Keep==1] 
data_rename_vars<-women_labels_tokeep$renamedVar[women_labels_tokeep$Keep==1] 

## Creates new clean data frame that is a subset of the overall data frame, 
## and renames the variables.
women_clean<-women_all[data_subset_vars]
names(women_clean)<-data_rename_vars






