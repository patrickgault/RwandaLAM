# Cleanup kids dataset for Rwanda DHS data --------------------------
# Nada Petrovic, USAID PPL & GeoCenter, npetrovic@usaid.gov
# 16 June 2016
# (c) 2016 via MIT License

## Reads in excel spreadsheet that states which variables to keep
## from kids data set
kids_labels_tokeep<-read_excel('Excel/kids_labels_tokeep.xlsx')

## Relabels "NA" values (ie variables that have not been decided on yet) as 0
## so that they are not selected. From the Excel spreadsheet pulls the list 
## of variables to keep and what they should be renamed.
kids_labels_tokeep$Keep[is.na(kids_labels_tokeep$Keep)]<-0
data_subset_vars<-kids_labels_tokeep$var[kids_labels_tokeep$Keep==1] 
data_rename_vars<-kids_labels_tokeep$renamedVar[kids_labels_tokeep$Keep==1] 

## Creates new clean data frame that is a subset of the overall data frame, 
## and renames the variables.
kids_clean<-kids_all[data_subset_vars]
names(kids_clean)<-data_rename_vars

## Checking if "gave child" variables vary by child, by seeing if the sum total
## of all the food variables + caseid has more uniqueness than caseid alone
kids_clean$child_food_tot<-rowSums(select(kids_diet,contains("child")),na.rm=TRUE)
length(unique(kids_diet$caseid))
## Output: [1] 5955
length(unique(paste(kids_diet$caseid,kids_diet$child_food_tot)))
## Output: [1] 5955
## It does not seem to vary per child

# Wording of question: "Now I would like to ask you about liquids or foods that (NAME FROM 649) #had yesterday during the day or at night. I am interested in whether your child had the #item I mention even if it was #combined with other foods.
#Did (NAME FROM 649) drink or eat:
# Where 649 refers to question about "Youngest child living with her born between 2013 and #2015"

### Calculate age by subtracting 

kids_clean$age_calc<-kids_clean$doi-kids_clean$dob_cmc


## Calculate WDSS 



