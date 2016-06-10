## Extracts household variables using phrase in hh_labels

#mutate(kids_clean,kidid=paste(trimws(kids_clean$caseid),kids_clean$midx))

kids_all<-kids_clean

kids_clean$kidid<-paste(trimws(kids_all$caseid),kids_all$midx)



#select(kids_clean, kidid, everything())






#hh_labels_subset<-c(
#  "case identification -> hhid",
#   "wealth index -> wealth_index",
#   "month of interview -> interview_month","year of interview -> interview_year",
#   "age of head of household -> HH_age", "sex of head of household -> HH_sex", 
#   "province", "district",
#   "owns land usable for agriculture -> owns_land", 
#   "hectares of agricultural land (1 decimal) -> hectares_land")

#hh_subset<-hh_clean[as.character(hh_labels$var[hh_labels$varDescrip %in% gsub(" ->.*", "", #hh_labels_subset)])]
#names(hh_subset)<-gsub(".*-> ", "", hh_labels_subset)

