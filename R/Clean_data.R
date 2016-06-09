
## Extracts household variables using phrase in hh_labels

hh_labels_subset<-c(
  "case identification -> hhid",
   "wealth index -> wealth_index",
   "month of interview -> interview_month","year of interview -> interview_year",
   "age of head of household -> HH_age", "sex of head of household -> HH_sex", 
   "province", "district",
   "owns land usable for agriculture -> owns_land", 
   "hectares of agricultural land (1 decimal) -> hectares_land")

hh_subset<-hh_clean[as.character(hh_labels$var[hh_labels$varDescrip %in% gsub(" ->.*", "", hh_labels_subset)])]
names(hh_subset)<-gsub(".*-> ", "", hh_labels_subset)

