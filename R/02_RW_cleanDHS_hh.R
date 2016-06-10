# Cleanup household dataset for Rwanda DHS data --------------------------
# Laura Hughes, USAID GeoCenter, lhughes@usaid.gov
# 10 June 2016
# (c) 2016 via MIT License

library(ggplot2)

# basic info --------------------------------------------------------------

hh_clean = hh %>% 
  select(hhid,
         cluster_id = hv001,
         hh_num = hv002,
         hh_weight = hv005,
         interview_month = hv006,
         interview_year = hv007,
         interview_cmc = hv008, # interview in century-month-code
         interview_day = hv016
         )


# WASH indicators ---------------------------------------------------------
# @cjolley: want to give it a go?
hh_clean = hh_clean


# Exploratory descriptive stats -------------------------------------------
# -- distribution of sampling dates --
# hh_clean %>% 
#   group_by(interview_month) %>% 
#   summarise(n())

ggplot(hh_clean, aes(x = interview_month)) + 
  geom_histogram(fill = 'dodgerblue') +
  theme_bw()
