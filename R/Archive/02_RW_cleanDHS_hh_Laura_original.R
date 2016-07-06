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
         
         num_hh = hv012, # de jure members
         num_females15_49 = hv010, # number of 'eligible' women: women 15-49 who slept in house previous night (irrespective of whetehr live or visit)
         num_males = hv011, # number of 'eligible' men: men 15-49 who slept in house previous night (irrespective of whetehr live or visit)
         num_under5 = hv014,
         
         hh_weight = hv005,
         male_weight = hv028,
         sample_strata = hv022,
         
         interview_month = hv006,
         interview_year = hv007,
         interview_cmc = hv008, # interview in century-month-code
         interview_day = hv016,
         interviewer_id = hv018,
         
         province_id = hv024,
         urban = hv025
         
         ) %>% 
  mutate(
    # -- Convert sampling rates to appropriate value --
    
    # -- Recode --
    urban = ifelse(urban == 1, TRUE, # recode rural category to be binary (1 if rural, 0 if urban)
                        ifelse(urban == 2, FALSE,
                               NA)),
    rural = !urban
    )


# WASH indicators ---------------------------------------------------------

binary10 <- function(x) {
  ifelse(x==1,TRUE,ifelse(x==0,FALSE,NA))
}

hh_wash <- hh %>%
  select(
    water_source = hv201,         # source of drinking water
    water_treat = hv237,          # anything done to treat water?
    water_treat_boil = hv237a,    # usually treat water by boiling?
    water_treat_bleach = hv237b,  # usually treat water with bleach/chlorine?
    water_treat_cloth = hv237c,   # usually treat water by straining through 
                                  # cloth?
    water_treat_filter = hv237d,  # usually treat water with a filter?
    water_treat_solar = hv237e,   # usually treat water by solar disinfection?
    water_treat_settle = hv237f,  # usually treat water by letting stand and 
                                  # settle?
    container_wash = sh106c,      # frequency of washing water containers
    
    handwashing_site = hv230a,    # type of handwashing site
    handwashing_water = hv230b,   # water observed at handwashing site?
    has_soap = hv232,             # was soap/detergent observed?
    
    toilet_type = hv205,          # type of toilet facility
    toilet_clean_dry = sh109aa,   # toilet dry and clean
    toilet_clean_urine = sh109ab, # toilet has urine and excreta
    toilet_clean_flies = sh109ac  # toilet has flies
  ) %>% mutate(
    # -- Recode "don't know" answers as NA --
    water_treat = binary10(water_treat),
    water_treat_boil = binary10(water_treat_boil),
    water_treat_bleach = binary10(water_treat_bleach),
    water_treat_cloth = binary10(water_treat_cloth),
    water_treat_filter = binary10(water_treat_filter),
    water_treat_solar = binary10(water_treat_solar),
    water_treat_settle = binary10(water_treat_settle),
    container_wash = ifelse(container_wash < 8,container_wash,NA)
  )
attr(hh_wash$water_treat,'label')        <- attr(hh$hv237,'label')
attr(hh_wash$water_treat_boil,'label')   <- attr(hh$hv237a,'label')
attr(hh_wash$water_treat_bleach,'label') <- attr(hh$hv237b,'label')
attr(hh_wash$water_treat_cloth,'label')  <- attr(hh$hv237c,'label')
attr(hh_wash$water_treat_filter,'label') <- attr(hh$hv237d,'label')
attr(hh_wash$water_treat_solar,'label')  <- attr(hh$hv237e,'label')
attr(hh_wash$water_treat_settle,'label') <- attr(hh$hv237f,'label')


hh_clean <- cbind(hh_clean,hh_wash)



# Exploratory descriptive stats -------------------------------------------

# -- distribution of sampling dates --
# hh_clean %>% 
#   group_by(interview_month) %>% 
#   summarise(n())

ggplot(hh_clean, aes(x = interview_month)) + 
  stat_bin(binwidth=1, fill = 'dodgerblue', colour = 'white') +   
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.2) + 
  scale_x_continuous(limits = c(0, 13),
                     breaks = seq(0,12, by = 2)) +
  theme_bw() + 
  ylab('') +
  ggtitle('Number of households interviewed by month')
