# Cleanup household dataset for Rwanda DHS data --------------------------
# Laura Hughes & Nada Petrovic, USAID GeoCenter & PPL, lhughes@usaid.gov
# 10 June 2016
# (c) 2016 via MIT License

library(ggplot2)

# basic info --------------------------------------------------------------

## Reads in excel spreadsheet that states which variables to keep
## from kids data set
hh_labels_tokeep<-read_excel('Excel/hh_vars_tokeep.xlsx')

## Relabels "NA" values (ie variables that have not been decided on yet) as 0
## so that they are not selected. From the Excel spreadsheet pulls the list 
## of variables to keep and what they should be renamed.
hh_labels_tokeep$Keep[is.na(hh_labels_tokeep$Keep)]<-0
data_subset_vars<-hh_labels_tokeep$var[hh_labels_tokeep$Keep==1] 
data_rename_vars<-hh_labels_tokeep$renamedVar[hh_labels_tokeep$Keep==1] 

## Creates new clean data frame that is a subset of the overall data frame, 
## and renames the variables.
hh_clean<-hh_all[data_subset_vars]
names(hh_clean)<-data_rename_vars

hh_clean %>% mutate(
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

# Digital indicators ---------------------------------------------------------

hh_digital <- hh %>%
  select(
    landline = hv221,         # has landline phone?
    mobile = hv243a,          # has mobile phone?
    computer = sh110g,        # has computer?
    tv = hv208,               # has television?
    radio = hv207,            # has radio?
    electricity = hv206,      # has electricity?
    bank = hv247)             # has bank account

hh_clean <- cbind(hh_clean,hh_digital)

# Others that might be useful from non-household data sets

# Women:
# v157 frequency of reading newspaper/magazine
# v158 frequency of listening to radio
# v159 frequency of watching television
# v743f person who usually decides what to do with money

# Men:
# mv157 frequency of reading newspaper/magazine
# mv158 frequency of listening to radio
# mv159 frequency of watching television

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
