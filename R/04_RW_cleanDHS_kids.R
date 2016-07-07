# Cleanup kids dataset for Rwanda DHS data --------------------------
# Nada Petrovic, USAID PPL & GeoCenter, npetrovic@usaid.gov
# 16 June 2016
# (c) 2016 via MIT License

## Reads in excel spreadsheet that states which variables to keep
## from kids data set
kids_labels_tokeep<-read.csv('Excel/kids_labels_tokeep.csv')

## Relabels "NA" values (ie variables that have not been decided on yet) as 0
## so that they are not selected. From the Excel spreadsheet pulls the list 
## of variables to keep and what they should be renamed.
kids_labels_tokeep$Keep[is.na(kids_labels_tokeep$Keep)] <- 0
data_subset_vars <- kids_labels_tokeep$var[kids_labels_tokeep$Keep==1] 
data_rename_vars <- kids_labels_tokeep$renamedVar[kids_labels_tokeep$Keep==1] 


## Creates new clean data frame that is a subset of the overall data frame, 
## and renames the variables.
kids_clean <- kids_all[data_subset_vars]
names(kids_clean) <- data_rename_vars


## Creates household id variable for merging with hh data set, note
## this is not the same as hhid due to weirdness in spacing of DHS
## paste job.  It can only be merged with a homemade household id like
## this one.
kids_clean$cluster_hh_num <- paste(kids_clean$cluster_num, kids_clean$hh_num)


## Replace all 9998 values with NA
kids_clean <-kids_clean %>% 
  mutate(height_age_zscore = replace(height_age_zscore,
                                     height_age_zscore==9998, NA))
## The zscore needs to be divided by 100
kids_clean$height_age_zscore <- (kids_clean$height_age_zscore / 100)

### Calculate age by subtracting 
kids_clean$age_calc_months <- (kids_clean$interview_date_cmc - kids_clean$dob_cmc)

# Check z-score versus age
library(ggthemes) 
# No noticable difference between boys and girls stunting, does peak around 20 months
kids_clean %>% mutate(stunted = ifelse(height_age_zscore <= -2, 1, 0)) %>%
  ggplot(aes(x = age_calc_months, y = stunted, colour = factor(sex))) +
  #geom_jitter(width = 0.25, height = 0.25) +
  stat_smooth(method = "loess", se = TRUE, span = 0.75, size = 1.15, alpha = 0.1) + 
  theme_fivethirtyeight() + ggtitle("stunting appears to peak near 20 months")

# Steady downward trend for stunting and wealth, not surprising
kids_clean %>% mutate(stunted = ifelse(height_age_zscore <= -2, 1, 0)) %>%
  ggplot(aes(x = wealth_index, y = stunted, colour = factor(sex))) +
  #geom_jitter(width = 0.25, height = 0.25) +
  stat_smooth(method = "loess", se = TRUE, span = 0.75, size = 1.15, alpha = 0.1) + 
  theme_fivethirtyeight() +
  ggtitle("stunting declines steadily with wealth (asset accumulation)")

# Finally, break it down by wealth and age category
# TODO: figure out if 1 is boy or girl!
kids_clean %>% mutate(stunted = ifelse(height_age_zscore <= -2, 1, 0), 
                      agegroup = cut(age_calc_months, seq(0, 60, by = 6))) %>%
  group_by(agegroup, wealth_index, sex) %>% 
  summarise(stunting = mean(stunted, na.rm = TRUE)) %>% 
  filter(stunting != 0) %>% 
  ggplot(aes(x = agegroup, y = wealth_index, fill = stunting)) +
  geom_tile(colour = 'white',size = 0.25, stat = "identity") +
  scale_fill_viridis(option="D") +
  geom_text(aes(y = wealth_index, x = agegroup, label = sprintf("%1.0f%%", round(100*stunting, 2)), size = 1)) +
  theme_fivethirtyeight() + facet_wrap(~sex, nrow = 2)
                        
## Note Max is 59 months, ie only children <5

## Checking if "gave child" variables vary by child, by seeing if the sum total
## of all the food variables + caseid has more uniqueness than caseid alone
child_food_tot <- rowSums(select(kids_clean, contains("child")), na.rm=TRUE)

length(unique(kids_clean$caseid))
## Output: [1] 5955

length(unique(paste(kids_clean$caseid,child_food_tot)))
## Output: [1] 5955
## It does not seem to vary per child

# Wording of question: "Now I would like to ask you about liquids or foods #that (NAME FROM 649) had yesterday during the day or at night. 
#I am interested in whether your child had the #item I mention even if it was #combined with other foods.
#Did (NAME FROM 649) drink or eat:
# Where 649 refers to question about "Youngest child living with her born #between 2013 and #2015"

kids_diet <- select(kids_clean,contains("child"), -child_other_food)

## Checking if there are rows with NAs in some but not all entries
table(rowSums(is.na(kids_diet)))
#Output:
#   0    1    3   13 
#4535   47    1 3273 

##Indeces of the rows that are all NAs
rows_allNAs <- rowSums(is.na(kids_diet)) == 13

## To avoid removing those that only have a couple missing, will put back in rows that are ## all NAs at the end
kids_diet[is.na(kids_diet)] <- 0

## Calculate WDDS

child =  child %>% 
  mutate(vitA = sum(child_veg_yellow_orange, 
                    child_fruit_vit_a, na.rm = TRUE))


## Starchy Staples
DietDiv<-(kids_diet$child_tubers+kids_diet$child_cereals)>0 

## Dark Green leafy vegetables
DietDiv<-DietDiv+kids_diet$child_veg_dark_green 

## Other Vitamin A rich fruit & veg

DietDiv<-DietDiv+((kids_diet$child_veg_yellow_orange+kids_diet$child_fruit_vit_a)>0)
## Other Fruit & Veg



DietDiv<-DietDiv+(kids_diet$child_fruit_other)
## Organ meat

DietDiv<-DietDiv+(kids_diet$child_meat_organ)
## Meat & fish

DietDiv<-DietDiv+((kids_diet$child_meat+kids_diet$child_fish)>0)
## Eggs

DietDiv<-DietDiv+kids_diet$child_eggs
## Legumes, nuts and seeds

DietDiv<-DietDiv+kids_diet$child_legumes_nuts
## Milk and Milk products

DietDiv<-DietDiv+((kids_diet$child_milk+kids_diet$child_milk_products)>0)

## Putting back in rows that are all NA
DietDiv[rows_allNAs]<-NA

#DietDiv (3273 NAs, 7856 total)
#   0    1    2    3    4    5    6    7    8 
#1446  317  614  885  679  401  182   44   15 

kids_clean$DietDiv_WDDS <- DietDiv





