source('00_RW_loadDHS.R')
source('02_RW_cleanDHS_hh.R')
hh_clean$cluster_hh_num<-paste(hh_clean$cluster_num,hh_clean$hh_num)
source('04_RW_cleanDHS_kids.R')

# Notes about Variables:
# bidx: is the line number on which the information was recorded, starting with the youngest
#       child so bidx=1 is youngest child recorded, bidx=2 is the 2nd youngest child recorded
#       etc. Note: midx and bidx are the same thing
# bord (renamed birth_order): is the birth order counting from the first born, so if there 
#       are 6 children total, bord=6 refers to the youngest, bord=5 refers to the second 
#       youngest etc.   
#       Note: while bidx and bord are reverses of eachother in principle, in practice
#       children above 5 years of age are not recorded, so you may have only 1 entry from a
#       given household with bidx=1 and bord=6, which means there is only one child below 5 
#       and they were the 6th born
# Note: haven't dealt with twins yet











# According to DHS report pg 170 12,699 household completed the survey, which matches
# nrow(hh_all)
# Can't figure out why the kids data has the number of rows it has for the life of me!


num_kids_vec<-vector(length=length(hhdiff))
for (j in 1:length(hhdiff)){
print(j)  
num_kids_vec[j]<-hh_clean[hh_clean$cluster_hh_num==hhdiff[j],]$num_under5
}

#Dependent Variable is "height_age_zscore"

#Independent Variables are:
# child birth intervals: birth_interval_preceding
# mothers' education: mother_highest_education_level
# child's sex: sex
# child's age calculated: age_calc_months
# birth order: birth_order
# wealth index: wealth_index
# dietary diversity: DietDiv
# women's stunting

fit <- lm(height_age_zscore~birth_interval_preceding+mother_highest_education_level+sex+age_calc_months+birth_order+wealth_index+DietDiv_WDDS,data=kids_clean) 

summary(fit)

## Histogram of the age distribution of the NA values for stunting, doesn't
## seem to exhibit a pattern
hist(kids_clean$age_calc_months[is.na(kids_clean$height_age_zscore)],bins=20)

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