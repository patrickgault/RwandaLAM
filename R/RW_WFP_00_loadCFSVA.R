#' Import Rwanda CFSVA
#'
#' Imports data from the 2014/2015 Demographic and Health Surveys from Rwanda
#'
#' @author Laura Hughes, lhughes@usaid.gov and others
#'
#' @import dplyr
#' @import haven
#' @import llamar
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#'

baseDir = '~/Documents/USAID/Rwanda/rawdata/RW_2015_CFSVA/'


# load appropriate packages -----------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)
library(tidyr)
library(lubridate)
library(llamar)


# Import in the raw data --------------------------------------------------
# Raw data is contained in three files:
# children's data: 'cfsva-2015-child-DB- annex.sav'
# household-level data:  'cfsva-2015-master-DB- annex.sav'
# women's data: 'cfsva-2015-mother-DB- annex.sav'

hh_raw = read_sav(paste0(baseDir, 'cfsva-2015-master-DB- annex.sav'))
children_raw = read_sav(paste0(baseDir, 'cfsva-2015-child-DB- annex.sav'))
mother_raw = read_sav(paste0(baseDir, 'cfsva-2015-mother-DB- annex.sav'))

ch2012 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2012_CFSVA/cfsvans-2012- children-v01.sav')
hh2012 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2012_CFSVA/cfsvans-2012- household-v01.sav')


ch2009 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2009_CFSVA/Section 13 enfants.sav')

stuntingDist12 = ch2012 %>% filter(!is.na(G_Stunted)) %>% group_by(fews_code) %>% summarise(avg = mean(G_Stunted), 
                                                                            std = sd(G_Stunted),
                                                                            num = n(),
                                                                            se = std / (sqrt(num)),
                                                                            lb = avg - ciFactor * se,
                                                                            ub = avg + ciFactor * se) %>% 
  arrange(desc(avg)) %>% 
  mutate(dist = fews_code)

# clean children’s data ---------------------------------------------------
# stunting is based on 2006 WHO children's growth standards
# stunting was calculated by WFP in SPSS.
metadata = lapply(children_raw, function(x) attr(x, 'label'))

dists = data.frame(codes = attr(children_raw$S0_D_Dist_lyr, 'labels')) %>% 
  mutate(dist = row.names(dists))


stuntingDist$dist = plyr::mapvalues(stuntingDist$dist, from = dists$codes, to = dists$dist)

livelihood_zones = data.frame(codes = attr(children_raw$livezone_lyr, 'labels')) 
livelihood_zones = livelihood_zones %>% 
  mutate(lz = row.names(livelihood_zones))

ch$livezone_lyr = plyr::mapvalues(ch$livezone_lyr, from = livelihood_zones$codes, to = livelihood_zones$lz)

# clean mother_raw --------------------------------------------------------


# children's dataset includes lots of duplicated variables.
write.csv()





# remove children without stunting scores ---------------------------------

ch = children_raw %>% filter(! is.na(Stunted_global))

ciFactor = 1.96

stuntingDist = ch %>% group_by(dist = livezone_lyr) %>% summarise(avg = mean(Stunted_global), 
                                             std = sd(Stunted_global),
                                             num = n(),
                                             se = std / (sqrt(num)),
                                             lb = avg - ciFactor * se,
                                             ub = avg + ciFactor * se) %>% 
  arrange(desc(avg))


stuntingDist = removeAttributes(stuntingDist)

stuntingDist$dist = factor(stuntingDist$dist, levels = rev(stuntingDist$dist))

ggplot(stuntingDist) +
  geom_pointrange(aes(x = dist, y = avg, ymin = lb, ymax = ub)) +
  coord_flip() +
  ylim(c(0.15, 0.72))

s = full_join(stuntingDist, stuntingDist12, by = 'dist')
ggplot(s %>% filter(dist %in% c(12, 2)), aes(x = `avg.y`, xend = `avg.x`, y = 2012, yend = 2015,
              colour = as.character(dist))) +
  geom_segment() + coord_flip() + 
  geom_text(aes(label = as.character(dist), x = `avg.y`, y = 2015), nudge_x = 0.1, size = 5)

# determine what should be base -------------------------------------------
hh_raw %>% group_by(livezone) %>% summarise(num = n()) %>% arrange(desc(num))
# livelihood zone #5 is most frequent therefore will be used as base.
# zone 5 == Central Plateau Cassava and Coffee Zone 

