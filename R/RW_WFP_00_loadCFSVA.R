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


# clean children’s data ---------------------------------------------------
# stunting is based on _______


# clean mother_raw --------------------------------------------------------


# children's dataset includes lots of duplicated variables.
write.csv()


# determine what should be base -------------------------------------------
hh_raw %>% group_by(livezone) %>% summarise(num = n()) %>% arrange(desc(num))
# livelihood zone #5 is most frequent therefore will be used as base.
# zone 5 == Central Plateau Cassava and Coffee Zone 

