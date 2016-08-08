#' Import Rwanda EICV3 and EICV4.
#'
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

baseDir = '~/Documents/USAID/Rwanda/rawdata/RW_2013-14_EICV4/'


# load appropriate packages -----------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)
library(tidyr)
library(lubridate)
library(llamar)


# Import in the raw data --------------------------------------------------
hh = read_sav(paste0(baseDir, 'cs_s0_s5_household.sav'))
