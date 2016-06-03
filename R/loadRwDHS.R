#' Import Rwanda DHS 2014/2015 data
#'
#' Imports data from the 2014/2015 Demographic and Health Surveys from Rwanda
#'
#' DHS modules are as follows:
#' (from http://dhsprogram.com/data/Dataset-Types.cfm)
#' * HR: household data (household level description)
#' * PR: household list (inidividual roster)
#' * IR: women's module (one woman/hh)
#' * MR: men's module (one man/hh)
#' * CR: couple's module
#' * KR: children's module (ea. child < 5 y old)
#' * BR: birth data (birth history of all children) --> fertility / mortality rates
#' * WI: wealth index data (for data before 1990 to calcualte wealth index)
#'
#' @author Laura Hughes, lhughes@usaid.gov
#'
#' @import dplyr
#' @import haven
#' @import llamar
#' @importFrom magrittr "%>%"
#'
#' @export
#'

loadRwDHS = function(user = 'Laura',
                     path = NA) {
  
  # Set up the working directory where the DHS data are locally stored.
  if (!user %in% c('Laura', 'Tim', 'Nada', 'Patrick')){ # User name isn't recognized
    if(is.na(path)) {
      stop("Path containing data isn't specified.")
    } else {
      path = path # Manually set path
    }
  } else if(user == 'Laura') {
    path = '~/Github/Rwanda/'
  } else if(user == 'Tim') {
    path = '~/Users/Tim/Rwanda/'
  } else if(user == 'Nada') {
    path = '~/Documents/USAID/Rwanda/'
  } else if(user == 'Patrick') {
    path = '~/Documents/USAID/Rwanda/'
  }
  
  
  # Import raw data ---------------------------------------------------------
  
  hh = read_dta(paste0(path, 'data in/RW_2014-15_DHS/rwhr70dt/RWHR70FL.DTA'))
  birth = read_dta(paste0(path, 'data in/RW_2014-15_DHS/rwbr70dt/RWBR70FL.DTA'))
  couples = read_dta(paste0(path, 'data in/RW_2014-15_DHS/rwcr70dt/RWCR70FL.DTA'))
  women = read_dta(paste0(path, 'data in/RW_2014-15_DHS/rwir70dt/RWIR70FL.DTA'))
  kids = read_dta(paste0(path, 'data in/RW_2014-15_DHS/rwkr70dt/RWKR70FL.DTA'))
  men = read_dta(paste0(path, 'data in/RW_2014-15_DHS/rwmr70dt/RWMR70FL.DTA'))
  couples = read_dta(paste0(path, 'data in/RW_2014-15_DHS/rwpr70dt/RWPR70FL.DTA'))
  
  
  
  # pull out value labels ---------------------------------------------------
  
  
  # remove value labels -----------------------------------------------------
  
  
}
