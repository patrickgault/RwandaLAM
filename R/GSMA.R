# Load mobile usage statistics from GSMA Intelligence

# Assumes that 01_RW_cleanDHS.R has already been called and packages loaded

# GSMA's spreadsheet export is really beautiful to look at in Excel, but I have
# better things to do with my time than try to turn it into tidy data in R. 
# (For now at least). Import the version that I cut and pasted by hand.

q_to_num <- function(s) {
  # convert strings into fractional years
  # q_to_num('Q4 2000') = 2000.75
  yr <- gsub('^Q[1-4] ','',s) %>% as.numeric
  q <- gsub(' [0-9]{4}$','',s) %>%
    gsub('Q1',0,.) %>%
    gsub('Q2',0.25,.) %>%
    gsub('Q3',0.5,.) %>%
    gsub('Q4',0.75,.) %>%
    as.numeric
  yr + q
}

# Need population from WDI so I can convert number of unique
# subscriptions to saturation rate.

pop <- WDI(country='RW',indicator='SP.POP.TOTL',start=2000,end=2015) %>% 
  na.omit() %>%
  mutate(pop=SP.POP.TOTL) %>%
  select(pop,year) 

# linear fit for extrapolation beyond 2015
coef <- lm(data=filter(pop,year>2010),pop ~ year)$coef

gsma <- read_excel('Datain/GSMA/cutpaste_0722.xlsx') %>%
  mutate(qnum=q_to_num(quarter),
         pop=approx(x=pop$year,y=pop$pop,xout=qnum)$y,
         pop=ifelse(!is.na(pop),pop,coef[1] + coef[2]*qnum),
         rate=total_unique_subs / pop)

###### sanity checks
# ggplot(gsma,aes(x=qnum,y=pop)) +
#   geom_line() +
#   geom_point() +
#   geom_point(data=pop,aes(x=year,y=pop),size=4,
#              color='cornflowerblue',alpha=0.5) +
#   theme_classic()
# 
# qplot(gsma$qnum,gsma$rate)

rm(pop,coef,q_to_num)
