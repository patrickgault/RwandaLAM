# Notes: midx and bidx are the same thing
# bidx is the line number on which the information was recorded, starting with the youngest child.  
# so bidx=1 is youngest child recorded, bidx=2 is the 2nd youngest child recorded etc.
# bord is the birth order counting from the first born, so if there are 6 children total, bord=6 refers to 
# the youngest, bord=5 refers to the second youngest etc.  
# Note: while bidx and bord are reverses of eachother in principle, in practice children above 5 years of 
# age are not recorded, so you may have only 1 entry from a given household with bidx=1 and bord=6, which means there
# is only one child below 5 and they were the 6th born
# Note: haven't dealt with twins yet


#Dependent Variable is "height_age_zscore"

#Independent Variables are:
# child birth intervals: birth_interval_preceding
# mothers' education: mother_highest_education_level
# child's sex: sex
#



