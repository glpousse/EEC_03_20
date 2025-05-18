#######################
##### CHAINED DiD #####
#######################

##### Setup ##### 
rm(list = ls())
setwd("/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20")

# Libraries
library(cdid)
library(haven)

##### Treatment and Control Dummies #####
df <- read_dta("Data/Clean/SYNTH_2.dta")

# ID1 

# ID2 