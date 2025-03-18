**********************************
**********************************
********* MASTER.DO FILE *********  
**********************************
**********************************

***** PART 0 - SET THE WORKING DIRECTORY *****

clear all 

global path /*"Insert filepath to where .zip was downloaded"*/ "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"

cd "$path"

***** PART I - CLEANING AND DATA PREP *****

do "Code/Cleaning.do" 

***** PART II - DESCRIPTIVES *****

***** PART III - ANALYSIS *****



