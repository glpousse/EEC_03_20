**********************************
**********************************
********* MASTER.DO FILE *********  
**********************************
**********************************
clear all 
***** PART 0 - SET THE WORKING DIRECTORY *****

global path /*"Insert filepath to where .zip was downloaded"*/

cd "$path"

***** PART I - PREPARING THE MERGE DATA *****

do "Code/1_merge_prep.do" 

***** PART II - PREPARING THE ANALYSIS DATA *****

do "Code/2_data_prep.do" 

***** PART III - DESCRIPTIVE ANALYSIS *****

do "Code/3_descriptive_analysis.do" 

***** PART IV - TEPA ANALYSIS *****

do "Code/4_TEPA_analysis.do"
