*******************************
*******************************
***** SYNTHETIC VARIABLES ***** 
*******************************
*******************************
// nbheur  = nb heures correspondant au salaire declare 

/***** QUESTIONS ***** 

- Centered moving average or lagged moving average? 
- Different lowess bandwidth? I took 0.4 as a standard number.

*********************/

* Changes to add to dataprep * 
replace naf10 = "00" if naf10 == "."

gen nb_salarie = . 
replace nb_salarie = 1 if nbsala < 5
replace nb_salarie = 2 if nbsala >=5 & nbsala < 7 
replace nb_salarie = 3 if nbsala ==7 
replace nb_salarie = 4 if nbsala > 7  
lab def newlab 1 "0-19" 2 "20-199" 3 "200-499" 4 "500+"
lab values nb_salarie newlab 

***************
**# WAGES ***** 
***************
/*
	The wage variable is only reported in the first and last interrogation of the LFS. 
	To simply categorize observations by wage. I used the average wage. 
	For more advanced analysis, I need variation in the wage. I propose to generate synthetic
	values of the wage, by leveraging the rolling panel structure of the LFS. 
	
	New individuals join the panel every month, while others exit. If I successfully define cohorts 
	grouping "new-joiners" by their common characteristics, then I can compute the average wage 
	for individuals	of that cohort in a given month, and use these values replacements for missing values.
	I do not use the replacement values if they are lower than an already reported wage: this 
	assumes no individuals see their wages decrease as their employmnt span increases. A safe 
	assumption. 
	
	E.g. 
	Persons A, B and C have similar characteristics (age, job industry, education): they fall into
	the same cohort. They all report wages in their first interrogation. A enters in Jan 2007, B enters in Feb 
	2007, and C enters in March 2007. Assuming the cohort requirements are specific enough, I can collapse 
	these wages into the wage of any individual of that cohort. Now, to add precision, consider A, B 
	and C to be waves (or vectors) or "new-joiners". This is our case in the LFS.
*/

use Data/Clean/df_wedges, clear 

	* My sample
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
//  drop if salred < smic_m_net		// I can't find net smic before 2005, hard to apply this as of now. 
	
	* Generating cohorts (NEED TO FIND CLASSIFICATION TO HAVE AT LEAST 10 OBS PER COHORT PER MONTH?)
	egen cohort_id_salred = group(age_cohort  nivet), missing
save "Data/Clean/synth_df_wedges.dta", replace 
	
	* Collapsing Monthly Cohort Wage Averages -- I NEED QUARTERLY IF I AM TO USE TSSMOOTH. WHICH ONE?
use Data/Clean/synth_df_wedges, clear
	* Keeping only obs with salred 
	keep if salred !=.
	collapse (mean) salred, by(cohort_id_salred datdeb_m)
	rename salred cohort_salred
save "Data/Clean/synth_collapsed.dta", replace 
	
use Data/Clean/synth_collapsed, clear 
	* SIMPLE MOVING AVERAGE -- CENTERED MOVING AVERAGE
	tsset cohort_id_salred datdeb_m
	gen salred_CMA = (L1.cohort_salred + cohort_salred + F1.cohort_salred) / 3
	* Alternative Simple moving average -- LAGGED MOVING AVERAGE 
	tssmooth ma salred_LMA = cohort_salred, window(3)
save "Data/Clean/MA_wages.dta", replace

	* LOWESS -- Locally weighted scatterplot smoothing (COMPUTATIONALLY HEAVY -- )
use Data/Clean/synth_collapsed, clear 
	
	levelsof cohort_id_salred, local(cohorts)
	foreach c of local cohorts {
		display "Smoothing cohort `c'"
		lowess cohort_salred datdeb_m if cohort_id_salred == `c', gen(smoothed_`c') bwidth(0.4)
	}
	
	gen lowess_wage = .
	levelsof cohort_id_salred, local(cohorts)
	foreach c of local cohorts {
		replace lowess_wage = smoothed_`c' if cohort_id_salred == `c'
	}
	drop smoothed_*
save "Data/Clean/LOWESS_wages.dta", replace
	
	* Merging back onto dataset
use Data/Clean/synth_df_wedges, clear 
	order datdeb_m cohort_id_salred 
	sort datdeb_m cohort_id_salred 

	merge m:m cohort_id_salred datdeb_m using "Data/Clean/LOWESS_wages.dta"
	drop _merge
	merge m:m cohort_id_salred datdeb_m using "Data/Clean/MA_wages.dta"
	drop _merge
	order indiv_num rgi cohort_id_salred salred*
save "Data/Clean/synth_df_wedges.dta", replace 

* NEXT STEP : NORMALIZING THE EVOLUTION OF ONE OF THE IMPUTED WAGE VARS, BY INDIVIDUAL 
*			  APPLYING THE TRENDS TO INDIVIDUAL'S SALRED VALUE. 
	
***********************
**# DESIRED HOURS ***** 
***********************
/*
	The desired hours question is asked at every interrogation of the LFS. It is not consistently 
	answered. While it is harder to assume similar desired hours by cohort, the LFS provides rich
	socio-dempgraphic data which I will try to leverage. 
	
	Variables to define Cohort: 
		- Current working hours 
		- 
*/
use Data/Clean/synth_df_wedges, clear 

	* Generating cohorts (NEED TO FIND CLASSIFICATION TO HAVE AT LEAST 10 OBS PER COHORT PER MONTH?)
	egen cohort_id_hplus = group(age_cohort  nivet), missing
save "Data/Clean/synth_df_wedges.dta", replace 
	
	
	
	

	
	
	


