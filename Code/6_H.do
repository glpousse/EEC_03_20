*************************
*************************
***** HETEROGENEITY *****
*************************
*************************

***** SETUP *****

use Data/Clean/SYNTH_2, clear 

 	* Sample 
	keep if in_tepa 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption 
	keep if mod_agree ==0
 	keep if min_smic

	* DiD DUMMIES
	gen treatment 		= (forfait == 2)
	gen control 		= (forfait ==1)
	drop if (treatment 	==. | control ==.) 
	gen post_treatment 	= post_tepa*treatment 


	* TD DUMMIES - OPTIM
	gen triple_optim 	= treatment * optim * post_tepa
	gen post_optim 		= post_tepa * optim
 	gen treatment_optim = treatment * optim
	* TD DUMMIS - MOTIVATION 							// Not conclusive 
	gen triple_motiv	= treatment * tplus * post_tepa
	gen post_motiv 		= post_tepa * tplus 
	gen treatment_motiv = treatment * tplus 
	* TD DUMMIES - GENDER
	gen gender 			= sexe
	gen triple_gender 	= treatment * sexe * post_tepa
	gen post_gneder		= post_tepa * sexe 
	gen treatment_gender= treatment * sexe 
	
	
	foreach var in "" "1"{ 
		
		cap drop wedge`var' 
		cap drop wedge`var'_sq 
		cap drop abs_wedge`var'
		gen wedge`var' = hplus`var' - empnbh
		gen wedge`var'_sq 	= wedge`var'^2
		gen abs_wedge`var' = abs(wedge`var')
		
		cap drop tplus`var' 
		gen tplus`var' = .
		replace tplus`var' = 1 if wedge`var' > 0 	& wedge`var' !=. 
		replace tplus`var' = 0 if wedge`var' == 0  	& wedge`var' !=. 
		replace tplus`var' = 0 if wedge`var' < 0	& wedge`var' !=. 
	}
	
	foreach var in CMA LMA lowess bspline{
		
		cap drop wedge_`var' wedge_`var'_sq abs_wedge_`var'
		gen wedge_`var' 	= hhplus_`var' - empnbh
		gen wedge_`var'_sq 	= wedge_`var'^2
		gen abs_wedge_`var' = abs(wedge_`var')
		
		cap drop tplus_`var' 
		gen tplus_`var' 	= .
		replace tplus_`var' = 1 if wedge_`var' > 0   & wedge_`var' !=. 
		replace tplus_`var' = 0 if wedge_`var' == 0  & wedge_`var' !=. 
		replace tplus_`var' = 0 if wedge_`var' < 0	 & wedge_`var' !=. 
	}
	
	
		foreach var in salred salred1 wsalred_CMA wsalred_LMA wsalred_lowess wsalred_bspline {
		cap drop weekly_`var' hourly_`var' log_`var' log_hourly_`var' `var'_sq
		gen weekly_`var'     	= `var'/ 4.33
		gen hourly_`var' 	  	= weekly_`var' / empnbh 
		gen log_`var' 	      	= log(`var')
		gen log_hourly_`var'  	= log(hourly_`var')
		gen `var'_sq = `var'^2 
	}
	
	* Making the wage tertiles 
	foreach var in "" "1" {  
		su salred`var'
		xtile tertile`var' = salred`var', nq(3)
	}

	foreach var in CMA LMA lowess bspline {
		su wsalred_`var'
		xtile tertile_`var' = wsalred_`var', nq(3)
	}
	
save "Data/Clean/H&R.dta", replace 
	
	
******************************
**# Regressions - GENDER ***** 
******************************


*** MEN ***
use Data/Clean/H&R, clear
	drop if optim ==1
	keep if sexe == 1

* OG (1)
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10 
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m1_1: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_4: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_5: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* A1 (2)
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m2_1: reghdfe hplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_3: reghdfe abs_wedge post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 											// * 
	eststo m2_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_5: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	

* CMA (3)
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m3_1: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_5: reghdfe tplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* LMA (4)
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m4_1: reghdfe hplus post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls   [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_4: reghdfe wedge_sq post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_5: reghdfe tplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)			// *

	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
										
* Lowess (5)
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m5_1: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_5: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)

	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
* BSpline (6)

	global i_controls sexe married child wsalred_bspline wsalred_bspline_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m6_1: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_5: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
*** WOMEN ***
use Data/Clean/H&R, clear
	drop if optim ==1
	keep if sexe ==0 
	
	* OG (1)
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10 
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m1_1: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_4: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_5: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* A1 (2)
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m2_1: reghdfe hplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_3: reghdfe abs_wedge post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 											// * 
	eststo m2_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_5: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	

* CMA (3)
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m3_1: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_5: reghdfe tplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* LMA (4)
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m4_1: reghdfe hplus post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls   [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_4: reghdfe wedge_sq post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_5: reghdfe tplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)			// *

	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
										
* Lowess (5)
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m5_1: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_5: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)

	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
* BSpline (6)

	global i_controls sexe married child wsalred_bspline wsalred_bspline_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m6_1: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_4: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_5: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)

	
********************************
**# Regressions - Industry ***** 
********************************

use Data/Clean/H&R, clear
	//keep if naf10 == "RU" | naf10 == "JZ"
	keeo if naf10 == 
	drop if optim == 1
	//keep if (naf10 =="KZ"| naf10 == "LZ" | naf10 == "RU" | naf10 == "JZ")
	
	* OG (1)
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10 
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m1_1: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_4: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_5: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* A1 (2)
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m2_1: reghdfe hplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_3: reghdfe abs_wedge1 post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 											// * 
	eststo m2_4: reghdfe wedge1_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_5: reghdfe tplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	

* CMA (3)
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m3_1: reghdfe hplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_3: reghdfe abs_wedge_CMA post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_4: reghdfe wedge_CMA_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_5: reghdfe tplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* LMA (4)
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m4_1: reghdfe hplus_LMA post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_3: reghdfe abs_wedge_LMA post_treatment treatment $i_controls $c_controls   [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_4: reghdfe wedge_LMA_sq post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_5: reghdfe tplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)			// *

	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
										
* Lowess (5)
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m5_1: reghdfe hplus_lowess post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_3: reghdfe abs_wedge_lowess post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_4: reghdfe wedge_lowess_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_5: reghdfe tplus_lowess post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)

	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
* BSpline (6)

	global i_controls sexe married child wsalred_bspline wsalred_bspline_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	* DiD
	eststo m6_1: reghdfe hplus_bspline post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_3: reghdfe abs_wedge_bspline post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_4: reghdfe wedge_bspline_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_5: reghdfe tplus_bspline post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)

***********************************
**# Regressions - WAGE GROUPS ***** 
***********************************
use Data/Clean/H&R, clear
	est clear 
	
	drop if optim ==1
	drop if wedge < 0 & wedge !=.
	
*** DiD *** ONLY USES THE IMPUTED WAGE *****
	
* OG (1)
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	eststo m1_11: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_12: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_13: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
		
	eststo m1_21: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_22: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_23: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m1_31: reghdfe abs_wedge  optim post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==1, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_32: reghdfe abs_wedge  optim post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==2, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_33: reghdfe abs_wedge  optim post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==3, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m1_41: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==1, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_42: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==2, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_43: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==3, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m1_51: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m1_52: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m1_53: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	esttab m1_11 m1_21 m1_31 m1_41 m1_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m1_12 m1_22 m1_32 m1_42 m1_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m1_13 m1_23 m1_33 m1_43 m1_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib

	
* A1 (2)
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m2_11: reghdfe hplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_12: reghdfe hplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_13: reghdfe hplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m2_21: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_22: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_23: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	eststo m2_31: reghdfe abs_wedge post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 1, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 	
	eststo m2_32: reghdfe abs_wedge post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 2, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 	
	eststo m2_33: reghdfe abs_wedge post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 3, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 	// * 
	
	eststo m2_41: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_42: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_43: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m2_51: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_52: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_53: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	esttab m2_11 m2_21 m2_31 m2_41 m2_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m2_12 m2_22 m2_32 m2_42 m2_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m2_13 m2_23 m2_33 m2_43 m2_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	

* CMA (3)
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m3_11: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_12: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_13: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m3_21: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_22: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_23: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	
	eststo m3_31: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_32: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_33: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m3_41: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_42: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_43: reghdfe wedge_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	
	eststo m3_51: reghdfe tplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m3_52: reghdfe tplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m3_53: reghdfe tplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m3_11 m3_21 m3_31 m3_41 m3_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m3_12 m3_22 m3_32 m3_42 m3_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m3_13 m3_23 m3_33 m3_43 m3_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	
	
* LMA (4)
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	preserve 
	keep if sexe == 0
	eststo m4_11: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_12: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_13: reghdfe hplus post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_21: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_22: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)	
	eststo m4_23: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_31: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls   [pweight=extri] if tertile_LMA ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_32: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls   [pweight=extri] if tertile_LMA ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_33: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls   [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_41: reghdfe wedge_sq post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==1, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_42: reghdfe wedge_sq post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==2, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_43: reghdfe wedge_sq post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==3, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m4_51: reghdfe tplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m4_52: reghdfe tplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m4_53: reghdfe tplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num) // 
	
	esttab m4_11 m4_21 m4_31 m4_41 m4_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m4_12 m4_22 m4_32 m4_42 m4_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m4_13 m4_23 m4_33 m4_43 m4_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib

* Lowess (5)
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m5_11: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_12: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_13: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m5_21: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_22: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_23: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_31: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_lowess == 1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_32: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_lowess == 2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_33: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_lowess == 3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_41: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_42: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_43: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_51: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_52: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_53: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m5_11 m5_21 m5_31 m5_41 m5_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m5_12 m5_22 m5_32 m5_42 m5_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m5_13 m5_23 m5_33 m5_43 m5_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	
* B-spline (6)
	global i_controls sexe married child wsalred_bspline wsalred_bspline_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp 
	
	eststo m6_11: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_12: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_13: reghdfe hplus post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m6_21: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_22: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_23: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m6_31: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_bspline == 1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_32: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_bspline == 2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_33: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_bspline == 3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m6_41: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_42: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_43: reghdfe wedge_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m6_51: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_52: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_53: reghdfe tplus post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m6_11 m6_21 m6_31 m6_41 m6_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m6_12 m6_22 m6_32 m6_42 m6_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m6_13 m6_23 m6_33 m6_43 m6_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	
	*(1)
	esttab m1_11 m1_21 m1_31 m1_41 m1_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m1_12 m1_22 m1_32 m1_42 m1_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m1_13 m1_23 m1_33 m1_43 m1_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(2)
	esttab m2_11 m2_21 m2_31 m2_41 m2_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m2_12 m2_22 m2_32 m2_42 m2_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m2_13 m2_23 m2_33 m2_43 m2_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(3)
	esttab m3_11 m3_21 m3_31 m3_41 m3_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m3_12 m3_22 m3_32 m3_42 m3_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m3_13 m3_23 m3_33 m3_43 m3_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(4)
	esttab m4_11 m4_21 m4_31 m4_41 m4_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m4_12 m4_22 m4_32 m4_42 m4_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m4_13 m4_23 m4_33 m4_43 m4_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(5)
	esttab m5_11 m5_21 m5_31 m5_41 m5_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m5_12 m5_22 m5_32 m5_42 m5_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m5_13 m5_23 m5_33 m5_43 m5_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(6)
	esttab m6_11 m6_21 m6_31 m6_41 m6_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m6_12 m6_22 m6_32 m6_42 m6_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m6_13 m6_23 m6_33 m6_43 m6_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	

***** IMPUTED DESIRED HOURS *****
	
	est clear
	* OG (1)
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	eststo m1_11: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_12: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_13: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
		
	eststo m1_21: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_22: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_23: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m1_31: reghdfe abs_wedge  optim post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==1, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_32: reghdfe abs_wedge  optim post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==2, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_33: reghdfe abs_wedge  optim post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile ==3, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m1_41: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==1, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_42: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==2, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_43: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==3, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m1_51: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m1_52: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m1_53: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile ==3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	esttab m1_11 m1_21 m1_31 m1_41 m1_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m1_12 m1_22 m1_32 m1_42 m1_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m1_13 m1_23 m1_33 m1_43 m1_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib

	
* A1 (2)
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m2_11: reghdfe hplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_12: reghdfe hplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_13: reghdfe hplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m2_21: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_22: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_23: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	eststo m2_31: reghdfe abs_wedge1 post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 1, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 	
	eststo m2_32: reghdfe abs_wedge1 post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 2, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 	
	eststo m2_33: reghdfe abs_wedge1 post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile1 == 3, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 	// * 
	
	eststo m2_41: reghdfe wedge1_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_42: reghdfe wedge1_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_43: reghdfe wedge1_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m2_51: reghdfe tplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 1, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_52: reghdfe tplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 2, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_53: reghdfe tplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri] if tertile1 == 3, ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	
	esttab m2_11 m2_21 m2_31 m2_41 m2_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m2_12 m2_22 m2_32 m2_42 m2_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m2_13 m2_23 m2_33 m2_43 m2_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	

* CMA (3)
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m3_11: reghdfe hplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_12: reghdfe hplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_13: reghdfe hplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m3_21: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_22: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_23: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	
	eststo m3_31: reghdfe abs_wedge_CMA post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_32: reghdfe abs_wedge_CMA post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_33: reghdfe abs_wedge_CMA post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m3_41: reghdfe wedge_CMA_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_42: reghdfe wedge_CMA_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_43: reghdfe wedge_CMA_sq post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	
	eststo m3_51: reghdfe tplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m3_52: reghdfe tplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m3_53: reghdfe tplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_CMA == 3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m3_11 m3_21 m3_31 m3_41 m3_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m3_12 m3_22 m3_32 m3_42 m3_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m3_13 m3_23 m3_33 m3_43 m3_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	
	
* LMA (4)
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	preserve 
	keep if sexe == 0
	eststo m4_11: reghdfe hplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_12: reghdfe hplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_13: reghdfe hplus_LMA post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_21: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_22: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)	
	eststo m4_23: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_31: reghdfe abs_wedge_LMA post_treatment treatment $i_controls $c_controls   [pweight=extri] if tertile_LMA ==1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_32: reghdfe abs_wedge_LMA post_treatment treatment $i_controls $c_controls   [pweight=extri] if tertile_LMA ==2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_33: reghdfe abs_wedge_LMA post_treatment treatment $i_controls $c_controls   [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_41: reghdfe wedge_LMA_sq post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==1, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_42: reghdfe wedge_LMA_sq post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==2, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_43: reghdfe wedge_LMA_sq post_treatment treatment $i_controls $c_controls  [pweight=extri] if tertile_LMA ==3, ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m4_51: reghdfe tplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m4_52: reghdfe tplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m4_53: reghdfe tplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_LMA ==3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num) // 
	
	esttab m4_11 m4_21 m4_31 m4_41 m4_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m4_12 m4_22 m4_32 m4_42 m4_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m4_13 m4_23 m4_33 m4_43 m4_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib

* Lowess (5)
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m5_11: reghdfe hplus_lowess post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_12: reghdfe hplus_lowess post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_13: reghdfe hplus_lowess post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m5_21: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_22: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_23: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_31: reghdfe abs_wedge_lowess post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_lowess == 1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_32: reghdfe abs_wedge_lowess post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_lowess == 2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_33: reghdfe abs_wedge_lowess post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_lowess == 3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_41: reghdfe wedge_lowess_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_42: reghdfe wedge_lowess_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_43: reghdfe wedge_lowess_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_51: reghdfe tplus_lowess post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_lowess == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_52: reghdfe tplus_lowess post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_lowess == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_53: reghdfe tplus_lowess post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_lowess == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m5_11 m5_21 m5_31 m5_41 m5_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m5_12 m5_22 m5_32 m5_42 m5_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m5_13 m5_23 m5_33 m5_43 m5_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	
* B-spline (6)
	global i_controls sexe married child wsalred_bspline wsalred_bspline_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp 
	
	eststo m6_11: reghdfe hplus_bspline post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_12: reghdfe hplus_bspline post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_13: reghdfe hplus_bspline post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m6_21: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_22: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_23: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m6_31: reghdfe abs_wedge_bspline post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_bspline == 1, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_32: reghdfe abs_wedge_bspline post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_bspline == 2, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_33: reghdfe abs_wedge_bspline post_treatment treatment $i_controls $c_controls [pweight=extri] if tertile_bspline == 3, ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m6_41: reghdfe wedge_bspline_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_42: reghdfe wedge_bspline_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_43: reghdfe wedge_bspline_sq post_treatment treatment $i_controls  $c_control [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m6_51: reghdfe tplus_bspline post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_bspline == 1, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_52: reghdfe tplus_bspline post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_bspline == 2, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_53: reghdfe tplus_bspline post_treatment treatment $i_controls  $c_controls  [pweight=extri] if tertile_bspline == 3, ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m6_11 m6_21 m6_31 m6_41 m6_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m6_12 m6_22 m6_32 m6_42 m6_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m6_13 m6_23 m6_33 m6_43 m6_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	
	*(1)
	esttab m1_11 m1_21 m1_31 m1_41 m1_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m1_12 m1_22 m1_32 m1_42 m1_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m1_13 m1_23 m1_33 m1_43 m1_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(2)
	esttab m2_11 m2_21 m2_31 m2_41 m2_51 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m2_12 m2_22 m2_32 m2_42 m2_52 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m2_13 m2_23 m2_33 m2_43 m2_53 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(3)
	esttab m3_11 m3_21 m3_31 m3_41 m3_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m3_12 m3_22 m3_32 m3_42 m3_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m3_13 m3_23 m3_33 m3_43 m3_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(4)
	esttab m4_11 m4_21 m4_31 m4_41 m4_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m4_12 m4_22 m4_32 m4_42 m4_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m4_13 m4_23 m4_33 m4_43 m4_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(5)
	esttab m5_11 m5_21 m5_31 m5_41 m5_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m5_12 m5_22 m5_32 m5_42 m5_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m5_13 m5_23 m5_33 m5_43 m5_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	*(6)
	esttab m6_11 m6_21 m6_31 m6_41 m6_51, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 1st tertile of wage distrib
	esttab m6_12 m6_22 m6_32 m6_42 m6_52, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 2nd tertile of wage distrib
	esttab m6_13 m6_23 m6_33 m6_43 m6_53, keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)		// 3rd tertile of wage distrib
	
	
	
	
	
	* DiD with imputed Wages 
	
	
use Data/Clean/H&R, clear

// 	gen temp = 0 
// 	replace temp = 1 if wedge <= 0 & wedge !=. & datdeb <= tepa_date  
// 	sort indiv_num datdeb 
// 	bysort indiv_num: egen wasnt_underworked = max(temp)
// 	order wasnt_underworked
//	
// 	keep if wasnt_underworked ==0

	est clear	
	keep if optim ==0

* OG (1)
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10 ue_q_dep
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m1_1: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m1_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_4: reghdfe wedge_sq post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m1_5: reghdfe tplus post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* A1 (2)
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10 ue_q_dep
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m2_1: reghdfe hplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m2_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m2_3: reghdfe abs_wedge1 post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 											// * 
	eststo m2_4: reghdfe wedge1_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m2_5: reghdfe tplus1 post_treatment treatment $i_controls   $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	

* CMA (3)
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10 ue_q_dep
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m3_1: reghdfe hplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m3_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	eststo m3_3: reghdfe abs_wedge_CMA post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m3_4: reghdfe wedge_CMA_sq post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	eststo m3_5: reghdfe tplus_CMA post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	
* LMA (4)
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10 ue_q_dep
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m4_1: reghdfe hplus_LMA post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_3: reghdfe abs_wedge_LMA post_treatment treatment $i_controls $c_controls   [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m4_4: reghdfe wedge_LMA_sq post_treatment treatment $i_controls $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	eststo m4_5: reghdfe tplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)			// *
	
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
										
* Lowess (5)
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10 ue_q_dep
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m5_1: reghdfe hplus_lowess post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m5_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_3: reghdfe abs_wedge_lowess post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_4: reghdfe wedge_lowess_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m5_5: reghdfe tplus_lowess post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
* BSpline (6)

	global i_controls sexe married child wsalred_bspline wsalred_bspline_sq age age_sq ancentr cat_naf10 ue_q_dep
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m6_1: reghdfe hplus_bspline post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	eststo m6_2: reghdfe empnbh post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_3: reghdfe abs_wedge_bspline post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_4: reghdfe wedge_bspline_sq post_treatment treatment $i_controls  $c_control [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	eststo m6_5: reghdfe tplus_bspline post_treatment treatment $i_controls  $c_controls  [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	
	* RESULTS 
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
