**********************
**********************
***** ROBUSTNESS *****
**********************
**********************

************************************************************
**# TEST 1 - SELECTION EFFECTS: Dropping Job Switchers *****
************************************************************

use Data/Clean/df_wedges, clear 

	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic

 bysort datdeb_q: egen test = mean(hplus)

 collapse (mean) test [pw=extri], by(datdeb_q)

 twoway (bar test datdeb_q)

 
 
 
 
drop du
gen du = 1 if so ==2
sort datdeb_q
twoway (bar du datdeb_q)


use Data/Clean/H&R, clear
	
	drop if ap0c == 2			// Statut identique par rapport à l'enquête précédente –– 2 = Non 
	* Regressions 
	keep if optim == 0

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
	
	* Results
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)

	
********************************************************
**# TEST 2 - ANTICIPATION EFFECTS: PLACEBO TESTING *****
********************************************************
	
*** 2006 *** 
	
use Data/Clean/H&R, clear
	
	est clear
	gen post_placebo = 0
	replace post_placebo = 1 if (datdeb_q >= tq(2006Q4))
	drop post_treatment 
	gen post_treatment 	= post_placebo * treatment 
	drop if optim ==1


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
	
	* Results
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)



**********************************************************
**# TEST 3 - COMPOSITIONAL EFFECTS: GROUP TIME TREND *****
**********************************************************

use Data/Clean/H&R, clear

	* Trends
	gen time_trend 	= datdeb_q - tq(2007q3)
	gen group_trend = treatment * time_trend 

**********************************************
**# TEST 4 - Normalizing SD: Log(1+var)  ***** NOT ROBUST
**********************************************

use Data/Clean/H&R, clear
	
	cap drop temp
	rename hplus temp 
	gen hplus 	= log(1+temp) 
	drop temp 
	rename empnbh temp
	gen empnbh 	= log(1+temp)
	drop temp
	drop wedge abs_wedge wedge_sq tplus 
	gen wedge 		= hplus - empnbh 
	gen abs_wedge 	= abs(wedge)
	gen wedge_sq	= wedge^2
	gen tplus 		= .
	replace tplus 	= 1 if wedge > 0 
	replace tplus 	= 0 if (wedge == 0 | wedge < 0)
	
	drop if optim ==1
	
***** REGRESSIONS ***** 
	est clear 
		
	*OG (1)
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
	absorb (datdeb indiv_num) 	 vce(cluster indiv_num)
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

	* RESULTS
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)
	esttab m6_1 m6_2 m6_3 m6_4 m6_5 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)

	
	
***************************************
**# TEST 5 - P(BEING A HOMEOWNER) *****
***************************************

use Data/Clean/H&R, clear

	* SETUP 	
	gen homeowner = (so == 2)
	drop if optim ==1 
	est clear 
	
	gen new_treatment = control
	gen new_control = treatment 
	drop treatment control 
	rename new_treatment treatment 
	rename new_control control 
	drop post_treatment
	gen post_treatment = treatment * tepa_date
	
	
	* REGRESSING
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10 
	global c_controls gs_gdp CBC 
	eststo m1: reghdfe homeowner post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb) vce(cluster indiv_num)
	
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	eststo m2: reghdfe homeowner post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb) vce(cluster indiv_num)
	
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	eststo m3: reghdfe homeowner post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb) vce(cluster indiv_num)
	
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	eststo m4: reghdfe homeowner post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb) vce(cluster indiv_num)
	
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	eststo m5: reghdfe homeowner post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb) vce(cluster indiv_num)
	
	global i_controls sexe married child wsalred_bspline wsalred_bspline_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	eststo m6: reghdfe homeowner post_treatment treatment $i_controls  $c_controls [pweight=extri], ///
	absorb(datdeb) vce(cluster indiv_num)

	* RESULTS
	esttab m1 m2 m3 m4 m5 m6 , keep(post_treatment) se star(* 0.10 ** 0.05 *** 0.01)


		

	




















