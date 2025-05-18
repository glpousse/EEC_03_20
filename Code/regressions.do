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
	gen treatment 		= (dom_bel == 1 | dom_ger == 1 | dom_swz == 1 | dom_lux == 1) & france == 1
	gen control		= (trans_bel == 1 | trans_ger == 1| trans_swz == 1 | trans_lux == 1)
	gen post_treatment 	= treatment * post_tepa 
	keep if treatment | control
	
	* TD DUMMIES - OPTIM
	gen triple_optim 	= treatment * optim * post_tepa
	gen post_optim 		= post_tepa * optim
 	gen treatment_optim = treatment * optim
	* TD DUMMIES - GENDER
	gen triple_gender 	= treatment * sexe * post_tepa
	gen post_sexe 		= post_tepa * sexe 
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
	
	foreach var in CMA LMA lowess{
		
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
	
	
		foreach var in salred salred1 wsalred_CMA wsalred_LMA wsalred_lowess {
		cap drop weekly_`var' hourly_`var' log_`var' log_hourly_`var' `var'_sq
		gen weekly_`var'     	= `var'/ 4.33
		gen hourly_`var' 	  	= weekly_`var' / empnbh 
		gen log_`var' 	      	= log(`var')
		gen log_hourly_`var'  	= log(hourly_`var')
		gen `var'_sq = `var'^2 
	}
	
	order tplus* wedge_* 
	
	preserve 
		collapse (mean) hplus abs_wedge tplus empnbh, by(treatment datdeb_q)
		twoway(line hplus datdeb_q if treatment ==1)(line hplus datdeb_q if treatment ==0), legend(label(1 "Treatment") label(2 "Control"))
	restore
	
	preserve 
		collapse (mean) hplus abs_wedge tplus empnbh, by(treatment datdeb_q)
		twoway(line abs_wedge datdeb_q if treatment ==1)(line abs_wedge datdeb_q if treatment ==0), legend(label(1 "Treatment") label(2 "Control"))
	restore 
	
	preserve
		collapse (mean) hplus abs_wedge tplus empnbh, by(treatment datdeb_q)
		twoway(line tplus datdeb_q if treatment ==1)(line tplus datdeb_q if treatment ==0), legend(label(1 "Treatment") label(2 "Control"))
	restore
	
	tab obs_count

*********************************************
**# DEPENDENT VARIABLE VOLATILITY TABLE *****
*********************************************
	
	foreach var in hplus empnbh abs_wedge wedge_sq tplus{
		display "Stats for `var'"
		preserve 
			keep if datdeb <= tepa_date & `var' != .
			su `var'
			display "SD = " r(sd)
			display "CV = " (r(sd)/r(mean)) *100

			cap drop pre_t_obs
			bysort indiv_num: gen pre_t_obs = _N 
			display "Summarizing Pre-Treatment Observations"
			su pre_t_obs
			display "Avg. Pre-Treatment Obs. = " r(mean)
		restore
	}
		
***********************
***** Regressions ***** 
***********************	
	
	est clear
	
* OG
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m1_1: reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m1_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m1_3: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m1_4: reghdfe wedge_sq post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m1_5: reghdfe tplus post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	esttab m1_1 m1_2 m1_3 m1_4 m1_5 , keep(post_treatment) p star(* 0.10 ** 0.05 *** 0.01)
	
	* LOGIT
	xtset indiv_num datdeb
	xtlogit tplus post_treatment treatment $i_controls $c_controls, fe
	
* A1  
	global i_controls sexe married child salred1 salred1_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m2_1: reghdfe hplus1 post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m2_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	eststo m2_3: reghdfe abs_wedge1 post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num) 											// * 
	
	eststo m2_4: reghdfe wedge1_sq post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m2_5: reghdfe tplus1 post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	  vce(cluster indiv_num)
	
	esttab m2_1 m2_2 m2_3 m2_4 m2_5 , keep(post_treatment) p star(* 0.10 ** 0.05 *** 0.01)
	
	* LOGIT
	xtset indiv_num datdeb
	xtlogit tplus1 post_treatment treatment $i_controls $c_controls , fe

* CMA 
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m3_1: reghdfe hhplus_CMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m3_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	  vce(cluster indiv_num)
	
	eststo m3_3: reghdfe abs_wedge_CMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m3_4: reghdfe wedge_CMA_sq post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num ) vce(cluster indiv_num)
	
	eststo m3_5: reghdfe tplus_CMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m3_1 m3_2 m3_3 m3_4 m3_5 , keep(post_treatment) p star(* 0.10 ** 0.05 *** 0.01)
	
	* LOGIT
	xtset indiv_num datdeb
	xtlogit tplus_CMA post_treatment treatment $i_controls $c_controls, fe
	
* LMA 
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m4_1: reghdfe hhplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_3: reghdfe abs_wedge_LMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m4_4: reghdfe wedge_LMA_sq post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	vce(cluster indiv_num)
	
	eststo m4_5: reghdfe tplus_LMA post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)		// **
	
	esttab m4_1 m4_2 m4_3 m4_4 m4_5 , keep(post_treatment) p star(* 0.10 ** 0.05 *** 0.01)
	
	reghdfe tplus_LMA post_treatment treatment $i_controls $c_controls ///
	if naf4 == "ET" [pweight=extri], absorb( datdeb indiv_num) 	 vce(cluster indiv_num)				// **
	
	reghdfe tplus1 triple_gender sexe post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)		
	
	* LOGIT
	xtset indiv_num datdeb
	xtlogit tplus_LMA post_treatment treatment $i_controls $c_controls, fe					
	
* Lowess
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	* DiD
	eststo m5_1: reghdfe hhplus_lowess post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) 	 vce(cluster indiv_num)
	
	eststo m5_2: reghdfe empnbh post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_3: reghdfe abs_wedge_lowess post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_4: reghdfe wedge_lowess_sq post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	eststo m5_5: reghdfe tplus_lowess post_treatment treatment $i_controls $c_controls [pweight=extri], ///
	absorb( datdeb indiv_num) vce(cluster indiv_num)
	
	esttab m5_1 m5_2 m5_3 m5_4 m5_5 , keep(post_treatment) p star(* 0.10 ** 0.05 *** 0.01)
	
	* LOGIT
	xtset indiv_num datdeb
	xtlogit tplus_lowess post_treatment treatment $i_controls $c_controls, fe
	
	
****************************
***** Regressions - TD ***** 
**************************** 


* OG 
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC
	
	reghdfe hplus triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe empnbh triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe abs_wedge triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe wedge_sq triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe tplus triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)

	
* A1
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC
	
	reghdfe hplus1 triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe empnbh triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe abs_wedge1 triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe wedge1_sq triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe tplus1 triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)

* CMA	
	global i_controls sexe married child wsalred_CMA wsalred_CMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC
	
	reghdfe hplus_CMA triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe empnbh triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe abs_wedge_CMA triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe wedge_CMA_sq triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe tplus_CMA triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	
*LMA	
	global i_controls sexe married child wsalred_LMA wsalred_LMA_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC

	reghdfe hplus_LMA triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe empnbh triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe abs_wedge_LMA triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe wedge_LMA_sq triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe tplus_LMA triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
* Lowess 	
	global i_controls sexe married child wsalred_lowess wsalred_lowess_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC
	
	reghdfe hplus_lowess triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe empnbh triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe abs_wedge_lowess triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe wedge_lowess_sq triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	reghdfe tplus_lowess triple_optim optim post_treatment treatment_optim treatment $i_controls $c_controls  ///
	[pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)
	
	