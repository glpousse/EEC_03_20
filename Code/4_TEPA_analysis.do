*********************************
*********************************
********* TEPA ANALYSIS ********* 
*********************************
*********************************

cd "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"

cap ssc install winsor2

************************
************************
**# DiD Analaysis ******
************************
************************

use Data/Clean/old_2003-2020/df_wedges, clear
	drop if salred < smic_m_net


	keep if in_tepa 
	keep if contra ==1 
	keep if hhc > 34 & hhc < 71
	drop if empaff != 4
	 gen tplus = .
 	replace tplus = 1 if wedge > 0
 	replace tplus = 0 if (wedge == 0 | wedge < 0)

********************************
***** Cross-Border Workers ***** 
********************************

*** SETUP *** 

use Data/Clean/p_df_wedges, clear

drop if salred < smic_m_net
drop if naf10 == "AZ"
drop if naf4 == "ES"

	keep if in_tepa

	
	*DiD dummies
	gen treatment 	= (dom_bel == 1 | dom_ger == 1 | dom_swz == 1 | dom_lux == 1) & france == 1
	gen control 	= (trans_bel == 1 | trans_ger == 1| trans_swz == 1 | trans_lux == 1  )
	gen post_treatment = treatment * post_tepa 
	
	* TD dummies 
	gen triple_optim = treatment * optim * post_tepa
	gen post_optim = post_tepa * optim
 	gen treatment_optim = treatment * optim
	
	* Trends 
	gen group_trend = datdeb * treatment 
	
	keep if control | treatment
	
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
*** DID REGRESSIONS *** 
	
	*** Linear ***
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) // 
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tplus ==1, absorb(datdeb indiv_num) vce(cluster indiv_num) // 
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if tplus ==0, absorb(datdeb indiv_num) vce(cluster indiv_num) //

	*** Logit *** 
	xtset indiv_num datdeb
	xtlogit tplus post_treatment treatment $i_controls, fe  //
	xtlogit tplus post_treatment treatment $i_controls i.annee, fe  //
	
	
**********************************************************************
* New sample
********************************************************************** 
use Data/Clean/p_df_wedges, clear

* Code to add to data prep file * 		
			

// drop if ap01a 	== 1				// Job Switch 		

 	* Sample 
	keep if in_tepa 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	drop if salred < smic_m_net			// I dont have smic net pre-2005
	
	* DiD DUMMIES
	gen treatment 		= (dom_bel == 1 | dom_ger == 1 | dom_swz == 1 | dom_lux == 1 | dom_uk ==1) & france == 1
	gen control 		= (trans_bel == 1 | trans_ger == 1| trans_swz == 1 | trans_lux == 1 | trans_uk ==1 )
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
	
	* Controls 
	global i_controls sexe married child salred salred_sq age age_sq ancentr cat_naf10
	global c_controls gs_gdp CBC 
	
	
	* SIMPLE OLS MODELS (DiD)
	reghdfe hplus post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) 
	
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) 
	
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if wedge >0, absorb(datdeb indiv_num) vce(cluster indiv_num) 
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri] if wedge <0, absorb(datdeb indiv_num) vce(cluster indiv_num) 
	
	reghdfe tplus post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) 
	
	* SIMPLE OLS MODELS (TD)
	reghdfe hplus optim post_treatment treatment $i_controls $c_controls  [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) 
	
	reghdfe wedge optim $i_controls [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) 
	
	
	
	
	
	
	* LOGISTIC MODELS
	xtset indiv_num datdeb
	xtlogit tplus post_treatment treatment $i_controls $c_controls i.datdeb, fe 	
	
	
	
	
**********************************************************************
* HOURS vs DAYS
**********************************************************************
use Data/Clean/p_df_wedges, clear

* Code to add to data prep file * 
encode nafg16, gen(cat_nafg16)
drop if nafg16 == "EB"				// Industries agricole -- check with supervisor

	* Sample 
	keep if in_tepa 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	drop if salred < smic_m_net			// I dont have smic net pre-2005
	
	* DiD Dummies
	gen treatment 		= (forfait ==1)
	gen control 		= (forfait ==2)
	gen post_treatment 	= treatment * post_tepa 
	
	

	

	
**********************************************************************

*** Tables for Meeting *** 




* Absolute 

eststo abs1: reghdfe wedge  post_treatment treatment $i_controls $c_controls if wedge>0 [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)

eststo abs2: reghdfe abs_wedge post_treatment treatment $i_controls $c_controls if wedge<0 [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num)





*Logit
xtset indiv_num datdeb_q

eststo L1: xtlogit tplus post_treatment treatment $i_controls $c_controls i.datdeb_q, fe 

eststo L2: xtlogit tplus post_treatment treatment $i_controls $c_controls i.annee, fe 

eststo L3: xtlogit tplus post_treatment treatment $i_controls $c_controls, fe 
	
esttab L1 L2 L3 using 	"Output/Tables/Moshe2.tex", keep(post_treatment treatment) n p star(* 0.10 ** 0.05 *** 0.01) label replace
	


	
	
	* Y = Wedge  
	
	eststo m1: reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) // **
	
	eststo m2: reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(indiv_num) vce(cluster indiv_num) // **
	

	
	
	
	
	
	
	
	
	

	
	* Y = |Wedge|
	
	eststo m1: reghdfe abs_wedge post_treatment treatment post_tepa  $i_controls $c_controls  [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) // * 
	
	eststo m2: reghdfe abs_wedge triple_optim optim post_treatment treatment  $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) // *** + * ADD SUMMARY STATS ON MANAGERS
	
	eststo m3: reghdfe wedge post_treatment treatment  $i_controls $c_controls if tplus ==0 [pweight=extri], absorb( datdeb indiv_num) vce(cluster indiv_num) // * 
	
	eststo m4: reghdfe abs_wedge triple_optim optim post_treatment treatment $i_controls $c_controls if tplus ==0 [pweight=extri], absorb(datdeb indiv_num) vce(cluster indiv_num) // *** + * ADD SUMMARY STATS ON MANAGERS
	
	esttab m1 m3 using "Output/Tables/abs_w_bytplus_tplus.tex", keep(treatment post_treatment) n p star(* 0.10 ** 0.05 *** 0.01) label replace 
	
	
	* Y = tplus -- indicator for individuals wanting to work more.
	*OLS
	eststo ID1_tplus_time_FE_did: reghdfe tplus post_treatment treatment $i_controls $c_controls, absorb(datdeb indiv_num) vce(cluster indiv_num)
	eststo ID1_tplus_notime_FE_did: reghdfe tplus post_treatment treatment $i_controls $c_controls, absorb(indiv_num) vce(cluster indiv_num)
	*Logit
	xtset indiv_num datdeb_q
	eststo L1: xtlogit tplus post_treatment treatment $i_controls $c_controls i.datdeb_q, fe 
	eststo L2: xtlogit tplus post_treatment treatment $i_controls $c_controls i.annee, fe 
	eststo L3: xtlogit tplus post_treatment treatment $i_controls $c_controls, fe 
	
	esttab L1 L2 L3 using 	"Output/Tables/Moshe2.tex", keep(post_treatment treatment) n p star(* 0.10 ** 0.05 *** 0.01) label replace 
	





	
*** ROBUSTNESS CHECKS *** 

* WINSORIZING the Dependent Variable - I ensure the resulrs are not driven by outliers. 
	
	foreach var in wedge hplus abs_wedge {
		winsor2 `var', cuts(1 99) suffix(_w1)
		winsor2 `var', cuts(2 98) suffix(_w2)
		winsor2 `var', cuts(3 97) suffix(_w3)
		winsor2 `var', cuts(4 96) suffix(_w4)
		winsor2 `var', cuts(5 95) suffix(_w5)
	}
	
	* Regressions 
	forvalues i=1/5{
	reghdfe wedge_w`i' post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(indiv_num datdeb) vce(cluster indiv_num)
	} // **
	
* Assumption #2 on desired hours 
	
	

	



* This is a rolling panel --> we only observe individuals for 6 quarters. I do not have enough pre-treatment periods to test for pre-trends, I compare the sumamry statistics of both groups. 
	

	
	keep if annee < 2012
	reghdfe wedge treatment i.datdeb_q [pweight=extri] if treatment ==1, absorb(indiv_num) cluster(indiv_num)
	margins datdeb_q
	marginsplot, xline(`=tq(2007q3)', lpattern(dash) lcolor(red)) 
	reghdfe wedge treatment i.datdeb_q [pweight=extri] if treatment ==0, absorb(indiv_num) cluster(indiv_num)
	margins datdeb_q
	marginsplot, xline(`=tq(2007q3)', lpattern(dash) lcolor(red)) 

	gen treat_label = cond(treatment == 1, "Treated", "Control")
	reghdfe wedge i.datdeb_q##i.treatment [pweight=extri], absorb(indiv_num) cluster(indiv_num)
	margins datdeb_q, over(treatment)
	marginsplot, noci recast(line) legend(position(6) ring(0)) ///
    title("Predicted Wedge by Quarter and Treatment Status") ///
    ylabel(, angle(horizontal)) xtitle("datdeb_q") ytitle("Predicted wedge")
	
	* Placebo Testing
	
	

***************
***** ID2 ***** I NEED TO USE SYNTHETIC DID? NOT ENOUGH OBS BEFORE TREATMENT. 
***************

use Data/Clean/p_df_wedges, clear

	keep if in_tepa & forfait !=.
	keep if contra == 1
	
	gen tplus = .
	replace tplus = 1 if wedge > 0
	replace tplus = 0 if (wedge == 0 | wedge < 0)
	
*** Forfait Jours vs. Forfait Heures ***
	
	gen treatment 	= (forfait == 2)
	gen control 	= (forfait ==1)
	drop if (treatment ==. | control ==.) 
	drop if wedge == .
	gen post_treatment = post_tepa*treatment 
	gen triple_optim = treatment * optim * post_tepa
	gen triple_gender = treatment * sexe * post_tepa
	gen triple_underworked = treatment * underworked * post_tepa
	
	//keep if hhc >=35 & hhc <=70
	
	global controls_i sexe married child salred salred_sq age age_sq annees_etudes cat_naf10
	global controls_f nbsala nbsalb 

*** Regressions *** 

	* Y = Wedge  
		
	eststo ID2_did: reghdfe wedge post_treatment treatment $controls_i $controls_f [pweight=extri] , absorb(indiv_num datdeb) cluster(indiv_num)
	
	eststo ID2_td: reghdfe wedge triple_optim post_treatment treatment $controls_i $controls_f [pweight=extri], absorb(datdeb indiv_num) cluster(indiv_num) 
	
	* indicator
	
	eststo ID2_abs_did: reghdfe wedge post_treatment treatment $controls_i $controls_f  if tplus ==1 [pweight=extri], absorb(datdeb_q indiv_num) cluster(indiv_num) 
	
	eststo ID2_abs_td: reghdfe wedge triple_optim post_treatment treatment $controls_i $controls_f if tplus ==1 [pweight=extri], absorb(datdeb indiv_num) cluster(indiv_num) 
	
	xtset indiv_num datdeb
	
	eststo ID2_logit_did: xtlogit tplus treatment#post_tepa treatment i.annee $controls_i $controls_f, fe 
	xtlogit tplus post_treatment treatment i.annee $controls_i $controls_f, fe 
	
	eststo ID2_logit_td: xtlogit tplus triple_optim optim post_treatment treatment i.annee $controls_i $controls_f, fe 
	


*** Generalized Synthetic Control Method *** 
	
	* I need at least 3 pre-treatment obs to identify a "trend"
	gen dummy1 = 1
	bysort indiv_num: egen balance_dummy = total(dummy1)
	drop if control == 1 & datdeb < tepa_date & balance_dummy < 3
	drop if treatment == 1 & datdeb < tepa_date & balance_dummy < 3
	
	export delimited using "Data/Clean/gscm.csv", replace
		
	* No GSCM package exists in STATA
	shell "Rscript /Code/GSCM.r"
	
	
	






*** Descriptive Statistics on Control & Treatment *** 

	tabstat wedge abs_wedge $controls_i $controls_f, by(treatment)


	* Age 
	 twoway(hist age if treatment ==1, fraction color(red%50))(hist age if treatment ==0, color(blue%50) fraction), legend(label(1 "Treatment") label(2 "Control" )) xtitle(Age)
	 
	 * Industry  
	 twoway(graph bar naf10 if treatment ==1, fraction color(red%50))(graph bar naf10 if treatment ==0, color(blue%50) fraction), legend(label(1 "Treatment") label(2 "Control" )) xtitle(Industry)
	
	* Wedge
	twoway(hist wedge if treatment ==1, fraction color(red%50))(hist wedge if treatment ==0, fraction color(blue%50)), legend(label(1 "Forfait Jours") label(2 "Forfait Heures"))
	
	* "Parallel" Trends
	preserve
		collapse (mean) wedge [pweight=extri], by(datdeb treatment)
		twoway(line wedge datdeb if treatment == 1)(line wedge datdeb if treatment == 0)
	restore 
	
	* Placebo Testing
	
	gen ttt = datdeb - tepa_date

	eventstudyinteract wedge ttt treatment salred, absorb(indiv_num datdeb) vce(cluster indiv_num) cohort(treatment) control_cohort(control)
	
	
***** PSM ***** 

	logit post_treatment $controls_i $controls_f  [pweight = extri]
	predict pscore
	
	
	gen ipw = .
	replace ipw = 1/pscore if post_treatment == 1
	replace ipw = 1/(1-pscore) if post_treatment == 0
	
	gen final_weight = ipw * extri

	reghdfe wedge post_treatment treatment $controls_i $controls_f [pweight=final_weight] , absorb(indiv_num datdeb) cluster(indiv_num)
	
		* Y = Wedge  
	eststo wID2_did: reghdfe wedge post_treatment treatment $controls_i $controls_f [pweight=final_weight] , absorb(indiv_num datdeb) cluster(indiv_num)
	eststo wID2_td: reghdfe wedge triple_optim post_treatment treatment $i_controls $controls_f [pweight=final_weight], absorb(datdeb indiv_num) cluster(indiv_num) 
	
	* Y = |Wedge|
	eststo wID2_abs_did: reghdfe abs_wedge post_treatment treatment i.tplus $i_controls $controls_f i.tplus [pweight=final_weight], absorb(datdeb indiv_num) cluster(indiv_num) 
	eststo wID2_abs_td: reghdfe abs_wedge triple_optim post_treatment treatment i.tplus $i_controls $controls_f i.tplus [pweight=final_weight], absorb(datdeb indiv_num) cluster(indiv_num) 
	
	xtset indiv_num datdeb_q
	eststo wID2_logit_did: xtlogit tplus post_treatment treatment  i.annee $controls_i $controls_f, fe 
	eststo wID2_logit_td: xtlogit tplus triple_optim optim post_treatment treatment i.annee $controls_i $controls_f, fe 
	
esttab wID2_did wID2_td wID2_abs_did wID2_abs_td wID2_logit_did wID2_logit_td using "Output/Tables/PSM.tex" , keep(treatment post_treatment triple_optim) n p star(* 0.10 ** 0.05 *** 0.01) label replace 
	

	
***************
***** ID4 *****
***************

***** Heterogeneous Treatment Design ***** 

	/*
		Pre-TEPA (Jan 2003, Loi Fillon): 
			
			> 20 employee firms: 0 < OT < 8 => 25% premium - taxed
								 8 < OT 	=> 50% premium - taxed
			
			< 20 employee firms: 0 < OT < 4 => 10% premium - taxed
								 4 < OT < 8 => 25% premium - taxed
								 8 < OT 	=> 50% premium - taxed
								 
		Post-TEPA (Oct 2007, Loi TEPA):
			
			> 20 employee firms: 0 < OT < 8 => 25% premium - untaxed
								 8 < OT 	=> 50% premium - untaxed
			
			< 20 employee firms: 0 < OT < 8 => 25% premium - untaxed
								 8 < OT 	=> 50% premium - untaxed
	*/
	
use Data/Clean/p_df_wedges, clear
	
	keep if in_tepa & emphre !=. & wedge !=. 
	
	gen low = (emphre >0 & emphre < 8 & emphre !=.)
	gen high = (emphre >= 8)
	keep if low | high
	
	gen treatment = high 
	gen post_treatment = treatment * post_tepa 
	
	
	
	
	

	



	
	
//////////////////// MISC/ WIP //////////////////////// 
	




	
	
	
	

	


***** Bunching at 35hours ***** 

use Data/Clean/p_df_wedges, clear
	
	keep if in_tepa 
	
	gen treatment = (empnbh == 40 & (emphre == 0 | emphre == .) & wedge >0 )

	egen sd_OT = sd(emphre), by(indiv_num)
	
	egen sd_work	=	sd(empnbh), by(indiv_num)
	
	gen constant_work = (sd_work == 0)
	
	gen constant_OT = (sd_OT == 0)
	
	gen control = (constant_OT == 1 & treatment == 0 & empnbh > 40)
	
	keep if treatment | control
	
	gen post_treatment = treatment * post_tepa 
	
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num)
	

	
	* Pooled Sample 
	
	xtreg neg_wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc,fe robust cluster(indiv_num) // signig hhc (makes sense) 
	
	xtreg wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc,fe robust cluster(indiv_num) // signif hhc (makes sense) 
	
	* Cross Section
	
	collapse (mean) overworked wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc [pweight=extri], by (indiv_num annee)
	
	reg wedge sexe i.age_group i.educ_degree log_salred log_empnbh if annee == 2019, robust // signig hhc (makes sense) 
	
	
	