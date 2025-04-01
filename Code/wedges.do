**************************
**************************
********* WEDGES ********* 
**************************
**************************

use Data/Clean/df4_master_final, clear

cap ssc install coefplot

	* Past 8000 hhc goes down 
	
***********************
***** DEFINITIONS *****
***********************

	keep if tppred == 1 // Let's look at full time employees
	
	//drop if (stplc == 9 | stplc == .) 	// Drop those who "do not know" their motivation
	
	replace stplc = 0 if stplc == 2 	// Making binary for analysis
	
	
	* The hplus and salred assumptions 
	
	foreach var in hplus {
		
		bysort indiv_num (datdeb): replace `var' = `var'[_n-1] if missing(`var') & !missing(`var'[_n-1])
	
		bysort indiv_num (datdeb): replace `var' = `var'[_n+1] if missing(`var') & !missing(`var'[_n+1])
	}
	
	bysort indiv_num (datdeb): egen mean_wage = mean(salred)
	
	replace salred = mean_wage 
	
	drop mean_wage 

	

***** New Vars *****

	*bysort annee: keep if hplus !=. & hhc !=. & salred != .
	
	gen wedge 		      	= hplus - empnbh 
	
	gen neg_wedge   	  	= (wedge < 0)
	
	replace neg_wedge 		= . if wedge == . 
		
	gen pos_wedge       	= (wedge > 0)
	
	replace pos_wedge 		= . if wedge == .  
	
	gen worked 		      	= (wedge==0)
	
	gen log_salred 	      	= log(salred)
	
	gen weekly_salred     	= salred/ 4.33
	
	gen hourly_salred 	  	= weekly_salred / empnbh 
	
	ge log_hourly_salred  	= log(hourly_salred)
	
	gen hourly_salmee 	  	= salmee/ (4.33*empnbh)
		
	gen log_hourly_salmee 	= log(hourly_salmee)
	
	gen log_empnbh 	      	= log(empnbh)
	
	gen empnbh_round = ceil(empnbh)
	
	gen married           	= (mpr == 2 | matriprm == 2)
	
	gen age_group 			= . 
	
	replace age_group 		= 1 if (age <= 24 & age >=18)  	// OECD: just entering the working pop
	
	replace age_group 		= 2 if (age >= 25 & age <= 54)	// OECD: Prim working age 
	
	replace age_group 		= 3 if (age >=55 & age <= 64) 	// OECD: Past peak approaching retirement
	
	label define age_lbl 1 "18 - 35" 2 "25 - 54" 3 "55 - 64"
	
	label values age_group age_lbl
	
	drop if age_group == .
	
	gen educ_degree 		= . 
	
	replace educ_degree 	= 1 if nivp >=41 
	
	replace educ_degree 	= 2 if (nivp == 30 | nivp == 40)
	
	replace educ_degree 	= 3 if nivp <= 20 
	
	label define educ_lbl 1 "No Tertiary Education" 2 "Vocational Training" 3 "Higher Education"
	
	label values educ_degree educ_lbl
	
	cap drop male
	
	gen male 		= (sexe == 1)
	
	gen female 		= (sexe == 0)
	
	gen not_married = (married == 0)
	
	gen child 		= enfant 
	
	gen no_child 	= (enfant == 0)
	
	gen age_sq 		= age^2 
	
	gen empnbh_cat 	= .
	
	replace empnbh_cat = 1  if empnbh <  20 
	replace empnbh_cat = 2  if empnbh >= 20 & empnbh < 25
	replace empnbh_cat = 3  if empnbh >= 25 & empnbh < 30
	replace empnbh_cat = 4  if empnbh >= 30 & empnbh < 35
	replace empnbh_cat = 5  if empnbh >= 35 & empnbh < 40
	replace empnbh_cat = 6  if empnbh >= 40 & empnbh < 45
	replace empnbh_cat = 7  if empnbh >= 45 & empnbh < 50
	replace empnbh_cat = 8  if empnbh >= 50 & empnbh < 55
	replace empnbh_cat = 9  if empnbh >= 55 & empnbh < 60
	replace empnbh_cat = 10 if empnbh >= 60 & empnbh < 65
	replace empnbh_cat = 11 if empnbh >= 65 
	
	label define empnbh_labels 	1  "-20"	2  "20–24" 3  "25–29" 4  "30–34" 5  "35–39" 6  "40–44" ///
								7  "45–49"  8  "50–54" 9   "55–59" 10  "60–64" 11  "65+" 
   
	label values empnbh_cat empnbh_labels
   
	drop salmet 
		
	label drop salmet_labels

	gen salmet = .

	replace salmet = 1  if salred < 500
	replace salmet = 2  if salred >= 500  & salred < 1000
	replace salmet = 3  if salred >= 1000 & salred < 1250
	replace salmet = 4  if salred >= 1250 & salred < 1500
	replace salmet = 5  if salred >= 1500 & salred < 2000
	replace salmet = 6  if salred >= 2000 & salred < 2500
	replace salmet = 7  if salred >= 2500 & salred < 3000
	replace salmet = 8  if salred >= 3000 & salred < 5000 
	replace salmet = 9  if salred >= 5000 & salred < 8000
	replace salmet = 10 if salred >= 8000

	replace salmet = 98 if salred == 98
	replace salmet = 99 if salred == 99

	label define salmet_labels 	1 "500-" 2 "500-1000" 3 "1000-1250" 4 "1250-1500" 5 "1500-2000" 6 "2000-2500" ///
								7 "2500-3000" 8 "3000-5000" 9 "5000-8000" 10 "8000+" 98 "Refus" 99 "Ne sait pas"

	label values salmet salmet_labels
	
	label variable salmet "Tranche de salaire de l'emploi principal"

save Data/Clean/df_wedges.dta, replace

****************************
**# Avg. Wedge by Year *****
**************************** 

	* Using OLS

use Data/Clean/p_df_wedges, clear
	
	xtset indiv_num datdeb
	
	global controls male female age_group educ_degree married not_married child no_child 
	
	reghdfe wedge i.annee $controls [pweight = extri], cluster(indiv_num) 
	
	margins annee

	marginsplot, ///
				name(margplot, replace) ///
				title("Unadjusted Wedge over Time - Linear Prediction") ///
				ytitle("Unadjusted Mean Wedge") ///
				xtitle("Year")
				
	graph export "Output/Figures/Wedge/mean_wedge.png", as(png) replace
	
	
	
***** Non-Monotonicity of Hourly wages *****

* MAKE SURE THIS FEATURE IS ROBUST (BREAK DOWN INTO SUB-GROUPS) * 

	* Figure 1 from Bick et al. 
	
	hist empnbh_cat 
	
	* Overall 
	
	preserve 
	
		keep if datdeb_q <= tq(2007q3)
	
		collapse (mean) salred, by(empnbh_cat)
		
		graph bar salred, over(empnbh_cat)
		
	restore 
	
	* By motivation type
		
	preserve 
		
		collapse (mean) salred, by(empnbh_cat stplc)
		
		graph bar salred, over(empnbh_cat) over(stplc)
	
	restore 
	

*****************************
**# Wedge Distributions *****
*****************************

use Data/Clean/p_df_wedges, clear
	
	global controls sexe age_group educ_degree married enfant
	
	preserve
		
		collapse (mean) prop_pos_wedge = pos_wedge (count) n=pos_wedge, by(empnbh_round)
		
		twoway (line prop_pos_wedge empnbh_round), ///
    ytitle("Proportion with Positive Wedge") ///
    xtitle("Weekly Hours Worked") ///
    title("Positive Wedge Rate by Hours Worked")
		

*****************************
**# WEDGE-HOURS Profile *****
*****************************

use Data/Clean/p_df_wedges, clear

	gen empnbh_round = ceil(empnbh)
	
	reghdfe wedge i.empnbh_cat $controls if datdeb_q <= tq(2007q3) [pweight=extri], absorb(datdeb) cluster(indiv_num)
	
	margins empnbh_cat, post 
	
	estimates store wedge_profile 
	
	coefplot wedge_profile, 	keep(*.empnbh_cat*) vertical ///
				drop(_cons) ///
				ciopts(recast(rcap)) msymbol(O) ///
				mcolor(black) lcolor(black) ///
				ylabel(, angle(horizontal)) ///
				xtitle("Actual Hours Bin") ytitle("") title("The Wedge-Hours Schedule (2003-2007)") ///
				xlabel(, angle(45))
	
	* Finding the kink? 
				
	twoway (lowess wedge empnbh), ///
    ytitle("Wedge") xtitle("Hours Worked") ///
    title("Smoothed Wedge Across Hours Worked")
	




****************************
**# WAGE-HOURS Profile *****
****************************	

use Data/Clean/p_df_wedges, clear
	
	global controls sexe age_group educ_degree married enfant
	
	reghdfe salred i.empnbh_cat $controls if datdeb_q <= tq(2007q3) [pweight=extri], absorb(datdeb) cluster(indiv_num)
	
	margins empnbh_cat, post 
	
	estimates store wage_profile
	
	coefplot wage_profile,	keep(*.empnbh_cat*) vertical ///
							drop(_cons) ///
							ciopts(recast(rcap)) msymbol(O) ///
							mcolor(black) lcolor(black) ///
							ylabel(, angle(horizontal)) ///
							xtitle("Hours Bin") ytitle("Predicted Monthly Wage (Adjusted)") ///
							title("The Wage-Hours Profile (2003-2007)") ///
							xlabel(, angle(45))
		
	graph export "Output/Figures/Wage_Hours_Profile/Wage_Hours_Profile_03-07.png", as(png) replace
	
	* log hourly wage 
	


							****** CONFUSED ABOUT THESE GRAPHS *****
					****** Wage Monotonically Decreasing in Hours ??? ******
								     ***** WEIGHTS ??? *****
									 
									 
*****************************
**# Wedge Heterogeneity *****
*****************************

use Data/Clean/df_wedges, clear 

	***** Aggregate 2003-2007 *****

    xtset indiv_num datdeb 

    global controls sexe age_group educ_degree married enfant

    global i_controls i.sexe i.age_group i.educ_degree i.married i.enfant

    reghdfe wedge $i_controls if datdeb_q <= tq(2007q3) [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model

    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }

    coefplot 	(m_sexe)(m_age_group) ///
				(m_educ_degree)(m_married)(m_enfant), ///
          horizontal ///
          ciopts(recast(rcap) lcolor(gs10)) ///
          mcolor(blue) msymbol(circle) ///
          title("Average Adjusted Wedge by Demographic Group (2003–2007, OG Sample)") ///
          nolabel legend(off) ///
          coeflabels(	1.sexe = "Male" 0.sexe = "Female" ///
						1.age_group = "18 - 24" ///
						2.age_group = "25 - 54" ///
						3.age_group = "55 - 64" ///
						1.educ_degree = "No Tertiary Education" ///
						2.educ_degree = "Vocational Training" ///
						3.educ_degree = "College Degree" ///
						1.married = "Married" 0.married = "Not Married" ///
						1.enfant = "Child in HH" 0.enfant = "No Child in HH"  ///
          )

    graph export "Output/Figures/Wedge_Heterogeneity/og_agg_wedge_H_03-07.png", as(png) replace 
	
**********************************
**# Wedge-Hours Distribution *****
**********************************

use Data/Clean/df_wedges, clear 

	preserve 
		
		keep if datdeb_q <= tq(2007q4)
		
		graph box wedge, over(empnbh_cat, label(angle(45))) ///
		title("Labor Supply Wedge by Hours Worked Bins") ///
		ylabel(, angle(horizontal))
		
		lowess wedge empnbh, ///
		title("Smoothed Labor Supply Wedge vs Hours Worked") ///
    ylabel(, angle(horizontal)) ///
    xtitle("Hours Worked") ytitle("Labor Supply Wedge")

		
	restore 

	preserve 
		
		keep if datdeb_q <= tq(2007q4)
	
	collapse (count) empnbh, by(empnbh_cat)
	gen total = sum(empnbh)
	gen density = empnbh / total
	
	twoway bar density empnbh_cat, ///
    ytitle("Density") xtitle("Hours Worked") ///
    title("Approximate Density of Labor Supply Wedge by Hours Worked")
	
	restore 

					
	* Log-Weekly Earnings 
	
	gen log_weekly_salred = log(weekly_salred)
	
	gen log_weekly_salmee = log(salmee/ 4)
	
	xtreg log_weekly_salred i.empnbh_cat $controls i.annee, fe robust cluster(indiv_num)
	
	xtreg log_weekly_salmee i.empnbh_cat $controls i.annee, fe robust cluster(indiv_num)
	
	* Log_Monthly Earnings 
	
	gen age_sq = age^2
	
	xtreg log_salred i.empnbh_cat $controls i.annee age age_sq, fe robust cluster(indiv_num)
	
	xtreg log_salmee i.empnbh_cat $controls i.annee age age_sq [pweight = new_weight], fe robust cluster(indiv_num)
		
	restore 
		
 
**************
**# CDFs ***** 
**************

use Data/Clean/df_wedges, clear

	sort indiv_num datdeb 
	
	* Generating CDF and mean values
	
	foreach var in empnbh hplus wedge {
		
		cumul `var', gen(cdf_`var'_yearly) by(annee)
		
		cumul `var', gen(cdf_`var'_agg) 
		
		cumul `var' if (datdeb_q <= tq(2007q3) & annee >= 2003), gen(cdf_`var'_03_07) 
		
		sum `var', meanonly
		
		global mean_`var'_agg = r(mean)
		
		sum `var' if (datdeb_q <= tq(2007q3) & annee >= 2003), meanonly 
		
		global mean_`var'_03_07 = r(mean)
	 
	}
	
	* Actual vs Desired Working Hours (2003-2007 aggregate)
	
	
	twoway	(line cdf_empnbh_03_07 empnbh, sort lwidth(thin) lcolor(blue)) ///
			(scatteri 0 $mean_empnbh_03_07 1.02 $mean_empnbh_03_07, recast(line) lpattern(dash) lcolor(blue) lwidth(thin)) ///
			(line cdf_hplus_03_07 hplus, sort lwidth(thin) lcolor(red)) ///
			(scatteri 0 $mean_hplus_03_07 1.02 $mean_hplus_03_07, recast(line) lpattern(dash) lcolor(red) lwidth(thin)), ///
			xtitle("Weekly Hours") ytitle("Empirical Cumulative Probability") ///
			title("ECDF of Actual vs. Desired Weekly Working Hours (2003-2007)") ///
			yscale(range(0 1)) ///
			legend(order(1 "Actual Hours" 2 "Mean Actual Hours" 3 "Desired Hours" 4 "Mean Desired Hours")ring(0) position(4))
		
	graph export "Output/Figures/CDFs/Weekly_Hours/og_weekly_hours_03-07.png", as(png) replace
	
	restore
	
	* Working vs desired Working Hours (year by year)
	
	forvalues i = 2003/2020 {
		
		foreach var in empnbh hplus{
			
			sum `var' if annee == `i', meanonly
			
			local mean_`var' = r(mean)			
		}
		
		twoway 	(line cdf_empnbh_yearly empnbh if annee == `i', sort lwidth(thin) lcolor(blue)) ///
				(line cdf_hplus_yearly hplus if annee == `i', sort lwidth(thin) lcolor(red)),  ///
				xline(`mean_empnbh', lpattern(dash) lcolor(blue) lwidth(thin)) ///
				xline(`mean_hplus', lpattern(dash) lcolor(red) lwidth(thin)) ///
				xtitle("Weekly Hours") ytitle("Cumulative Probability") ///
				title("CDF of Actual vs. Desired Weekly Working Hours (`i')") ///
				legend(order(1 "Actual Hours" 2 "Desired Hours") ring(0) position(4))
		
		graph export "Output/Figures/CDFs/Weekly_Hours/weekly_hours_`i'.png", as(png) replace
	}
	
	
	* The Wedge (2003-2020 aggregate)
	
	line cdf_wedge_agg wedge, sort lwidth(thin) lcolor(blue) ///
			xtitle("Wedge (Desired Hours - Actual Hours)") ///
			ytitle("Cumulative Probability") /// 
			xline($mean_wedge_agg, lpattern(dash) lwidth(thin) lcolor(blue) ) ///
			title("CDF of the Labor Supply Wedge (2003-2020)") 
	
	graph export "Output/Figures/CDFs/Wedge/wedge_agg.png", as(png) replace
	
	* The Wedge (year by year)
	
	forvalues i = 2003/2020 {
		
		sum wedge if annee == `i', meanonly
			
		local mean_wedge = r(mean)
		
		line cdf_wedge_yearly wedge if annee == `i', sort lwidth(thin) lcolor(blue) ///
				xtitle("Wedge (Hours Worked - Hours Desired)") ///
				ytitle("Cumulative Probability") /// 
				title("CDF of the Labor Supply Wedge (`i')") ///
				xline(`mean_wedge', lpattern(dash) lwidth(thin) lcolor(blue))

	graph export "Output/Figures/CDFs/Wedge/wedge_`i'.png", as(png) replace
	
	}
	
**********************************************************
**# Wedge to Log_wage & to Hours Worked Relationship *****
**********************************************************

use Data/Clean/p_df_wedges, clear

	xtile quantile_empnbh 		= log_empnbh, n(20)  			
	
	xtile quantile_salred 	= log_salred, n(20) 	
	
	
	* Log-Weekly Hours 
	
	preserve	
		collapse (mean) wedge log_empnbh, by(quantile_empnbh)

		twoway 	(scatter wedge log_empnbh, msymbol(o) mcolor(blue)) ///
				(lfit wedge log_empnbh, lcolor(red%75) lpattern(dash)), ///
				xtitle("Log Weekly Hours") ytitle("Wedge") ///
				title("Scatter Plot with Fitted Line")
	restore 
	
	* Log-Monthly Income
	
	preserve 
		collapse (mean) wedge log_salred, by(quantile_salred)
	
		twoway 	(scatter wedge log_salred, msymbol(o) mcolor(blue)) ///
				(lfit wedge log_salred, lcolor(red%75) lpattern(dash)), ///
				xtitle("Log Monthly Income") ytitle("Wedge") ///
				title("Scatter Plot with Fitted Line")
	restore 
	
**************************
**# Frish Elasticity *****
**************************

	xtset indiv_num datdeb
	
	gen dlog_empnbh = .
	
	gen dlog_salred = . 
	
	bysort indiv_num (datdeb): replace dlog_empnbh = log_empnbh - log_empnbh[_n-1]
	
	bysort indiv_num (datdeb): replace dlog_salred = log_salred - log_salred[_n-1]

	reg dlog_empnbh dlog_salred, cluster(indiv_num)
	
	* Very Low --> Issue with the wage data!!!! What is the best way to spread? 
	

********************************
**# Regression Analaysis *******
********************************




***** Demographic Characteristics on the Wedge (Heterogeneity) *****

***** Evolution of Hourly Wage by Hours Worked Bin *****

***************
***** DiD *****
***************	

	* Looking at effect on desired hours worked is NOVEL??

use Data/Clean/p_df_wedges, clear

	keep if in_tepa  //& hhc != . & hplus != . & salred != . 
	
	global i_controls i.sexe i.age_group i.educ_degree i.married i.enfant
	
	global c_controls CBC gs_gdp

***** Cross-Border Workers ***** 
	
	gen treatment = (dom_bel == 1 | dom_ger == 1 | dom_swz == 1 | dom_lux == 1) & france == 1
	
	gen control = (trans_bel == 1 | trans_ger == 1| trans_swz == 1 | trans_lux == 1)

	keep if control | treatment
	
	gen post_treatment = treatment * post_tepa 
	
	gen triple_d_m = treatment * manager * post_tepa 
	
	gen triple_d_l = treatment * laborer * post_tepa 
	
	gen trip_motiv = treatment * stplc * post_tepa 
	
	
	*** Pre-Trends *** 
	
	gen rel_time = tq(2008q1) - datdeb_q 			// (t - F + 1)
	
	replace rel_time = rel_time + 4 
	
	reghdfe wedge i.rel_time, absorb(treatment) cluster(indiv_num)
	
	pretrends power numpre() 0.5 
	
	  
	 
	*** Regressions ***
	
	* Y = wedge  
	
	reghdfe wedge post_treatment treatment $controls $c_controls [pweight=extri], absorb(indiv_num datdeb) cluster(indiv_num) // 
	
	* Y = hplus (desired weekly hours)
	
	reghdfe hplus post_treatment treatment $controls $c_controls [pweight=extri], absorb(indiv_num datdeb) cluster(indiv_num) // ***

	* Y = empnbh (hours worked that week)
	
	reghdfe empnbh post_treatment treatment $controls $c_controls [pweight=extri], absorb(indiv_num) cluster(indiv_num) 
	
	* Y = emphre (OT hours)
	
	reghdfe emphre post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(indiv_num) cluster(indiv_num) // *** 
	
	
	
	
***** Small Firms (<2 employees) vs. Independent Workers *****

use Data/Clean/p_df_wedges, clear
	
	keep if in_tepa 
	
	gen treatment = 
	
	
	
	

	
	*** Colonne (1) et (2) : regression pour l'ensemble des categories professionnelles
xttab dumoct07 if echant_indpt==1  &  treatment!=1 & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" 
xttab dumoct07 if echant_indpt==1 &  treatment==1 & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" 


*** Colonne (3) et (4) : regression pour l'ensemble des categories professionnelles en Log
xi: xtreg lempnbh dum_treat_sal_1  dummy  if echant_indpt==1  & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" [weight=extrim], fe robust cluster(num_indiv) 
xi: xtreg lempnbh dum_treat_sal_1 treat_et_round  dummy round if echant_indpt==1   & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" [weight=extrim], fe robust cluster(num_indiv) 

*** Colonne 5) et (6) : regression pour l'artisanat
xttab dumoct07 if echant_art==1 &  treatment!=1 & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" 
xttab dumoct07 if echant_art==1 &  treatment==1 & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" 

*** Colonne (7) et (8) : regression pour le commerce
xttab dumoct07 if echant_com==1 &  treatment!=1 & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" 
xttab dumoct07 if echant_com==1 &  treatment==1 & empnbh6>3 & exclu0!=1 & exclu1!=1 & empnbh<dur_trav  & nafg16!="EA" & nafg16!="ER" & nafg16!="EQ" 


***** Bunching at 35hours ***** 

use Data/Clean/p_df_wedges, clear
	
	keep if in_tepa 
	
	gen treatment = (empnbh == 40 & (emphre == 0 | emphre == .) & wedge >0 )

	egen sd_OT = sd(emphre), by(indiv_num)
	
	gen constant_OT = (sd_OT == 0)
	
	gen control = (constant_OT == 1 & treatment == 0 & empnbh > 40)
	
	keep if treatment | control
	
	gen post_treatment = treatment * post_tepa 
	
	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num)
	
	
	
	
	
	
	
	
	* Pooled Sample 
	
	xtreg overworked sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc,fe robust cluster(indiv_num) // signig hhc (makes sense) 
	
	xtreg wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc,fe robust cluster(indiv_num) // signif hhc (makes sense) 
	
	* Cross Section
	
	collapse (mean) overworked wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc, by (indiv_num annee)
	
	reg wedge sexe i.age_group i.educ_degree log_salred log_empnbh if annee == 2019, robust // signig hhc (makes sense) 
	
	
	