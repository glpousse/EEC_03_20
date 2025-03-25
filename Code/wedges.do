**************************
**************************
********* WEDGES ********* 
**************************
**************************

use Data/Clean/df4_master_final, clear

	* Past 8000 hhc goes down 
	
	

***********************
***** DEFINITIONS *****
***********************

	keep if tppred == 1 // Let's look at full time employees
	
	drop if (stplc == 9 | stplc == .) 	// Drop those who "do not know" their motivation
	
	replace stplc = 0 if stplc == 2 	// Making binary for analysis

***** New Vars *****

	bysort annee: keep if hplus !=. & hhc !=. & salred != .
	
	gen wedge 		      = empnbh - hplus
	
	gen overworked   	  = (wedge < 0)
		
	gen underworked       = (wedge > 0)
	
	gen worked 		      = (wedge==0)
	
	gen log_salred 	      = log(salred)
	
	gen weekly_salred     = salred/ 4 
	
	gen hourly_salred 	  = weekly_salred / empnbh 
	
	ge log_hourly_salred  = log(hourly_salred)
	
	gen log_empnbh 	      = log(empnbh)
	
	gen married           = (mpr == 2 | matriprm == 2)
	
	gen age_18_35		  = (age <= 35 & age >=18) 
	
	gen age_36_55 		  = (age >= 36 & age <= 55)
	
	gen age_56_64 		  = (age >=56 & age <= 64)
	
	gen college_degree	  = (nivp <= 30)
	
	gen vocational_degree = (nivp <= 50) 
	
	gen empnbh_cat = .
	
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
	replace empnbh_cat = 11 if empnbh >= 65 & empnbh < 70
	replace empnbh_cat = 12 if empnbh >= 70 & empnbh < 75 
	replace empnbh_cat = 13 if empnbh >= 75 & empnbh < 80
	replace empnbh_cat = 14 if empnbh >= 80
	
	label define empnbh_labels ///
	1  "-20"   ///
	2  "20–24" ///
    3  "25–29" ///
    4  "30–34" ///
    5  "35–39" ///
    6  "40–44" ///
    7  "45–49" ///
    8  "50–54" ///
    9   "55–59" ///
   10  "60–64" ///
   11  "65–69" ///
   12  "70–74" ///
   13  "75–79" ///
   14  "80+"
   
   label values empnbh_cat empnbh_labels
		

save Data/Clean/df_wedges.dta, replace


***************************
**# DESCRIPTIVE FACTS *****
***************************

use Data/Clean/df_wedges, clear

***** Avg. Wedge by Year ***** 





***** Hours worked over salary brackets *****

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

	label define salmet_labels ///
    1 "500-" ///
    2 "500-1000" ///
    3 "1000-1250" ///
    4 "1250-1500" ///
    5 "1500-2000" ///
    6 "2000-2500" ///
    7 "2500-3000" ///
    8 "3000-5000" ///
    9 "5000-8000" ///
    10 "8000+" ///
    98 "Refus" ///
    99 "Ne sait pas"

	label values salmet salmet_labels
	
	label variable salmet "Tranche de salaire de l'emploi principal"

	graph bar empnbh, over(salmet) 
	
	
***** Non-Monotonicity of Hourly wages *****

* MAKE SURE THIS FEATURE IS ROBUST (BREAK DOWN INTO SUB-GROUPS) * 

	* Figure 1 from Bick et al. 
	
	hist empnbh_cat 
	
	* Overall 
	
	preserve 
	
		collapse (mean) salred, by(empnbh_cat)
		
		graph bar salred, over(empnbh_cat)
		
	restore 
	
	* By motivation type
		
	preserve 
		
		collapse (mean) salred, by(empnbh_cat stplc)
		
		graph bar salred, over(empnbh_cat) over(stplc)
	
	restore 
	
***** Cross-Sectional Relationship between Wages and Hours by Motivation Type *****

use Data/Clean/df_wedges, clear
	
	egen N = count(log_hourly_salred), by(empnbh_cat stplc)

	preserve 

		collapse (mean) log_hourly_salred_mean 	= log_hourly_salred ///
				 (mean) count 					= N 		 ///
				 (sd)	log_hourly_salred_sd 	= log_salred, by(empnbh_cat stplc)
		
		gen se = log_hourly_salred_mean/ sqrt(count)
		
		gen ci_upper = log_hourly_salred_mean + 1.96 * se 

		gen ci_lower = log_hourly_salred_mean - 1.96 * se 
		
		twoway ///
			rarea ci_upper ci_lower empnbh_cat if stplc == 1, color(gs8%50) || ///
			rarea ci_upper ci_lower empnbh_cat if stplc == 0, color(gs11%50) || ///
			line log_hourly_salred_mean empnbh_cat if stplc == 0, sort ///
			lcolor(black) lwidth(medthick) ///
			mcolor(black) msize(small) msymbol(triangle) || ///
			line log_hourly_salred_mean empnbh_cat if stplc == 1, sort ///
			lcolor(gs7) lwidth(medthick) ///
			mcolor(gs7) msize(small) msymbol(square), ///
			legend(order(3 "stplc = 0" 4 "stplc = 1")) ///
			xtitle("Hours Worked Category") ///
			ytitle("Log of Red. Salary") ///
			title("Log Salary by Hours Worked and stplc") ///
			xlabel(#14, valuelabel angle(45) labsize(small)) 
		
		graph export "Output/Figures/Log_Salary_by_Hours/"
		
	restore 
	
	
	





**************
**# CDFs ***** 
**************

	sort indiv_num datdeb 
	
	* Generating CDF and mean values
	
	foreach var in empnbh hplus wedge {
		
		cumul `var', gen(cdf_`var'_yearly) by(annee)
		
		cumul `var', gen(cdf_`var'_agg) 
		
		sum `var', meanonly
		
		global mean_`var'_agg = r(mean)
	 
	}
	
	* Actual vs Desired Working Hours (2003-2020 aggregate)
	
	twoway	(line cdf_empnbh_agg empnbh, sort lwidth(thin) lcolor(blue)) ///
			(line cdf_hplus_agg hplus, sort lwidth(thin) lcolor(red)), ///
			xline($mean_empnbh_agg, lpattern(dash) lcolor(blue) lwidth(thin)) ///
			xline($mean_hplus_agg, lpattern(dash) lcolor(red) lwidth(thin)) ///
			xtitle("Weekly Hours") ytitle("Cumulative Probability") ///
			title("CDF of Actual vs. Desired Weekly Working Hours (2003-2020)") ///
			legend(order(1 "Actual Hours" 2 "Desired Hours")ring(0) position(4))
		
	graph export "Output/Figures/CDFs/Weekly_Hours/weekly_hours_agg.png", as(png) replace
	
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
			xtitle("Wedge (Hours Worked - Hours Desired)") ///
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
	
****************
**# Trends ***** 
****************

	preserve
		collapse (mean) empnbh hplus wedge, by(datdeb_q)
		
		twoway 	(line empnbh datdeb_q, yaxis(1)) ///
				(line hplus datdeb_q, yaxis(1)) ///
				(line wedge datdeb_q, yaxis(2)) , ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) 
	restore 

	preserve 
		collapse (mean) empnbh hplus wedge, by(datdeb_q stplc)

		twoway 	(line empnbh datdeb_q, yaxis(1) by(stplc)) ///
				(line hplus datdeb_q, yaxis(1) by(stplc)) ///
				(line wedge datdeb_q, yaxis(2) by(stplc)) , ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) 		
	restore 	
	
*****************************
**# Wedge Heterogeneity *****
*****************************

use Data/Clean/df_wedges, clear 

	***** Overall Panel (2003-2020) *****

	* AGGREGATE MOTIVATION

	su wedge 

	global avg_wedge = r(mean)

	matrix results = J(16, 3, .)
	
	local row = 1 
	
	foreach var in sexe age_18_35 age_36_55 age_56_64 vocational_degree college_degree married enfant {
		
		qui su wedge if `var' 		== 1 // ERROR: I need all the other controls here to be equal to ZERO
		
		matrix results[`row',1] 	= `row'
		
		matrix results[`row',2] 	= r(mean) 
		
		matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
		local row = `row' + 1 
		
		qui su wedge if `var' 		== 0 
		
		matrix results[`row',1] 	= `row'
		
		matrix results[`row',2] 	= r(mean) 
		
		matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
		local row = `row' + 1 
	}
		
	preserve 
	
		clear 

		svmat results, names(col)
	
		rename c1 category 
	
		drop if (category == 4 | category == 6 | category == 8 | category == 10 | category == 12) 
	
		rename c2 mean 
	
		rename c3 se
	
		gen lower_ci = mean - 1.96 * se
	
		gen upper_ci = mean + 1.96 * se 
	
		replace category = _n 
	
		twoway  (scatter category mean, msymbol(O) mcolor(black)) /// 
			(rcap lower_ci upper_ci category, horizontal lcolor(black)), /// 
			ylabel(1 "Male" 2 "Female" 3 "18-35" 4 "36-55" 5 "56-64" 6 "Vocational" 7 "College" 8 "Married" 9 "Not Married" 10 "Child in HH" 11 "No Child in HH", angle(0)) ///
			ytitle("") xtitle("Mean Hours Gap (Wedge)") ///
			title("Mean Hours Gap with 95% Confidence Intervals (2003-2020)") ///
			xline($avg_wedge, lpattern(dash) lcolor(black)) ///
			legend(off) yscale(reverse)
	
		graph export "Output/Figures/Wedge_Heterogeneity/wedge_H_agg.png", as(png) replace
	restore 


	* BY MOTIVATION GROUP (STPLC = 1 & STPLC = 2) 

use Data/Clean/df_wedges, clear

	su wedge if stplc == 1 

	global avg_wedge_1 = r(mean) 
	
	su wedge if stplc == 2
	
	global avg_wedge_2 = r(mean) 
	
	su wedge 
	
	global avg_wedge = r(mean) 

	matrix results = J(16, 7, .)
	
	local row = 1 
	
	foreach var in sexe age_18_35 age_36_55 age_56_64 vocational_degree college_degree married enfant {
		
		qui su wedge if `var' 		== 1 
		
		matrix results[`row',1] 	= `row'
		
		matrix results[`row',2] 	= r(mean)  
		
		matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 1 & stplc == 1 
		
		matrix results[`row',4] 	= r(mean) 
		
		matrix results[`row',5] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 1 & stplc == 2 
		
		matrix results[`row',6] 	= r(mean) 
		
		matrix results[`row',7] 	= r(sd)/ sqrt(r(N)) 
		
		local row = `row' + 1 
		
		qui su wedge if `var' 		== 0 
		
		matrix results[`row',1] 	= `row'
		
		matrix results[`row',2] 	= r(mean) 
		
		matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 0 & stplc == 1 
		
		matrix results[`row',4] 	= r(mean) 
		
		matrix results[`row',5] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 0 & stplc == 2 
		
		matrix results[`row',6] 	= r(mean) 
		
		matrix results[`row',7] 	= r(sd)/ sqrt(r(N)) 
		
		local row = `row' + 1 
	}
		
	preserve 
	
		clear 

		svmat results, names(col)
	
		rename c1 category 
	
		drop if (category == 4 | category == 6 | category == 8 | category == 10 | category == 12) 
	
		rename c2 mean_agg
	
		rename c3 se_agg
		
		rename c4 mean_1 
		
		rename c5 se_1 
		
		rename c6 mean_2 
		
		rename c7 se_2
		
		foreach var in agg 1 2 {
			
			gen lower_ci_`var' = mean_`var' - 1.96 * se_`var'

			gen upper_ci_`var' = mean_`var' + 1.96 * se_`var'
			
		}

		replace category = _n 
	
		twoway  (scatter category mean_agg, msymbol(O) mcolor(black)) /// 
				(scatter category mean_1, msymbol(O) mcolor(blue)) ///
				(scatter category mean_2, msymbol(O) mcolor(red)) ///
			(rcap lower_ci_agg upper_ci_agg category, horizontal lcolor(black)) /// 
			(rcap lower_ci_1 upper_ci_1 category, horizontal lcolor(blue)) /// 
			(rcap lower_ci_2 upper_ci_2 category, horizontal lcolor(red)), /// 
			ylabel(1 "Male" 2 "Female" 3 "18-35" 4 "36-55" 5 "56-64" 6 "Vocational" 7 "College" 8 "Married" 9 "Not Married" 10 "Child in HH" 11 "No Child in HH", angle(0)) ///
			ytitle("") xtitle("Mean Hours Gap (Wedge)") ///
			title("Mean Hours Gap with 95% Confidence Intervals (2003-2020)") ///
			xline($avg_wedge, lpattern(dash) lcolor(black)) ///
			xline($avg_wedge_1, lpattern(dash) lcolor(blue)) ///
			xline($avg_wedge_2, lpattern(dash) lcolor(red)) ///
			legend(order(1 "Aggregate" 2 "Motivated" 3 "Not Motivated") ///
       position(2) ring(0) col(1) region(lstyle(none)) region(lstyle(none) fcolor(none)))
	   
		graph export "Output/Figures/Wedge_Heterogeneity/by_stplc_type/wedge_H_agg_stplc.png", as(png) replace
	restore 
	
	***** Year by Year *****
	
	* I want to input a graph that has time on the x and values on the y axis. The goal is to make a connected fot chart, with each dot representing an average value for a specific group that year, since each group has relativey different values. There will be one distinguishible line per group which I will label. With this it will be easy to study that varibales trend across heterogeneous groups. I also want each dot to have its CI going vertically through it. 

	forvalues i = 2003/2020 {	

		use Data/Clean/df_wedges, clear  
		
		keep if annee == `i'
		
		su wedge 

		global avg_wedge = r(mean)

		matrix results = J(16, 3, .)
	
		local row = 1 
	
		foreach var in sexe age_18_35 age_36_55 age_56_64 vocational_degree college_degree married enfant {
		
			qui su wedge if `var' 		== 1 // I need all the other controls here to be equal to ZERO?
		
			matrix results[`row',1] 	= `row'
		
			matrix results[`row',2] 	= r(mean) 
		
			matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
			local row = `row' + 1 
		
			qui su wedge if `var' 		== 0 
		
			matrix results[`row',1] 	= `row'
			
			matrix results[`row',2] 	= r(mean) 
		
			matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
			local row = `row' + 1 
	}
		
		preserve 
	
		clear 

		svmat results, names(col)
	
		rename c1 category 
	
		drop if (category == 4 | category == 6 | category == 8 | category == 10 | category == 12) 
	
		rename c2 mean 
	
		rename c3 se
	
		gen lower_ci = mean - 1.96 * se
	
		gen upper_ci = mean + 1.96 * se 
	
		replace category = _n 
	
		twoway  (scatter category mean, msymbol(O) mcolor(black)) /// 
			(rcap lower_ci upper_ci category, horizontal lcolor(black)), /// 
			ylabel(1 "Male" 2 "Female" 3 "18-35" 4 "36-55" 5 "56-64" 6 "Vocational" 7 "College" 8 "Married" 9 "Not Married" 10 "Child in HH" 11 "No Child in HH", angle(0)) ///
			ytitle("") xtitle("Mean Hours Gap (Wedge)") ///
			title("Mean Hours Gap with 95% Confidence Intervals (`i')") ///
			xline($avg_wedge, lpattern(dash) lcolor(black)) ///
			legend(off) yscale(reverse)
	
		graph export "Output/Figures/Wedge_Heterogeneity/wedge_H_`i'.png", as(png) replace
		
		restore 
		
		
	use Data/Clean/df_wedges, clear
	
	keep if annee == `i'

	su wedge if stplc == 1 

	global avg_wedge_1 = r(mean) 
	
	su wedge if stplc == 2
	
	global avg_wedge_2 = r(mean) 
	
	su wedge 
	
	global avg_wedge = r(mean) 

	matrix results = J(16, 7, .)
	
	local row = 1 
	
	foreach var in sexe age_18_35 age_36_55 age_56_64 vocational_degree college_degree married enfant {
		
		qui su wedge if `var' 		== 1 
		
		matrix results[`row',1] 	= `row'
		
		matrix results[`row',2] 	= r(mean)  
		
		matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 1 & stplc == 1 
		
		matrix results[`row',4] 	= r(mean) 
		
		matrix results[`row',5] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 1 & stplc == 2 
		
		matrix results[`row',6] 	= r(mean) 
		
		matrix results[`row',7] 	= r(sd)/ sqrt(r(N)) 
		
		local row = `row' + 1 
		
		qui su wedge if `var' 		== 0 
		
		matrix results[`row',1] 	= `row'
		
		matrix results[`row',2] 	= r(mean) 
		
		matrix results[`row',3] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 0 & stplc == 1 
		
		matrix results[`row',4] 	= r(mean) 
		
		matrix results[`row',5] 	= r(sd)/ sqrt(r(N)) 
		
		qui su wedge if `var' 		== 0 & stplc == 2 
		
		matrix results[`row',6] 	= r(mean) 
		
		matrix results[`row',7] 	= r(sd)/ sqrt(r(N)) 
		
		local row = `row' + 1 
	}
		
	preserve 
	
		clear 

		svmat results, names(col)
	
		rename c1 category 
	
		drop if (category == 4 | category == 6 | category == 8 | category == 10 | category == 12) 
	
		rename c2 mean_agg
	
		rename c3 se_agg
		
		rename c4 mean_1 
		
		rename c5 se_1 
		
		rename c6 mean_2 
		
		rename c7 se_2
		
		foreach var in agg 1 2 {
			
			gen lower_ci_`var' = mean_`var' - 1.96 * se_`var'

			gen upper_ci_`var' = mean_`var' + 1.96 * se_`var'
			
		}

		replace category = _n 
	
		twoway  (scatter category mean_agg, msymbol(O) mcolor(black)) /// 
				(scatter category mean_1, msymbol(O) mcolor(blue)) ///
				(scatter category mean_2, msymbol(O) mcolor(red)) ///
			(rcap lower_ci_agg upper_ci_agg category, horizontal lcolor(black)) /// 
			(rcap lower_ci_1 upper_ci_1 category, horizontal lcolor(blue)) /// 
			(rcap lower_ci_2 upper_ci_2 category, horizontal lcolor(red)), /// 
			ylabel(1 "Male" 2 "Female" 3 "18-35" 4 "36-55" 5 "56-64" 6 "Vocational" 7 "College" 8 "Married" 9 "Not Married" 10 "Child in HH" 11 "No Child in HH", angle(0)) ///
			ytitle("") xtitle("Mean Hours Gap (Wedge)") ///
			title("Mean Hours Gap with 95% Confidence Intervals (`i')") ///
			xline($avg_wedge, lpattern(dash) lcolor(black)) ///
			xline($avg_wedge_1, lpattern(dash) lcolor(blue)) ///
			xline($avg_wedge_2, lpattern(dash) lcolor(red)) ///
			legend(order(1 "Aggregate" 2 "Motivated" 3 "Not Motivated") ///
       position(2) ring(0) col(1) region(lstyle(none)) region(lstyle(none) fcolor(none)))
	   
		graph export "Output/Figures/Wedge_Heterogeneity/by_stplc_type/wedge_H_`i'_stplc.png", as(png) replace
	restore 
	
	}

	
/*	global controls age_18_35 age_36_55 age_56_64 vocational_degree college_degree married

	foreach var of global controls {

    * Build condition: all other control vars must equal 0
		local cond
		
			foreach other of global controls {
			
			if "`other'" != "`var'" {
			
				local cond `cond' & `other' == 0
        }
    }

    * Generate variable: wedge_`var' equals wedge only when var == 1 AND others == 0
		gen wedge_`var' = .
	
		quietly replace wedge_`var' = wedge if `var' == 1 `cond'
}

	local cond & controls & married 

	gen wedge_male 			= wedge if sexe == 1 & 
	
	gen wedge_female 		= wedge if sexe == 0
	
	gen wedge_not_married 	= wedge if married == 0
	
	gen wedge_child 		= wedge if enfant == 1
	
	gen wedge_no_child 		= wedge if enfant == 0 
*/


**********************************************************
**# Wedge to Log_wage & to Hours Worked Relationship *****
**********************************************************

	xtile quantile_empnbh 		= log_hhc, n(20)  			
	
	xtile quantile_salred 	= log_salred, n(20) 	
	
	
	* Log-Weekly Hours 
	
	preserve	
		collapse (mean) wedge log_hhc, by(quantile_hhc)

		twoway 	(scatter wedge log_hhc, msymbol(o) mcolor(blue)) ///
				(lfit wedge log_hhc, lcolor(red%75) lpattern(dash)), ///
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
hist hhc, ///
xlabel(0(5)100)
	
***********************
**# Regressions *******
***********************

******************
***** Linear *****
******************


***** Demographic Characteristics on the Wedge (Heterogeneity) *****

***** Evolution of Hourly Wage by Hours Worked Bin *****


	

	
	



***************
***** DiD *****
***************

	* COULD IT BE BECAUSE I DO NOT CONTROL FOR COUNTRIES ECONOMICS SITUAATIONS???
	

use Data/Clean/df_wedges, clear

	keep if in_tepa & hhc != . & hplus != . & salred != . 
		
	xtset indiv_num datdeb 
	
	global i_controls sexe age_18_35 age_36_55 age_56_64 vocational_degree college_degree married enfant
	
	global c_controls CBC gs_gdp

***** CaCaJole ID Strat ***** 

	keep if dom_border | trans_border
	
	gen treatment = dom_border
	
	gen post_treatment = treatment * post_tepa 
	
	gen triple_d_m = treatment * manager * post_tepa 
	
	gen triple_d_l = treatment * laborer * post_tepa 
	
	gen trip_motiv = treatment * stplc * post_tepa 
	
	*** Parallel Trends *** 
	
	preserve 
	
		collapse (mean) wedge, by(datdeb_q treatment) 
	twoway (line wedge datdeb_q if treatment==1, sort) ///
       (line wedge datdeb_q if treatment==0, sort), ///
       legend(label(1 "Treated") label(2 "Control")) ///
       title("Pre-treatment Trends")
	  
	 restore 

	
	* Y = wedge  
	
	xtreg wedge post_treatment treatment $i_controls CBC gs_gdp ,robust cluster(indiv_num)

	
	
	
	* Y = hplus (desired weekly hours)
	
	xtreg hplus post_treatment treatment $controls $c_controls, fe robust cluster(indiv_num) 	// *** 
	
	xtreg hplus trip_motiv treatment $controls $c_controls, fe robust cluster(indiv_num) 
	
	xtreg hplus post_treatment treatment salred stplc, fe robust cluster(indiv_num)
	
	
	
	* Y = emphre (OT hours)
	
	xtreg emphre post_treatment treatment $i_controls $c_controls, fe robust cluster(indiv_num)
	
	xtreg emphre triple_d_m post_treatment treatment $controls $c_controls, fe robust cluster(indiv_num)
	
	
	* Y = hhc (hours worked)
	

***** ID Strat *****
	
	use Data/Clean/df_wedges, clear 
	
	
	
	
	
	
	
	
	
	


	
	

	

	
	
	
	* Pooled Sample 
	
	xtreg overworked sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc,fe robust cluster(indiv_num) // signig hhc (makes sense) 
	
	xtreg wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc,fe robust cluster(indiv_num) // signif hhc (makes sense) 
	
	* Cross Section
	
	collapse (mean) overworked wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc, by (indiv_num annee)
	
	reg wedge sexe age_36_55 age_56_64 married college_degree vocational_degree log_salmee log_hhc if annee == 2019, robust // signig hhc (makes sense) 
	
	

