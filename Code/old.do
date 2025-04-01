********************
***** Old Code *****
********************


clear all
cd "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%% Data Prep/Cleaning Script %%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

********************************
**# Making the master data ***** 
********************************

	forvalues i = 2003/2020{
	
	use Raw/qc`i'.dta, clear 

		display "running on year `i'"
	
	* Making sure all vars are lower case 
		quietly foreach var of varlist * {
			local lowername = lower("`var'")
			rename `var' `lowername'
		}
	* Generating unique ID and a month var for CPI merge: 
		cap drop indiv
		gen indiv = ident + noi 
		label variable indiv "NI d'individu"
		cap drop mois
		cap tostring datdeb, replace
		gen mois = substr(datdeb, 5, 2) 
		label variable mois "Mois de l'enquête"
	
	* Converting "datdeb" to actual date
		gen var1 = date(datdeb, "YMD")
		format var1 %td
		drop datdeb 
		rename var1 datdeb
		order indiv annee mois datdeb 
	
	* CPI, UE, merge
		cap destring annee mois trim, replace
		merge m:1 annee mois using "Inflation/CPI_base_2015.dta"
		drop _merge 
		drop if annee != `i'
		merge m:1 annee trim using "Unemployment/ueq_FR.dta" // Finding data by departement? 
		drop _merge
		drop if annee != `i'
		merge m:1 annee mois using "SMIC/smic_FR.dta"
		drop _merge 
		drop if annee != `i'
		lab var smic_h "SMIC Horaire"
		lab var smic_m "SMIC Mensuel"

	// France metro:
	//cap destring metrodom reg, replace
	//cap keep if metrodom == 1 
	//cap drop if deparc > "95" 
	//cap drop if dep > "95"
	//foreach j in 9A  9B  9C  9D  9E  9F  9G  9H  9I  9J  9K  9L  9M {
	//	cap drop if dep == "`j'"
	//}
	//foreach j in 9A  9B  9C  9D  9E  9F  9G  9H  9I  9J  9K  9L  9M {
	//	cap drop if depeta == "`j'"
	//}
	//cap drop if (reg == 1 | reg == 2 | reg == 3 | reg == 4)
	
	// Population Active 
	//cap destring retrai acteu reg res, replace 
	//cap keep if (acteu == 1 | acteu ==.)  
	//cap drop if retrai == 1 // in case  
	//cap drop if (res == 16 | res == 14 | ret == 1) // ménage composé exclusivement d'inactifs de 65 ans et plus
	
	// Data set appending all years is too big to quickly run commands. So I'm dropping some variables I won't be needing: 
	//foreach var in acteu_drap dnai scj noech otravm otravp index cateaav2020 compq dens2020 spr sexeprm sexeprmcj mob noi* ident rea creant redem acessep fodeba fodebm fofina fofinm fordur fortyp fc9a enbsaa opa enbsab durstg age3 age5 ag ag5 ag3 ag3b agcj agecj ageprm ageprmcj agprm agprmcj agepr metrodom ageq agq ancrech ancinact temp nm* form zus rgi rga rgl rstg datult nafant* dmm* presnoi* lnaisp lnaism pent dnaip dnaim naimer naiper idaire tur5 mra mrb mrc mrd mre mrh mrj mrn mro forniv cspp cspm res datcoll spr00 spr01 spr02 spr03 spr04 spr05 spr06 spr07 spr08 spr09 spr10 spr11 spr12 cov* nresid nfrp nfrm salpar{
	//cap drop `var'
	
	* Stringing everything to make append possible
	
		quietly tostring *, replace 
		
	save Clean/df_`i'.dta, replace
	
	}
	
	
	

***** Log Salary by Hours (just taking the means, not accurate)

use Data/Clean/df_wedges, clear
	preserve 
	
		egen N = count(log_hourly_salred), by(empnbh_cat)

		collapse (mean) log_hourly_salred_mean 	= log_hourly_salred ///
				 (mean) count 					= N 		 ///
				 (sd)	log_hourly_salred_sd 	= log_salred, by(empnbh_cat)
		
		gen se = log_hourly_salred_mean/ sqrt(count)
		
		gen ci_upper = log_hourly_salred_mean + 1.96 * se 

		gen ci_lower = log_hourly_salred_mean - 1.96 * se 
		
		twoway ///
			rarea ci_upper ci_lower empnbh_cat, color(gs8%50) || ///
			line log_hourly_salred_mean empnbh_cat, sort ///
			lcolor(black) lwidth(medthick) ///
			mcolor(black) msize(small) msymbol(triangle) || ///
			, ///
			legend(off) ///
			xtitle("Hours Worked Category") ///
			ytitle("Log Hourly Salary") ///
			title("Log Salary by Hours Worked (2003-2020 agg.)") ///
			xlabel(#14, valuelabel angle(45) labsize(small)) 
		
		graph export "Output/Figures/Log_Salary_by_Hours/Log_sal_hours_agg.png", as(png) replace 
		
	restore 
	
*******************************************
**# Computing Raw Wedge Means with CIs **** 
*******************************************

	* Standard mean reporting 
	
	mean wedge, over(annee) 	 // Table
	
	matrix list r(table)
	
	matrix wedge_stats = r(table)
	
	preserve
	
		clear 
		
		svmat wedge_stats, names(row)
		
		forvalues i = 1/18 {
			local annee = 2002 + `i'
			rename row`i' year_`annee'
}

		keep if (_n == 1 | _n == 2 | _n == 5 | _n == 6)
		
		gen str stat = ""
		
		replace stat = "mean" 	if _n ==1 
		replace stat = "se" 	if _n ==2
		replace stat = "lower_ci" if _n ==3 
		replace stat = "upper_ci" if _n ==4
		
		order stat
		
		reshape long year_, i(stat) j(year)
		
		rename year_ value
		
		reshape wide value, i(year) j(stat) string
		
		rename valuemean mean
		rename valuelower_ci ci_low
		rename valueupper_ci ci_high
		
		twoway ///
			(rcap ci_high ci_low year, lcolor(gs8)) ///
			(scatter mean year, mcolor(blue) msymbol(circle)) ///
			(scatteri -7 2007 -3 2007, recast(line) lpattern(dash) lcolor(red) lwidth(thin)) ///
			(scatteri -7 2012 -3 2012, recast(line) lpattern(dash) lcolor(red) lwidth(thin)), ///
			ytitle("Mean Wedge") xtitle("Year") ///
			xlabel(2003(5)2020) ///
			xscale(range(2003 2020)) ///
			title("Unadjusted Wedge Over Time") ///
			legend(order(2 "Mean" 1 "95% CI" 3 "TEPA"))

		graph export "Output/Figures/Wedge/slides_mean_wedge_CI.png", as(png) replace 
		
	restore 
	
******************************************
**# Wedge Heterogeneity (Unadjsuted) *****
******************************************

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
	
	
	
	
	gen pos_wedge = (wedge > 0)

. replace pos_wedge = . if wedge == . 
(742,233 real changes made, 742,233 to missing)

. tab pos_wedge

  pos_wedge |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     70,533       22.50       22.50
          1 |    242,903       77.50      100.00
------------+-----------------------------------
      Total |    313,436      100.00

. collapse (mean) pos_wedge, by(indiv_num empnbh)

. tab pos_wedge

     (mean) |
  pos_wedge |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     47,486       24.09       24.09
   .1666667 |         10        0.01       24.10
         .2 |         34        0.02       24.11
        .25 |        100        0.05       24.16
   .3333333 |        249        0.13       24.29
         .4 |         40        0.02       24.31
         .5 |      1,173        0.60       24.90
         .6 |         53        0.03       24.93
   .6666667 |        410        0.21       25.14
        .75 |        190        0.10       25.24
         .8 |        118        0.06       25.30
   .8333333 |         29        0.01       25.31
          1 |    147,227       74.69      100.00
------------+-----------------------------------
      Total |    197,119      100.00

. gen empnbh_round = ceil(empnbh)
(573 missing values generated)

. gen n_total = sum(_n), by(empnbh_round)
option by() not allowed
r(198);

. bysort empnbh_round: egen n_total = sum(_n)

. order n_total

. drop n_total

. gen one = 1

. bysort empnbh_round: egen n_total = sum(one)

. gen new_pos = (pos_wedge == 1)

. replace new_pos = 0 if pos_wedge ==0
(0 real changes made)

. replace new_pos = . if pos_wedge ==.
(408,701 real changes made, 408,701 to missing)

. bysort empnbh: egen total_pos = sum(new_pos)

. gen pos_prop = total_pos/ n_total *100

. graph bar pos_prop empnbh

. graph bar pos_prop, over empnbh
option over not allowed
r(198);

. bar pos_prop empnbh
command bar is unrecognized
r(199);

. graph bar empnbh pos_prop

. twoway (bar pos_prop empnbh)

	


