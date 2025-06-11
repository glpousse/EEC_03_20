********************
***** Old Code *****
********************

**********************************
**# Wedge by Education & Age *****
**********************************

use Data/Clean/p_df_wedges, clear

	gen period = . 
	replace period = 1 if datdeb < tepa_date
	replace period = 2 if datdeb >= tepa_date & datdeb < abrog_date
	replace period = 3 if datdeb >= abrog_date & datdeb <macron_date

	graph bar wedge, over(period, relabel(1 "Pre-TEPA" 2 "TEPA" 3 "Post-TEPA")) over(agd) 


	gen pos = . 
	replace pos = 1 if (wedge >0 & wedge !=.)
	replace pos = 0 if (wedge <0 & wedge !=.)
	count if wedge !=. // 306,028
	
	collapse (count) wedge [pweight=extri], by(pos nivp)
	replace wedge = (wedge / 306028) *100
	reshape wide wedge, i(nivp) j(pos)
	
	graph bar wedge, over(pos) over(nivp)
	



	* Syth w 
	
// 	* Trying with one before looping
//	
// 	* Imputed Wage Growth: I apply the percentage wage growth of the imputed wage, on the actual wage
// 	* Forrwards (compounding) or backwards (discounting) depedning on whether it is the firts_wage or last_wage 
// 	bysort indiv_num: gen wage_growth = (salred_lowess - salred_lowess[_n-1])/salred_lowess[_n-1]
// 	bysort indiv_num: replace wsalred_lowess = wsalred_lowess[_n-1] * (1+wage_growth) if (du1 == 1 & wsalred_lowess ==.) 
// 	gsort indiv_num -obs_number
// 	bysort indiv_num: replace wsalred_lowess = wsalred_lowess[_n-1] / (1+wage_growth) if (du1 == 1 & wsalred_lowess ==.) 
//		



*********************************
**# WEDGE - OT RELATIONSHIP ***** 		
*********************************
	 	
/*
	ENDOGENEITY concern between OT and wedge, the below regressions cannot be interpretted causally. 
	Potential Instrumnts which have not proven valid: 
	- Quarterly GVA by sector <-- also potentially endogeneous, duh
	- Quarterly output by sector  <-- also potentially endogeneous, duh
		- Alternatigve: lag it or leave-one-out? 
	
	Next attempts:
	- export demand 
	- sector level prices (data exists?)
*/

	* Altenratively, scatter plots? 

use Data/Clean/p_df_wedges, clear 

	scatter emphre wedge if emphre < 80 & emphre > 0 || lfit emphre wedge if emphre < 80 & emphre > 0
	
	scatter emphre abs_wedge if emphre < 80 & emphre > 0 || lfit emphre abs_wedge if emphre < 80 & emphre > 0
	
*** CDI + CDD ***
 
	*** In TEPA ***  
	* Absolute
	eststo full1: reghdfe abs_wedge emphre if in_tepa [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // *** 
	* Raw
	eststo full2: reghdfe wedge emphre if in_tepa [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // *** 
	
	*** Pre-2007 *** 
	* Absolute 
	eststo full3: reghdfe abs_wedge emphre if annee <= 2007 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // ***
	* Raw
	eststo full4: reghdfe wedge emphre if annee <= 2007 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // ***
	
*** CDI ***
	* Absolute
	eststo perm1: reghdfe abs_wedge emphre if in_tepa & contra == 1 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // *** 
	* Raw
	eststo perm2: reghdfe wedge emphre if in_tepa & contra == 1 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // *** 
	
	*** Pre-2007 *** 
	* Absolute 
	eststo perm3: reghdfe abs_wedge emphre if annee <= 2007 & contra == 1 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // ***
	* Raw
	eststo perm4: reghdfe wedge emphre if annee <= 2007 & contra == 1 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // ***
	
*** CDD ***
	* Absolute
	eststo fixed1: reghdfe abs_wedge emphre if in_tepa & contra ==2 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // *** 
	* Raw
	eststo fixed2: reghdfe wedge emphre if in_tepa & contra ==2 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // *** 
	
	*** Pre-2007 *** 
	* Absolute 
	eststo fixed3: reghdfe abs_wedge emphre if annee <= 2007 & contra ==2 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // ***
	* Raw
	eststo fixed4: reghdfe wedge emphre if annee <= 2007 & contra ==2 [pweight = extri], absorb(indiv_num datdeb) cluster(indiv_num) // ***
	

*** Making the Latec Table ***
	
	esttab full1 full2 full3 full4 using "Output/Tables/OT_Wedge_Relationship/panel_full.tex", ar2 n nocons se label fragment replace varlabels(emphre "Overtime Hours" abs_wedge "Absolute Mismatch" wedge "Raw Mismatch")
	esttab fixed1 fixed2 fixed3 fixed4 using "Output/Tables/OT_Wedge_Relationship/panel_fixed.tex", ar2 n nocons se label fragment replace varlabels(emphre "Overtime Hours" abs_wedge "Absolute Mismatch" wedge "Raw Mismatch")
	esttab perm1 perm2 perm3 perm4 using "Output/Tables/OT_Wedge_Relationship/panel_perm.tex", ar2 n nocons se label fragment replace varlabels(emphre "Overtime Hours" abs_wedge "Absolute Mismatch" wedge "Raw Mismatch")


	esttab model1 model2 model3 model4 using "Output/Tables/OT_Wedge_Relationship.tex", ///
											  mtitles("Absolute Wedge" "Raw Wedge" "Absolute Wedge" "Raw Wedge") ///choose the column names 
											  varlabels(emphre "Overtime Hours" ///
											  abs_wedge "Absolute Mismatch" ///
											  wedge "Raw Mismatch") ///
											   /// choose the stats you want 
											  note("Note: Blabla. Standard errors are in parentheses") /// write a note
											  title("Relationship Between Overtime Hours and the Labor Supply Wedge") ///
											  replace 



	
	* ANNUAL GVA BY REGION (Attempmted IV - didn't work)
	gen reg2016 = ""
	replace reg2016 = "Île-de-France"                        if inlist(reg, 11)
	replace reg2016 = "Grand Est"                            if inlist(reg, 21, 22, 41, 42)
	replace reg2016 = "Normandie"                            if inlist(reg, 23, 25)
	replace reg2016 = "Centre-Val de Loire"                  if inlist(reg, 24)
	replace reg2016 = "Bourgogne-Franche-Comté"              if inlist(reg, 26, 43)
	replace reg2016 = "Hauts-de-France"                      if inlist(reg, 31)
	replace reg2016 = "Pays de la Loire"                     if inlist(reg, 52)
	replace reg2016 = "Bretagne"                             if inlist(reg, 53)
	replace reg2016 = "Nouvelle-Aquitaine"                   if inlist(reg, 54, 72, 74)
	replace reg2016 = "Occitanie"                            if inlist(reg, 73, 91)
	replace reg2016 = "Provence-Alpes-Côte d'Azur"           if inlist(reg, 93)
	replace reg2016 = "Auvergne-Rhône-Alpes"                 if inlist(reg, 82, 83)
	replace reg2016 = "Corse"                                if inlist(reg, 94)
	
	sort reg2016 naf4 annee 
	merge m:1 reg2016 naf4 annee using "Data/GVA/annual_values.dta"
	drop if _merge ==2 
	drop _merge
save "Data/Clean/df_wedges.dta", replace

	* Quarterly Value added by sector (Attempmted IV - didn't work)
	gen newnaf = ""
	replace newnaf = "Agriculture"                        if inlist(nafg36, "A0")
	replace newnaf = "Agro-food industries"               if inlist(nafg36, "B0")
	replace newnaf = "Manufactured goods"                 if inlist(nafg36, "C1", "C2", "C3", "F1", "F2", "F3", "F4", "F5")
	replace newnaf = "Capital goods"                      if inlist(nafg36, "C4", "E2", "E3", "F6")
	replace newnaf = "Transport equipment"                if inlist(nafg36, "D0", "E1")
	replace newnaf = "Energy, water, waste"               if inlist(nafg36, "G1", "G2")
	replace newnaf = "Construction"                       if inlist(nafg36, "H0")
	replace newnaf = "Distributive trade sector"          if inlist(nafg36, "J1", "J2", "J3")
	replace newnaf = "Transport"                          if inlist(nafg36, "K0")
	replace newnaf = "Financial services"                 if inlist(nafg36, "L0")
	replace newnaf = "Real estate services"               if inlist(nafg36, "M0")
	replace newnaf = "Information and communication"      if inlist(nafg36, "N1")
	replace newnaf = "Business Services"                  if inlist(nafg36, "N2", "N3", "N4")
	replace newnaf = "Accommodation and food service activities"    if inlist(nafg36, "P1")
	replace newnaf = "Household services"                 if inlist(nafg36, "P2", "P3")
	replace newnaf = "Mainly non-market services"         if inlist(nafg36, "Q1", "Q2")
	replace newnaf = "Non-market services"                if inlist(nafg36, "R1", "R2")
	
	sort newnaf datdeb_q
	merge m:1 newnaf datdeb_q using "Data/Quarterly_Branch_Account/GVA_q.dta"
	drop if _merge ==2
	drop _merge
	merge m:1 newnaf datdeb_q using "Data/Quarterly_Branch_Account/production_q.dta"
	drop if _merge ==2
	drop _merge
	
	/* 
		nafg36 data only goes to 2009. That's fine I only need until 2008. I provide the nafg36 codes to
		be able to understand the reclassification. In the end 2822 observations were not matched 
		these were observations with no reported nafg36 code.	
	*/
save "Data/Clean/df_wedges.dta", replace


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

use Data/Clean/p_df_wedges, clear
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


	* PEPA Bonus Eligibility
	
	/*replace valprie = 0 if valprie == . 
	replace valprie_2015 = 0 if valprie_2015 == .
	gen just_wage_2015 = salmee_2015 - valprie_2015 if prim ==1 
	
	gen elig_pepa = (just_wage_2015 < (smic_m * 3) & datdeb > date("01jan2019", "DMY"))
	order elig_pepa*/
	
	
	
	
	***** Non-Monotonicity of Hourly wages *****

* MAKE SURE THIS FEATURE IS ROBUST (BREAK DOWN INTO SUB-GROUPS) * 

	* Figure 1 from Bick et al. 
	
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
	
*************
**# ID2 *****
*************

use Data/Clean/quick_test.dta, clear
	
	keep if in_tepa 
	//keep if contra == 1

*** Workers in small firms (<2 workers) vs. Independents ***

	gen control 		= 1 if statutr == 1 & efen == 0
	gen treatment 		= 1 if statutr != 1 & efen == 1 
	gen post_treatment 	= post_tepa * treatment 
	
	keep if control == 1 | treatment == 1
	
	gen triple_optim 		= treatment * optim * post_tepa	// 0 = laborer, 1 = manager 
	gen triple_gender		= treatment * sexe * post_tepa	// 0 = femme, 1 = homme
	gen triple_underworked	= treatment * underworked * post_tepa // 0 = underworked, 1 = not undeworked 
	
	global i_controls sexe married enfant salred salred_sq educ_degree age age_sq annees_etudes ancentr cat_naf10
	
*** Regressions ***

	reghdfe wedge post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(indiv_num datdeb) cluster(indiv_num) // 
	
	reghdfe wedge triple_optim optim post_treatment treatment post_tepa  $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) cluster(indiv_num) //
	
	* Y = |Wedge|
	
	reghdfe abs_wedge post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) cluster(indiv_num) // 

	reghdfe abs_wedge triple_optim optim post_treatment treatment $i_controls $c_controls [pweight=extri], absorb(datdeb indiv_num) cluster(indiv_num) // 
	



