*******************************
*******************************
***** SYNTHETIC VARIABLES ***** 
*******************************
*******************************

/***** QUESTIONS ***** 

-	CREATING COHORTS. From my unerstanding, I want at least 10 individuals per cohort, in order 
					  to minimize outlier sensitivity. For this, I generate cohorts based on age 
					  and years of study. 

- 	SMOOTHING METHOD: What is the most appropriate method to smooth the cohort wage averages? I have 
					  compared results using CMA, LMA, and lowess? I also know of spline smoothing 
					  and kalman smoothing. Should I try them too?
		- MAYBE, would it be a good idea to take the average of the three synthetic wages? 

- 	LOWESS BANDWIDT: I took 0.4 as a the lowess bandwidth standard number. 
					 How do I know if I want to capture more or less variance?
	
-	SPARSE DATA: If data is sparsely reported (more the case for desired hours). It becomes harder to
				 smooth out the average. This creates many missing values. 
				 
				 What are the tradeoffs between using the smoothed average and the raw cohort average 
				 for that time period? 
*********************/

***************
**# WAGES ***** 
***************
/*
	The wage variable is mainly reported in the first and last interrogation of the LFS. 
	To simply categorize observations by wage. I used the average wage. 
	For more advanced analysis, I need variation in the wage. I propose to generate synthetic
	values of the wage, by leveraging the rolling panel structure of the LFS. 
	
	New individuals join the panel every month, while others exit. If I successfully define cohorts 
	grouping "new-joiners" by their common characteristics, then I can compute the average wage 
	for individuals	of that cohort in a given month. Then I can impute missing wage values by 
	applying each cohort's variation in "synthetic" wages to the individual's "true" values of wages.
	
	E.g. 
	Persons A, B and C have similar characteristics (age, job industry, education): they fall into
	the same cohort. They all report wages in their first interrogation. A enters in Jan 2007, B enters in
	Feb 2007, and C enters in March 2007. Assuming the cohort requirements are specific enough, I can 
	collapse these wages into the wage of any individual of that cohort. Now, I study the evolution of that 
	wage and its variation. Then, I apply this trend to the pre-exisitng values of wage for individuals 
	belonging to the same cohort.
	To add precision, consider A, B and C to be waves (or vectors) or "new-joiners". 
	This is our case in the LFS!
*/

use Data/Clean/df_wedges, clear 
	
	cap ssc intall bspline 

	* My sample
	keep if CDI 					// permanent contract
	keep if full_time 				// full_time
	keep if du_hhc					// 35 <= average weekly hours <= 70
	keep if no_interruption			// uninterrupted workschedule
	keep if mod_agree ==0			// no modulation agreement 
	keep if min_smic				// Min wage 	
	
	* STEP 1: Cohort Monthly Averages
	
	* GENERATING COHORTS (GOAL: Maximize # of categories while minimizaing # of small categories
	egen cohort_id_salred = group(age_cohort sexe nivet), missing
	
	bysort cohort_id_salred: gen cohort_id_count = _N
	
	preserve 
		collapse (mean) cohort_id_count [pw=extri], by(cohort_id_salred) 
		count										// 142 cohorts total. 
		count if cohort_id_count <10				// 4 cohorts with less than 10 individuals. 
		levelsof cohort_id_salred if cohort_id_count < 10, local(lowcount_cohorts)
		tab cohort_id_salred if cohort_id_count < 10 
	restore
		
	* Counting the Number of idnividuals in cohorts of less than 10
	cap drop wflag_10
	gen wflag_10 = . 
	local total = 0
	foreach i in `lowcount_cohorts'{
		replace wflag_10 = 1 if cohort_id_salred == `i'
	quietly count if cohort_id_salred == `i'
    local total = `total' + r(N)
	}
	display "Total count: `total'" 
	count if in_tepa & wflag_10 ==1 				// 0 People in TEPA and in cohort under 10. 
save "Data/Clean/wsynth_df_wedges.dta", replace 
	
	* Collapsing Monthly Cohort Wage Averages -- I NEED QUARTERLY IF I AM TO USE TSSMOOTH. WHICH ONE?
use Data/Clean/wsynth_df_wedges, clear
	* Keeping only obs with salred 
	keep if salred !=.
	collapse (mean) salred [pw=extri], by(cohort_id_salred datdeb_m)
	rename salred cohort_salred
save "Data/Clean/wsynth_collapsed.dta", replace 
	
	* STEP 2: Smoothing Collapsed Values & Merging 
use Data/Clean/wsynth_collapsed, clear 
	sort cohort_id_salred datdeb_m
	
	* CENTERED MOVING AVERAGE
	tsset cohort_id_salred datdeb_m
	gen salred_CMA = (L1.cohort_salred + cohort_salred + F1.cohort_salred) / 3
	
	* LAGGED MOVING AVERAGE 
	tssmooth ma salred_LMA = cohort_salred, window(3)
save "Data/Clean/MA_wages.dta", replace

	* LOWESS -- Locally weighted scatterplot smoothing 
use Data/Clean/wsynth_collapsed, clear 
	
	* Bandwidth 0.4
	levelsof cohort_id_salred, local(cohorts)
	foreach c of local cohorts {
		display "Smoothing cohort `c'"
		lowess cohort_salred datdeb_m if cohort_id_salred == `c', gen(smoothed_`c') bwidth(0.4)
	}
	
	levelsof cohort_id_salred, local(cohorts)
	gen salred_lowess =. 
	foreach c of local cohorts {
		replace salred_lowess = smoothed_`c' if cohort_id_salred == `c'
	}
	drop smoothed_*
save "Data/Clean/LOWESS_wages.dta", replace

	* B-SPLINE 
// Load your cleaned data
use Data/Clean/wsynth_collapsed, clear

levelsof cohort_id_salred, local(cohorts)
gen income_smooth = .


foreach c of local cohorts {
    preserve
        keep if cohort_id == `c'  // Keep only the current cohort
        sort datdeb_m

        * Determine knots
        quietly count
        local n = r(N)
		if `n' < 2 {
			restore 
			continue 
		}
    
        * Create a tag for unique values of datdeb_m
        egen unique_tag = tag(datdeb_m)
        quietly count if unique_tag == 1
        local unique_vals = r(N)  // Count unique values
        
        local k = max(3, floor(sqrt(`n')*1.5)) // I boost the square root (OG fit was too smooth) 
        local k = min(`k', `unique_vals')  // Ensure k does not exceed unique values

        if `k' == 2 { 
            * If fewer than 2 knots, fit simple linear regression
            regress cohort_salred datdeb_m
            predict smooth_temp, xb
        }
        else {
            * Get knot positions using _pctile
            local knotvals ""
            forvalues i = 1/`k' {
                local pct = `i'/(`k'+1)*100
                _pctile datdeb_m, p(`pct')  // Calculate percentile
                local knotvals "`knotvals' `r(r1)'"  // Store knot values
            }

            * Build spline and fit model
            mkspline sp_time = datdeb_m, cubic knots(`knotvals')  
            regress cohort_salred sp_time*
            predict smooth_temp, xb
        }
        
        * Store smoothed income
        gen smoothed_income = smooth_temp
        keep datdeb_m cohort_id smoothed_income  
        save "Data/bspline/salred/smoothed_`c'", replace
    restore
}

	* Append smoothed results
	local files
	forvalues i = 1/142 {
		if inlist(`i', 19, 29, 58) continue  // Skip missing files
		local files `files' "Data/bspline/salred/smoothed_`i'.dta"
}

use Data/bspline/smoothed_1.dta, clear
	forvalues i = 2/142 {
		if inlist(`i', 19, 29, 58) continue
		append using "Data/bspline/salred/smoothed_`i'"
}
	rename smoothed_income salred_bspline

save "Data/Clean/BSPLINE_wages.dta", replace
	
	* Merging back onto dataset
use Data/Clean/wsynth_df_wedges, clear 
	order datdeb_m cohort_id_salred 
	sort cohort_id_salred datdeb_m

	merge m:m cohort_id_salred datdeb_m using "Data/Clean/LOWESS_wages.dta"
	drop _merge
	merge m:m cohort_id_salred datdeb_m using "Data/Clean/MA_wages.dta"
	drop _merge 
	merge m:m cohort_id_salred datdeb_m using "Data/Clean/BSPLINE_wages.dta"
	drop _merge
	order indiv_num cohort_id_salred rgi salred salred_CMA salred_LMA salred_lowess salred_bspline 
save "Data/Clean/wsynth_df_wedges.dta", replace 

	count if salred_CMA 	== . 	//  15,850
	count if salred_LMA 	==.		//  5,486
	count if salred_lowess 	== .	//  3,855
	count if salred_bspline ==. 	//  3,835
	
	* MA
	preserve 
		collapse (mean) salred_CMA salred_LMA  salred, by (datdeb_q)
		twoway(line salred_CMA datdeb_q)(line salred_LMA datdeb_q)(line salred datdeb_q)
	restore 
	
	* LOWESS BSPLINE
	preserve 
		collapse (mean) salred_lowess salred_bspline  salred, by (datdeb_q)
		twoway(line salred_bspline datdeb_q)(line salred_lowess datdeb_q)(line salred datdeb_q)
	restore 

	* STEP 3 : Applying Estimated Wage Variation to True wage values

use Data/Clean/wsynth_df_wedges, clear 

	* Individuals provide a maximum of two wage observations
	gen du = 1 if salred !=.
	sort indiv_num datdeb
	bysort indiv_num: egen du1 = total(du)
	tab du1
	
	* 37.86% provide both these observations. 

//         du1 |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |     43,421       12.54       12.54
//           1 |    171,836       49.61       62.14
//           2 |    131,141       37.86      100.00
// ------------+-----------------------------------
//       Total |    346,398      100.00

	* CASE 1: For individuals with no observations, I propose to assign imputed wage values. 
	
	foreach var in salred_CMA salred_LMA salred_lowess salred_bspline {
		gen w`var' 		= salred 
		replace w`var' 	= `var' if du1 ==0 & salred ==. 
	}
	
	* CASE 2: For individuals with one true wage observation, I propose to apply their cohort's wage growth rate. 
	
	* To know whether the wage was observed in the individuals first or last query
	* First a dummy to know if its the first or second 
	sort indiv_num datdeb 
	bysort indiv_num: gen tot_obs 		= _N
	bysort indiv_num: gen obs_number 	= _n
	bysort indiv_num (datdeb): gen rank_duw = sum(du) // rank_duw = 1 --> 1st wage || rank_duw = 2 --> 2nd wage 
	
	* Computing Synthetic Growth Rates & Merging
	preserve 
		collapse (mean) salred_CMA salred_LMA salred_lowess salred_bspline [pw=extri], by (datdeb_m cohort_id_salred)
		sort cohort_id_salred datdeb_m
		foreach var in salred_CMA salred_LMA salred_lowess salred_bspline{ 
			bysort cohort_id_salred: gen wage_growth_`var' = (`var' - `var'[_n-1]) / `var'[_n-1]
		}
		drop salred_CMA salred_LMA salred_lowess salred_bspline
		tempfile synth_wgrowth_rates
		save `synth_wgrowth_rates'
	restore 
	
	merge m:1 cohort_id_salred datdeb_m using `synth_wgrowth_rates'
	drop _merge
	
	* Applying the Synthetic Growth Rate to True Wages 
	foreach var in salred_CMA salred_LMA salred_lowess salred_bspline {
		* Forwards 
		bysort indiv_num: replace w`var' = w`var'[_n-1] * (1+wage_growth_`var') if (du1 == 1 & w`var' ==.)
		
		* Backwards (no rate for 1st obs, will be added after case 3)
		gsort indiv_num -datdeb
		bysort indiv_num: replace w`var' = w`var'[_n-1] / (1+wage_growth_`var') if (du1 == 1 & w`var' ==.)
		sort indiv_num datdeb_m
	}

	* CASE 3: For individuals with two observations, these observations are not necessarilly the first 
	* 		  and the last given by the individual. There may be more missing wage observations before
	*         the first true value or after the last true value. Hence, I propose a mixed imputation strategy. 
	* 		  For missing values between the known wage values --> proportional normalization 
	*		  For missing values before (after) the first (last) value --> same as in Case 2.
	
	* Spreading values I will need later on:
	
	* Spreading dates for the first and last wage observations 
	sort indiv_num datdeb
	gen first_temp = datdeb if rank_duw == 1 & du1 ==2 & salred !=. 
	gen last_temp = datdeb if rank_duw == 2 & du1 ==2 & salred !=. 
	bysort indiv_num: egen first_time = max(first_temp)
	bysort indiv_num: egen last_time  = max(last_temp)
	drop first_temp last_temp
	format first_time last_time %td 
	
	* Spreading values for the first and last wage observations 
	gen first_temp = salred if rank_duw == 1 & du1 ==2 & salred !=. 
	gen last_temp = salred if rank_duw == 2 & du1 ==2 & salred !=. 
	bysort indiv_num: egen first_salred = max(first_temp)
	bysort indiv_num: egen last_salred  = max(last_temp)
	drop first_temp last_temp
	
	* Spreading values for the first and last synthtetic wage observations 
	foreach var in CMA LMA lowess bspline {
		gen first_temp = salred_`var' if rank_duw == 1 & du1 ==2 & salred !=. 
		gen last_temp = salred_`var' if rank_duw == 2 & du1 ==2 & salred !=. 
		bysort indiv_num: egen first_synth_w`var' = max(first_temp)
		bysort indiv_num: egen last_synth_w`var'  = max(last_temp)
		drop first_temp last_temp 
	}
	
	* Identifying rows between true wage values 
	gen inside 	= datdeb > first_time & datdeb < last_time
	
	* Relative time position of value inside (0-1 scale)
	gen rel_pos = (datdeb - first_time) / (last_time - first_time) if inside 
	
	* The normalization formula: generating the adjustment coefficient
	foreach var in CMA LMA lowess bspline {
		gen log_adj_`var' = log(last_salred / last_synth_w`var') * rel_pos + log(first_salred / first_synth_w`var') * (1 - rel_pos)
	}
	
	* Applying the adjustement coeff 
	foreach var in CMA LMA lowess bspline {
		replace wsalred_`var' = salred_`var' * exp(log_adj_`var') if inside
	}
	order datdeb indiv_num rgi wage_growth* salred_* wsalred*
	
**********************************
**# FIGURES: Synthetic Wages *****
**********************************

	foreach var in salmee salred wsalred_CMA wsalred_LMA wsalred_lowess wsalred_bspline salsee smic_h_brut smic_m_net  {
		cap noisily replace `var' = . if (`var' == 9999998 | `var' == 9999999 | `var' == 999999)
		cap drop `var'_2015
		gen `var'_2015 = `var' * (100/cpi)
	}

	* Monthly 
	preserve 	
		collapse (mean) wsalred_lowess_2015 wsalred_CMA_2015 wsalred_LMA_2015 salred_2015 wsalred_lowess wsalred_CMA wsalred_LMA wsalred_bspline wsalred_bspline_2015 salred, by(datdeb_m)
		
		twoway(line wsalred_CMA_2015 datdeb_m)(line wsalred_LMA_2015 datdeb_m)(line salred_2015 datdeb_m, lcolor(gold)),  ///
		xtitle("") ytitle("Wage (€, 2015 base)") /// 
		legend(position(4) ring(0) label(1 "CMA Smoothing") label(2 "LMA Smoothing") label(3 "True Wages")) 
		graph export "Output/Figures/Synthetic/2015w_CMA_LMA_monthly.png", as(png) replace 
		
		twoway(line wsalred_lowess_2015 datdeb_m, lcolor(orange))(line wsalred_bspline_2015 datdeb_m, lcolor(green))(line salred_2015 datdeb_m, lcolor(gold)), ///
		xtitle("") ytitle("Wage (€, 2015 base)") ///
		legend(position(4) ring(0) label(1 "Lowess Smoothing") label(2 "B-Spline Smoothing") label(3 "True Wages") ) name(M2015, replace)
		graph export "Output/Figures/Synthetic/2015w_bspline_lowess_monthly.png", as(png) replace 
	restore 
	
	* Quarterly
	preserve 	
		collapse (mean) wsalred_lowess_2015 wsalred_CMA_2015 wsalred_LMA_2015 salred_2015 wsalred_lowess wsalred_CMA wsalred_LMA wsalred_bspline wsalred_bspline_2015 salred, by(datdeb_q)
		
		twoway(line wsalred_CMA_2015 datdeb_q)(line wsalred_LMA_2015 datdeb_q)(line salred_2015 datdeb_q, lcolor(gold)),  ///
		xtitle("") ytitle("Wage (€, 2015 base)") /// 
		legend(position(4) ring(0) label(1 "CMA Smoothing") label(2 "LMA Smoothing") label(3 "True Wages")) 
		graph export "Output/Figures/Synthetic/2015w_CMA_LMA_quarterly.png", as(png) replace 
		
		twoway(line wsalred_lowess_2015 datdeb_q, lcolor(orange))(line wsalred_bspline_2015 datdeb_q, lcolor(green))(line salred_2015 datdeb_q, lcolor(gold)), ///
		xtitle("") ytitle("Wage (€, 2015 base)") ///
		legend(position(4) ring(0) label(1 "Lowess Smoothing") label(2 "B-Spline Smoothing") label(3 "True Wages") ) name(M2015, replace)
		graph export "Output/Figures/Synthetic/2015w_bspline_lowess_quarterly.png", as(png) replace 
	restore 
	
	preserve 	
		collapse (mean) salred salred1, by(datdeb_q)
		
		twoway(line salred datdeb_q)(line salred1 datdeb_q),  ///
		xtitle("") ytitle("Wage (€, 2015 base)") /// 
		legend(position(4) ring(0) label(2 "Avg. Wages") label(1 "True Wages")) 
		//graph export "Output/Figures/Synthetic/2015w_CMA_LMA_quarterly.png", as(png) replace 
	restore 
	
save "Data/Clean/SYNTH_1.dta", replace

do "Code/preliminary_data_prep/XGB.do" 

 
***********************
**# DESIRED HOURS ***** 
***********************
/*
	The desired hours question is asked at every interrogation of the LFS. It is not consistently 
	answered. I propose to apply the same method as wages, where I create cohorts based on 
	different socio-economic categories. Thanks to the richness of the LFS, I have a wide range of 
	characteristics to choose from, making this a credible strategy. 
	
	There are two goals.
	GOAL 1: Maximize the cohorts I create. 
	GOAL 2: Minimizing the number of cohorts with less than 10 individuals. 
	
	I consider a few different alternatives. I justify the final catefories with the heterogeneity 
	analysis in `4_descriptive_analysis'.
		- Current working hours 
		- Gender 
		- Age 
		- ± 5000 salred
		- marriage status
	Desired hours are reported more sparsely than the wages. To account for this I estiamte them quarterly.  
*/
	
use Data/Clean/SYNTH_1, clear 

	* My sample
	keep if CDI 					// permanent contract
	keep if full_time 				// full_time
	keep if du_hhc					// 35 <= average weekly hours <= 70
	keep if no_interruption			// uninterrupted workschedule
	keep if mod_agree ==0			// no modulation agreement 
	keep if min_smic				// min wage (NET)
	
	* H analysis shows that + - 5000 salred influences the desired hours
	gen wage_5k = (salred > 5000 | wsalred_CMA > 5000 | wsalred_LMA > 5000 | wsalred_lowess > 5000) 
// ATTENTION, HERE I HAVE NOT ADDED IMPUTED WAGES. 
// 1. CHOOSE THE BEST IMPUTED WAGE. 
// 2. RERUN THIS ANALYSIS. 

	* STEP 1: Cohort Monthly Averages
	
	* Generating cohorts 
	egen cohort_id_hplus = group(sexe hhc_cat age_group educ_degree married wage_5k), missing
	cap drop cohort_id_count
	bysort cohort_id_hplus: gen cohort_id_count = _N
	
	* Counting the Number of Cohorts, also those with less than 10 individuals. 
	preserve 
		collapse (mean) cohort_id_count [pw=extri], by(cohort_id_hplus) 
		count										// 790 cohorts 
		count if cohort_id_count <10				// 197 cohorts with less than 10 individuals
		levelsof cohort_id_hplus if cohort_id_count < 10, local(lowcount_cohorts)
		tab cohort_id_hplus if cohort_id_count < 10 
	restore
	
	* Counting the Number of idnividuals in cohorts of less than 10
	gen hflag_10 = . 
	local total = 0
	foreach i in `lowcount_cohorts'{
		replace hflag_10 = 1 if cohort_id_hplus == `i'
	quietly count if cohort_id_hplus == `i'
    local total = `total' + r(N)
	}
	display "Total count: `total'" // 839 individuals in a cohort of under 10 ppl, of which 116 in the TEPA sample.
	count if in_tepa & hflag_10 ==1 
save "Data/Clean/hsynth_df_wedges.dta", replace

	* Collapsing 
use Data/Clean/hsynth_df_wedges, clear		
	keep if hplus !=.
	collapse (mean) hplus [pw=extri], by(cohort_id_hplus datdeb_q hflag_10)
	rename hplus cohort_hplus 
save "Data/Clean/hsynth_collapsed.dta", replace
	
	* STEP 2: Smoothing Collapsed Values & Merging 
use Data/Clean/hsynth_collapsed, clear 

	* CENTERED MOVING AVERAGE
	sort cohort_id_hplus datdeb_q
	tsset cohort_id_hplus datdeb_q
	gen hplus_CMA = (L1.cohort_hplus + cohort_hplus + F1.cohort_hplus) / 3
	
	* LAGGED MOVING AVERAGE 
	tssmooth ma hplus_LMA = cohort_hplus, window(3)
save "Data/Clean/MA_hplus.dta", replace 

	* LOWESS -- Locally weighted scatterplot smoothing 
use Data/Clean/hsynth_collapsed, clear 

	* Bandwidth 0.4
	levelsof cohort_id_hplus, local(cohorts)
	foreach c of local cohorts {
		display "Smoothing cohort `c'"
		lowess cohort_hplus datdeb_q if cohort_id_hplus == `c', gen(smoothed_`c') bwidth(0.4)
	}
	
	levelsof cohort_id_hplus, local(cohorts)
	gen hplus_lowess =. 
	foreach c of local cohorts {
		replace hplus_lowess = smoothed_`c' if cohort_id_hplus == `c'
	}
	drop smoothed_*
save "Data/Clean/LOWESS_hplus.dta", replace

	* B-Spline
	// Load your cleaned data
use Data/Clean/hsynth_collapsed, clear

levelsof cohort_id_hplus, local(cohorts)
gen hplus_bspline = .

foreach c of local cohorts {
    preserve
        keep if cohort_id == `c'  // Keep only the current cohort
        sort datdeb_q

        * Determine knots
        quietly count
        local n = r(N)
		if `n' < 2 {
			restore 
			continue 
		}
    
        * Create a tag for unique values of datdeb_q
        egen unique_tag = tag(datdeb_q)
        quietly count if unique_tag == 1
        local unique_vals = r(N)  // Count unique values
        
        local k = max(3, floor(sqrt(`n')*1.5)) // I boost the square root (OG fit was too smooth) 
        local k = min(`k', `unique_vals')  // Ensure k does not exceed unique values

        if `k' == 2 { 
            * If fewer than 2 knots, fit simple linear regression
            regress cohort_hplus datdeb_q
            predict smooth_temp, xb
        }
        else {
            * Get knot positions using _pctile
            local knotvals ""
            forvalues i = 1/`k' {
                local pct = `i'/(`k'+1)*100
                _pctile datdeb_q, p(`pct')  // Calculate percentile
                local knotvals "`knotvals' `r(r1)'"  // Store knot values
            }

            * Build spline and fit model
            mkspline sp_time = datdeb_q, cubic knots(`knotvals')  
            regress cohort_hplus sp_time*
            predict smooth_temp, xb
        }
        
        * Store smoothed income
        gen smoothed_hplus = smooth_temp
        keep datdeb_q cohort_id smoothed_hplus  
        save "Data/bspline/hplus/smoothed_`c'", replace
    restore
}

	* I use the following command in my computer terminal to identify missing files 
	* Missing files = cohorts which were skipped because of too few knots
// >	for i in {1..486}; do
// >     file="smoothed_${i}.dta"
// >     if [ ! -f "$file" ]; then
// >         echo "Missing: $file"
// >     fi
// > 	done

	* Append smoothed results
	local files
	forvalues i = 1/486 {
		if inlist(`i', 48, 71, 75, 76, 79, 83, 84, 101, 103, 104, 105, 106, 107, 111, 115, 116, 119, 120, 133, 134, 137, 138, 139, 141, 143, 144, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 158, 162, 168, 169, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 199, 200, 201, 202, 203, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 216, 218, 223, 224, 225, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 285, 322, 339, 340, 343, 356, 357, 358, 375, 379, 385, 386, 389, 390, 391, 392, 393, 394, 407, 416, 418, 420, 422, 423, 424, 425, 426, 428, 429, 448, 450, 454, 456, 457, 458, 459, 460, 461, 463, 464, 473, 477, 481, 482, 484, 485) continue
  // Skip missing files
		local files `files' "Data/bspline/hplus/smoothed_`i'.dta"
}

use Data/bspline/hplus/smoothed_1.dta, clear
	forvalues i = 2/486 {
		if inlist(`i', 48, 71, 75, 76, 79, 83, 84, 101, 103, 104, 105, 106, 107, 111, 115, 116, 119, 120, 133, 134, 137, 138, 139, 141, 143, 144, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 158, 162, 168, 169, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 199, 200, 201, 202, 203, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 216, 218, 223, 224, 225, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 285, 322, 339, 340, 343, 356, 357, 358, 375, 379, 385, 386, 389, 390, 391, 392, 393, 394, 407, 416, 418, 420, 422, 423, 424, 425, 426, 428, 429, 448, 450, 454, 456, 457, 458, 459, 460, 461, 463, 464, 473, 477, 481, 482, 484, 485) continue
		append using "Data/bspline/hplus/smoothed_`i'"
}
	rename smoothed_hplus hplus_bspline

save "Data/Clean/BSPLINE_hplus.dta", replace
	
	
	* Merging back onto dataset
use Data/Clean/hsynth_df_wedges, clear 

	order datdeb_m cohort_id_hplus 
	sort cohort_id_hplus datdeb_m

	merge m:m cohort_id_hplus datdeb_q using "Data/Clean/LOWESS_hplus.dta"				
	drop _merge 						
	merge m:m cohort_id_hplus datdeb_q using "Data/Clean/MA_hplus.dta"
	drop _merge
	merge m:m cohort_id_hplus datdeb_q using "Data/Clean/BSPLINE_hplus.dta"
	drop _merge
	order indiv_num cohort_id_hplus rgi hplus hplus_CMA hplus_LMA hplus_lowess hplus_bspline
save "Data/Clean/hsynth_df_wedges.dta", replace // this also has the imputed wages 

	count if hplus_CMA 		== . 	// hplus: 75,304 	salred: 15,850 			59k
	count if hplus_LMA 		== .	// hplus: 45,571	salred: 5,486			33k
	count if hplus_lowess 	== .	// hplus: 33,314	salred: 3,855			22k
	count if hplus_bspline 	== .	// hplus: 21,087	salred: 3,835			21k
	count if cohort_hplus 	== .	// hplus: 31,550	salred: 1,117			21k

	* STEP 3 : Applying Estimated Desired Hours Variation to True Desired Hours
use Data/Clean/hsynth_df_wedges, clear 

	gen hdu = (hplus !=.)
	bysort indiv_num: egen hdu1 = total(hdu)
	tab hdu1	
// 	hdu1 |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |    242,553       70.56       70.56
//           1 |     51,194       14.89       85.46
//           2 |     21,379        6.22       91.68
//           3 |     13,114        3.82       95.49
//           4 |      8,742        2.54       98.04
//           5 |      4,829        1.40       99.44
//           6 |      1,920        0.56      100.00
// ------------+-----------------------------------
//       Total |    343,731      100.00
	
	* This can still be categorized into the three cases from the wages exercise. 
	
	* CASE 1: Individuals with no reported desired hours take their cohort's desired hours. 
	foreach var in CMA LMA lowess bspline {
		gen hhplus_`var ' 		= hplus
		replace hhplus_`var '	= hplus_`var' if hdu1 == 0 & hplus == . 
	}
	
	* BEFORE PROCEEDING, I assume individuals with no variation in the reported desired hours had no variation in the non-reported ones. 
	* In which case, his desired hours are constant. 
	bysort indiv_num: egen mean_hplus = mean(hplus)
	bysort indiv_num: egen sd_hplus 	= sd(hplus)
	foreach var in CMA LMA lowess bspline {
		bysort indiv_num: replace hhplus_`var' = mean_hplus if  hhplus_`var' == . & sd_hplus == 0 & hdu1 > 1 
	}
	
	* Case 2: Individuals with only one reported hplus value, see that hplus value grow at the
	*		  same rate as their cohort. 
	
	* Computing the synthetic desired hours growth rates 	
	preserve 
		collapse (mean) hplus_CMA hplus_LMA hplus_lowess hplus_bspline, by (datdeb_q cohort_id_hplus)
		sort cohort_id_hplus datdeb_q
		foreach var in hplus_CMA hplus_LMA hplus_lowess hplus_bspline{ 
			bysort cohort_id_hplus: gen growth_`var' = (`var' - `var'[_n-1]) / `var'[_n-1]
		}
		drop hplus_CMA hplus_LMA hplus_lowess hplus_bspline
		tempfile synth_hgrowth_rates
		save `synth_hgrowth_rates'
	restore  
	
	cap drop _merge
	merge m:1 cohort_id_hplus datdeb_q using `synth_hgrowth_rates'
	drop _merge
	
	* Applying the Synthetic Growth Rate to Desired Hours
	order indiv_num obs_number hplus hhplus_* growth_hplus_* 
	br if hdu1 == 1
	sort indiv_num datdeb
	
	foreach var in hplus_CMA hplus_LMA hplus_lowess hplus_bspline {
		* Forwards 
		bysort indiv_num: replace h`var' = h`var'[_n-1] * (1+growth_`var') if (hdu1 == 1 & h`var' ==.)	
	* Backwards: Dynamic
	gsort indiv_num -datdeb
		bysort indiv_num: replace h`var' = h`var'[_n-1] / (1+growth_`var') if (hdu1 == 1 & h`var' ==.)
		sort indiv_num datdeb 
	* Backwards non-dynamic (for values at extreme ends of the survey rounds)
		bysort indiv_num: gen case2`var' = (h`var'==. & h`var'[_n+1] !=. & growth_`var'[_n+1] !=.) if (hdu1 == 1 & h`var' ==.)
		bysort indiv_num: replace h`var' = h`var'[_n+1] / (1+ growth_`var'[_n+1]) if case2`var' ==1 
		}
	
	* Case 3: For individuals with more than 1 hplus observation I proceed as for wages. 
	* 		  For any missing observations BETWEEN 2 true values: proportional normalization.
	*		  For any missing values on the "outside" I apply method from Case 2. 
	
	* Dummy to know order of true values
	sort indiv_num datdeb 
	bysort indiv_num (datdeb): gen rank_duh = sum(hplus!=.) 
	replace rank_duh = 0 if hplus == .	// rank_duh = 1 --> 1st wage, rank_duh = 2 --> 2nd wage, etc. 
	
	forvalues i = 1/5{
	
	* Spreading dates between the first 2 true values  
	sort indiv_num datdeb
	gen first_temp = datdeb if rank_duh == `i' & hdu1 >= 2 
	gen last_temp = datdeb if rank_duh 	== `i'+1 & hdu1 >= 2 
	cap drop first_time last_time
	bysort indiv_num: egen first_time = max(first_temp)
	bysort indiv_num: egen last_time  = max(last_temp)
	drop first_temp last_temp
	format first_time last_time %td 
	
	* Spreading values for the first and last wage observations 
	gen first_temp 	= hplus if rank_duh == `i' & hdu1 >=2 
	gen last_temp 	= hplus if rank_duh == `i'+1 & hdu1 >=2 
	cap drop first_hplus last_hplus 
	bysort indiv_num: egen first_hplus = max(first_temp)
	bysort indiv_num: egen last_hplus  = max(last_temp)
	drop first_temp last_temp
	
	* Spreading values for the first and last synthtetic observations 
	foreach var in CMA LMA lowess bspline {
		gen first_temp 	= hplus_`var' if rank_duh == `i' & hdu1 >=2 
		gen last_temp 	= hplus_`var' if rank_duh == `i'+1 & hdu1 >=2 
		cap drop first_synth_h`var' last_synth_h`var'
		bysort indiv_num: egen first_synth_h`var' 	= max(first_temp)
		bysort indiv_num: egen last_synth_h`var'  	= max(last_temp)
		drop first_temp last_temp 
	}
	
	* Identifying rows between true wage values 
	cap drop inside
	gen inside 	= datdeb > first_time & datdeb < last_time 
	
	* Relative time position of value inside (0-1 scale)
	cap drop rel_pos 
	gen rel_pos = (datdeb - first_time) / (last_time - first_time) if inside 
	
	* The normalization formula: generating the adjustment coefficient
	foreach var in CMA LMA lowess bspline {
		cap drop log_adj_`var'
		gen log_adj_`var' = log(last_hplus / last_synth_h`var') * rel_pos + log(first_hplus / first_synth_h`var') * (1 - rel_pos)
	}
	
	* Applying the adjustement coeff 
	foreach var in CMA LMA lowess bspline {
		replace hhplus_`var' = hplus_`var' * exp(log_adj_`var') if inside == 1
	}
	}
	
	* Lastly, applying growth rates to missing values outside the interval defined by "true" valules of desired hours.
	foreach var in hplus_CMA hplus_LMA hplus_lowess hplus_bspline {
		* Forwards 
		bysort indiv_num: replace h`var' = h`var'[_n-1] * (1+growth_`var') if (hdu1 >1 & h`var' ==.)	
		* Backwards: 
		gsort indiv_num -datdeb
		bysort indiv_num: replace h`var' = h`var'[_n-1] / (1+growth_`var') if (hdu1 > 1 & h`var' ==.)
		sort indiv_num datdeb 
		}
		
******************************************
**# FIGURES: Synthetic Desired Hours *****
******************************************

	* MONTHLY COMPARISON 
	preserve 	
		collapse (mean) hhplus_lowess hhplus_CMA hhplus_LMA hhplus_bspline hplus, by(datdeb_m)
		
		twoway(line hhplus_CMA datdeb_m)(line hhplus_LMA datdeb_m)(line hplus datdeb_m, lcolor(gold)), xtitle("") ytitle("Desired Hours") legend(position(4) ring(0)label(1 "CMA Smoothing") label(2 "LMA Smoothing") label(3 "True Desired Hours"))
		graph export "Output/Figures/Synthetic/hplus_CMA_LMA_monthly.png", as(png) replace 
		
		
		twoway(line hhplus_lowess datdeb_m, lcolor(orange))(line hhplus_bspline datdeb_m, lcolor(green))(line hplus datdeb_m, lcolor(gold)), xtitle("") ytitle("Desired Hours") legend(position(4) ring(0) label(1 "Lowess Smoothing") label(2 "Bspline Smoothing") label(3 "True Desired Hours"))
		graph export "Output/Figures/Synthetic/hplus_bspline_lowess_monthly.png", as(png) replace 
	restore 
	
	* QUARTERLY COMPARISON 
	preserve 	
		collapse (mean) hhplus_lowess hhplus_CMA hhplus_LMA hhplus_bspline hplus, by(datdeb_q)
		
		twoway(line hhplus_CMA datdeb_q)(line hhplus_LMA datdeb_q)(line hplus datdeb_q, lcolor(gold)), xtitle("") ytitle("Desired Hours") legend(position(4) ring(0)label(1 "CMA Smoothing") label(2 "LMA Smoothing") label(3 "True Desired Hours"))
		graph export "Output/Figures/Synthetic/hplus_CMA_LMA_quarterly.png", as(png) replace 
		
		
		twoway(line hhplus_lowess datdeb_q, lcolor(orange))(line hhplus_bspline datdeb_q, lcolor(green))(line hplus datdeb_q, lcolor(gold)), xtitle("") ytitle("Desired Hours") legend(position(4) ring(0) label(1 "Lowess Smoothing") label(2 "Bspline Smoothing") label(3 "True Desired Hours"))
		graph export "Output/Figures/Synthetic/hplus_bspline_lowess_quarterly.png", as(png) replace 
	restore 
	
	
	
save "Data/Clean/SYNTH_2.dta", replace 
