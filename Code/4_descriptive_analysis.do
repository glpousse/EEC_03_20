****************************************
****************************************
********* DESCRIPTIVE ANALYSIS ********* 
****************************************
****************************************
cap ssc install estout, replace
cap ssc install gtools
cap ssc install coefplot

************************
**# Response Rates *****
************************
	
	* Table for Summarizing non-responses 
use Data/Clean/df_wedges, clear 
	keep if in_tepa 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	codebook wedge 				// Missing .: 36,782/44,429 --> 82.76
	codebook hplus 				// Missing .: 36,782/44,429 --> 82.76
	codebook empnbh 			// Missing .: 0/44,429		--> 0 
	codebook salred				// Missing .: 30,841/44,429	--> 69.43

use Data/Clean/df_wedges, clear 
	keep if datdeb < tepa_date 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	codebook wedge 				// Missing .: 153,737/184,480 --> 83.37
	codebook hplus 				// Missing .: 153,737/184,480 --> 83.37
	codebook empnbh 			// Missing .: 1/184,480		  --> 0
	codebook salred				// Missing .: 126,591/184,480 --> 68.62
	
use Data/Clean/df_wedges, clear 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	codebook wedge 				// Missing .: 290,156/342,604 --> 
	codebook hplus 				// Missing .: 290,156/342,604 -->
	codebook empnbh 			// Missing .: 2/342,604		  -->
	codebook salred				// Missing .: 234,952/342,604 -->
	
	* Table summarizing response rates for Hplus 
use Data/Clean/df_wedges, clear 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	tab rgi if hplus !=. 

	
************************
**# STATS FOR DEFENSE *****
************************
use Data/Clean/SYNTH_2, clear 
	keep if datdeb < tepa_date 
	keep if wedge !=.
	
	tabstat wedge, stat(mean) by(empnbh_cat)
	
	bysort empnbh_cat: 
	
	tabstat sexe age annees_etudes child nbenfc married ancentr empnbh emphre, by(tplus) stat(mean sd) column(statistics)
	


	
	
************************
**# STYLIZED FACTS *****
************************

use Data/Clean/SYNTH_2, clear 
keep if datdeb < tepa_date 

***** FACT 1 ***** 

	* The table
	tabstat wedge hplus empnbh,by(tplus) stat(mean sd n)
	//    tplus |     wedge     hplus    empnbh
	// ---------+------------------------------
	//        0 | -6.904109  36.04195  42.94606
	//          |   7.48135  8.909774  7.815418
	//          |      4527      4527      4527
	// ---------+------------------------------
	//        1 |  7.926261  43.61471  37.96623
	//          |  6.629422  6.077461  8.190416
	//          |     20315     20315    149318
	// ---------+------------------------------
	//    Total |  5.223697  42.23471  38.11276
	//          |  8.883424  7.294823  8.222782
	//          |     24842     24842    153845
	// ----------------------------------------

	dis "% of overworked =" (4527 / 24842) * 100
	dis "% of underworked =" (20315 / 24842) * 100

* The Mismatch CDF 
	cumul wedge, gen(cdf_wedge)
	twoway line cdf_wedge wedge, sort lwidth(thin) lcolor(blue) ///
		ytitle("Empirical Cumulative Probability") ///
		xtitle("Work Hours Mismatch") ///
		xline(0, lcolor(red) lpattern(dash))
	graph export "Output/Figures/Stylized_Facts/0_CDF.png", as(png) replace 
	
***** FACT 2 ***** 
	global controls sexe age_group educ_degree married child  
    global i_controls i.sexe i.age_group i.educ_degree i.married i.child   
	
* MISMATCH 
	reghdfe wedge $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	
	* Across Demographics (FACT 1)
    coefplot 	(m_sexe)(m_age_group) ///
				(m_educ_degree)(m_married)(m_child), ///
          horizontal ///
		  xtitle("Adjusted Work Hours Mismatch") ///
          ciopts(recast(rcap) lcolor(gs10)) ///
          mcolor(blue) msymbol(circle) ///
          nolabel legend(off) ///
          coeflabels(	1.sexe = "Male" 0.sexe = "Female" ///
						1.age_group = "18 - 24" ///
						2.age_group = "25 - 54" ///
						3.age_group = "55 - 64" ///
						1.educ_degree = "No Tertiary Education" ///
						2.educ_degree = "Vocational Training" ///
						3.educ_degree = "College Degree" ///
						1.married = "Married" 0.married = "Not Married" ///
						1.child = "Child in HH" 2.child = "No Child in HH")
    graph export "Output/Figures/Stylized_Facts/1_wedge_H_03-07.png", as(png) replace 

* HPLUS 
    reghdfe hplus $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	
	* Across Demographics (FACT 1)
    coefplot 	(m_sexe)(m_age_group) ///
				(m_educ_degree)(m_married)(m_child), ///
          horizontal ///
		  xtitle("Adjusted Desired Hours") ///
          ciopts(recast(rcap) lcolor(gs10)) ///
          mcolor(blue) msymbol(circle) ///
          nolabel legend(off) ///
          coeflabels(	1.sexe = "Male" 0.sexe = "Female" ///
						1.age_group = "18 - 24" ///
						2.age_group = "25 - 54" ///
						3.age_group = "55 - 64" ///
						1.educ_degree = "No Tertiary Education" ///
						2.educ_degree = "Vocational Training" ///
						3.educ_degree = "College Degree" ///
						1.married = "Married" 0.married = "Not Married" ///
						1.child = "Child in HH" 2.child = "No Child in HH" )
    graph export "Output/Figures/Stylized_Facts/1_hplus_H_03-07.png", as(png) replace 

* EMPNBH
    reghdfe empnbh $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	* Across Demographics (FACT 1)
    coefplot 	(m_sexe)(m_age_group) ///
				(m_educ_degree)(m_married)(m_child), ///
          horizontal ///
		  xtitle("Adjusted Actual Hours") ///
          ciopts(recast(rcap) lcolor(gs10)) ///
          mcolor(blue) msymbol(circle) ///
          nolabel legend(off) ///
          coeflabels(	1.sexe = "Male" 0.sexe = "Female" ///
          coeflabels(	1.sexe = "Male" 0.sexe = "Female" ///
						1.age_group = "18 - 24" ///
						2.age_group = "25 - 54" ///
						3.age_group = "55 - 64" ///
						1.educ_degree = "No Tertiary Education" ///
						2.educ_degree = "Vocational Training" ///
						3.educ_degree = "College Degree" ///
						1.married = "Married" 0.married = "Not Married" ///
						1.child = "Child in HH" 2.child = "No Child in HH" )
    graph export "Output/Figures/Stylized_Facts/1_empnbh_H_03-07.png", as(png) replace 
	
***** FACT 3 ***** 
	global controls sexe age_group educ_degree married child salmet 
    global i_controls i.sexe i.age_group i.educ_degree i.married i.child i.salmet 
	global controls salmet
* MISMATCH    
	reghdfe wedge i.salmet  [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	
	* Across Income Brackets 
	coefplot 	(m_salmet), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Work Hours Mismatch") ///
				nolabel legend(off) ///
				coeflabels( 2.salmet = "SMIC-999" 3.salmet = "1,000–1,249" ///
				4.salmet = "1,250–1,499" 5.salmet = "1,500–1,999" 6.salmet = "2,000–2,499" ///
				7.salmet = "2,500–2,999" 8.salmet = "3,000–4,999" 9.salmet = "5,000–7,999" ///
				10.salmet = "8,000+" 98.salmet = "Non-Response" )
	graph export "Output/Figures/Stylized_Facts/2_wedge_H_03-07.png", as(png) replace
	
* HPLUS    
	reghdfe hplus $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	
	* Across Income Brackets 
	coefplot 	(m_salmet), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Desired Hours") ///
				nolabel legend(off) ///
				coeflabels( 2.salmet = "SMIC-999" 3.salmet = "1,000–1,249" ///
				4.salmet = "1,250–1,499" 5.salmet = "1,500–1,999" 6.salmet = "2,000–2,499" ///
				7.salmet = "2,500–2,999" 8.salmet = "3,000–4,999" 9.salmet = "5,000–7,999" ///
				10.salmet = "8,000+" 98.salmet = "Non-Response" )
	graph export "Output/Figures/Stylized_Facts/2_hplus_H_03-07.png", as(png) replace
	
* EMPNBH 
	global controls sexe age_group educ_degree married child salmet 
    global i_controls i.sexe i.age_group i.educ_degree i.married i.child i.salmet 
    
	reghdfe wedge $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	
	* Across Income Brackets 
	coefplot 	(m_salmet), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Actual Hours") ///
				nolabel legend(off) ///
				coeflabels( 2.salmet = "SMIC-999" 3.salmet = "1,000–1,249" ///
				4.salmet = "1,250–1,499" 5.salmet = "1,500–1,999" 6.salmet = "2,000–2,499" ///
				7.salmet = "2,500–2,999" 8.salmet = "3,000–4,999" 9.salmet = "5,000–7,999" ///
				10.salmet = "8,000+" 98.salmet = "Non-Response" )
	graph export "Output/Figures/Stylized_Facts/2_empnbh_H_03-07.png", as(png) replace
	
	/*
		In the above regression, I do not include empnbh_cat in order to avoid mechanically
		controlling for something which defines the dependent variable.
		
		However, to study the wedge across different wage brackets, I include them in the next regression.
	*/
	
***** FACT 4 ***** 
	global controls sexe age_group educ_degree married child salmet empnbh_cat
    global i_controls i.sexe i.age_group i.educ_degree i.married i.child i.salmet i.empnbh_cat

* MISMATCH 
	reghdfe wedge $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model

    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	* Across Hours Worked
	coefplot 	(m_empnbh_cat), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Work Hours Mismatch") ///
				nolabel legend(off) ///
				keep(5.empnbh_cat 6.empnbh_cat 7.empnbh_cat 8.empnbh_cat 9.empnbh_cat 10.empnbh_cat 11.empnbh_cat) ///
				coeflabels(5.empnbh_cat = "35–39" 6.empnbh_cat = "40–44" 7.empnbh_cat = "45–49" ///
				8.empnbh_cat = "50–54" 9.empnbh_cat = "55–59" 10.empnbh_cat = "60–64" 11.empnbh_cat = "65+")
	graph export "Output/Figures/Stylized_Facts/3_wedge_H_03-07.png", as(png) replace

* HPLUS 
	reghdfe hplus $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model

    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	* Across Hours Worked
	coefplot 	(m_empnbh_cat), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Desired Hours") ///
				nolabel legend(off) ///
				keep(5.empnbh_cat 6.empnbh_cat 7.empnbh_cat 8.empnbh_cat 9.empnbh_cat 10.empnbh_cat 11.empnbh_cat) ///
				coeflabels(5.empnbh_cat = "35–39" 6.empnbh_cat = "40–44" 7.empnbh_cat = "45–49" ///
				8.empnbh_cat = "50–54" 9.empnbh_cat = "55–59" 10.empnbh_cat = "60–64" 11.empnbh_cat = "65+")
	graph export "Output/Figures/Stylized_Facts/3_hplus_H_03-07.png", as(png) replace

* EMPNBH 
	reghdfe empnbh $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model

    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	* Across Hours Worked
	coefplot 	(m_empnbh_cat), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Actual Hours") ///
				nolabel legend(off) ///
				keep(5.empnbh_cat 6.empnbh_cat 7.empnbh_cat 8.empnbh_cat 9.empnbh_cat 10.empnbh_cat 11.empnbh_cat) ///
				coeflabels(5.empnbh_cat = "35–39" 6.empnbh_cat = "40–44" 7.empnbh_cat = "45–49" ///
				8.empnbh_cat = "50–54" 9.empnbh_cat = "55–59" 10.empnbh_cat = "60–64" 11.empnbh_cat = "65+")
	graph export "Output/Figures/Stylized_Facts/3_empnbh_H_03-07.png", as(png) replace
	

**# ROBUSTNESS *****	
use Data/Clean/SYNTH_2, clear 
	keep if datdeb < tepa_date 
	
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

* FACT 1 
	cumul wedge1, gen(cdf_wedge1)
	twoway line cdf_wedge1 wedge1, sort lwidth(thin) lcolor(blue) ///
		ytitle("Empirical Cumulative Probability") ///
		xtitle("Work Hours Mismatch") ///
		xline(0, lcolor(red) lpattern(dash))
	graph export "Output/Figures/Stylized_Facts/0_CDF_robustness.png", as(png) replace 

* FACT 2 
	global controls sexe age_group educ_degree married child 
    global i_controls i.sexe i.age_group i.educ_degree i.married i.child 
	
	* MISMATCH 
	reghdfe wedge1 $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	
	* Across Demographics (FACT 1)
    coefplot 	(m_sexe)(m_age_group) ///
				(m_educ_degree)(m_married)(m_child), ///
          horizontal ///
		  xtitle("Adjusted Work Hours Mismatch") ///
          ciopts(recast(rcap) lcolor(gs10)) ///
          mcolor(blue) msymbol(circle) ///
          nolabel legend(off) ///
          coeflabels(	1.sexe = "Male" 0.sexe = "Female" ///
						1.age_group = "18 - 24" ///
						2.age_group = "25 - 54" ///
						3.age_group = "55 - 64" ///
						1.educ_degree = "No Tertiary Education" ///
						2.educ_degree = "Vocational Training" ///
						3.educ_degree = "College Degree" ///
						1.married = "Married" 0.married = "Not Married" ///
						1.child = "Child in HH" 2.child = "No Child in HH")
    graph export "Output/Figures/Stylized_Facts/1_wedge_H_03-07_ROBUST.png", as(png) replace 


* FACT 3
	global controls sexe age_group educ_degree married child cat_salred1 
    global i_controls i.sexe i.age_group i.educ_degree i.married i.child i.cat_salred1
	* MISMATCH    
	reghdfe wedge1 $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model
	
    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	
	* Across Income Brackets 
	coefplot 	(m_cat_salred1, drop(1.cat_salred1)), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Work Hours Mismatch") ///
				nolabel legend(off) ///
				coeflabels( 2.cat_salred1 = "SMIC-999" 3.cat_salred1 = "1,000–1,249" ///
				4.cat_salred1 = "1,250–1,499" 5.cat_salred1 = "1,500–1,999" 6.cat_salred1 = "2,000–2,499" ///
				7.cat_salred1 = "2,500–2,999" 8.cat_salred1 = "3,000–4,999" 9.cat_salred1 = "5,000–7,999" ///
				10.cat_salred1 = "8,000+" 98.cat_salred1 = "Non-Response" )
	graph export "Output/Figures/Stylized_Facts/2_wedge_H_03-07_ROBUST.png", as(png) replace

* FACT 4
	global controls sexe age_group educ_degree married child cat_salred1 empnbh_cat
    global i_controls i.sexe i.age_group i.educ_degree i.married i.child i.cat_salred1 i.empnbh_cat

* MISMATCH 
	reghdfe wedge1 $i_controls [pweight = extri], absorb(datdeb) cluster(indiv_num)
    estimates store base_model

    foreach var in $controls {
        estimates restore base_model
        margins `var', post
        estimates store m_`var'
    }
	* Across Hours Worked
	coefplot 	(m_empnbh_cat), ///
				horizontal ///
				ciopts(recast(rcap) lcolor(gs10)) ///
				mcolor(blue) msymbol(circle) ///
				xtitle("Adjusted Work Hours Mismatch") ///
				nolabel legend(off) ///
				keep(5.empnbh_cat 6.empnbh_cat 7.empnbh_cat 8.empnbh_cat 9.empnbh_cat 10.empnbh_cat 11.empnbh_cat) ///
				coeflabels(5.empnbh_cat = "35–39" 6.empnbh_cat = "40–44" 7.empnbh_cat = "45–49" ///
				8.empnbh_cat = "50–54" 9.empnbh_cat = "55–59" 10.empnbh_cat = "60–64" 11.empnbh_cat = "65+")
	graph export "Output/Figures/Stylized_Facts/3_wedge_H_03-07_ROBUST.png", as(png) replace

	

*****************
**# FIGURES *****
*****************

***** FIGURE: EMPNBH HPLUS 2003-2010 *****
use Data/Clean/SYNTH_2.dta, clear

	preserve 
		collapse (mean) hplus empnbh [pweight=extri], by(datdeb_q)
		twoway(bar empnbh datdeb_q, barwidth(0.8)),
			 xtitle("") ytitle("Desired Hours") xline(`=date("2007-10-01", "YMD")') 
	restore 

	


******************
******************
**# APPENDIX *****
******************
******************

************************************************************************
**# APPENDIX : Statistics on Individuals having Responded to HPLUS *****
************************************************************************

* TOTAL 
use Data/Clean/df_wedges, clear 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	cap drop hrep
	gen hrep 		= . 
	replace hrep 	= 1 if hplus != .	
	replace hrep 	= 0 if hplus == . 
	* By Socio-Demographic Group
	tabstat sexe age annees_etudes child nbenfc married ancentr empnbh emphre, by(hrep) stat(mean sd) column(statistics)
	* T-TEST on the means between hrep categories  
	foreach var in sexe age annees_etudes child nbenfc married ancentr empnbh emphre{
		dis "T-test for variable `var'"
		ttest `var', by(hrep)
	}
	* Desired Hours Distribution of non-repsonses by Job industry
	tab naf10 if hrep == 1
	tab naf10 if hrep == 0 
	* Wage distributions
	twoway (hist salred if salred < 10000 & hrep ==0, percent color(%60) fcolor(red))(hist salred if salred < 10000 & hrep == 1, color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Individuals Having Responded to the Desired Hours Question") label(2 "Individuals Having not Responded to the Desired Hours Question")) xtitle("Net Monthly Income")
	graph export "Output/Figures/Descriptives/hplus_wages_TOTAL.png", as(png) replace

* DESCRIPTIVE
use Data/Clean/df_wedges, clear 
	keep if datdeb < tepa_date
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	cap drop hrep
	gen hrep 		= . 
	replace hrep 	= 1 if hplus != .	
	replace hrep 	= 0 if hplus == . 
	* By Socio-Demographic Group
	tabstat sexe age annees_etudes child nbenfc married ancentr empnbh emphre, by(hrep) stat(mean sd) column(statistics)
	* T-TEST on the means between hrep categories  
	foreach var in sexe age annees_etudes child nbenfc married ancentr empnbh emphre{
		dis "T-test for variable `var'"
		ttest `var', by(hrep)
	}
	* Desired Hours Distribution of non-repsonses by Job industry
	tab naf10 if hrep == 1
	tab naf10 if hrep == 0 
	* Wage distributions
	twoway (hist salred if salred < 10000 & hrep ==0, percent color(%60) fcolor(red))(hist salred if salred < 10000 & hrep == 1, color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Individuals Having Responded to the Desired Hours Question") label(2 "Individuals Having not Responded to the Desired Hours Question")) xtitle("Net Monthly Income")
	graph export "Output/Figures/Descriptives/hplus_wages_DESCRIPTIVE.png", as(png) replace

* TEPA  
use Data/Clean/df_wedges, clear 
	keep if in_tepa
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	cap drop hrep
	gen hrep 		= . 
	replace hrep 	= 1 if hplus != .	
	replace hrep 	= 0 if hplus == . 
	* By Socio-Demographic Group
	tabstat sexe age annees_etudes child nbenfc married ancentr empnbh emphre, by(hrep) stat(mean sd) column(statistics)
	* T-TEST on the means between hrep categories  
	foreach var in sexe age annees_etudes child nbenfc married ancentr empnbh emphre{
		dis "T-test for variable `var'"
		ttest `var', by(hrep)
	}
	* Desired Hours Distribution of non-repsonses by Job industry
	tab naf10 if hrep == 1
	tab naf10 if hrep == 0 
	* Wage distributions
	twoway (hist salred if salred < 10000 & hrep ==0, percent color(%60) fcolor(red))(hist salred if salred < 10000 & hrep == 1, color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Individuals Having Responded to the Desired Hours Question") label(2 "Individuals Having not Responded to the Desired Hours Question")) xtitle("Net Monthly Income")
	graph export "Output/Figures/Descriptives/hplus_wages_TEPA.png", as(png) replace

	
**********************************************
**# APPENDIX : Statistics on Hours Worked ****
**********************************************

use Data/Clean/p_df_wedges, clear
	keep if in_tepa 
/*
	nbhp - Dans le cadre de votre emploi principal quel est (ou : était) le nombre d'heures
	normalement prévu par semaine pour vous (en moyenne) ? 
	
	hhc - Dans le cadre de votre emploi principal (ou : dans le cadre de votre temps partiel), en moyenne
	combien d'heures travaillez-vous par semaine ?
	
	empnbh - La semaine du lundi... au dimanche..., combien d'heures avez-vous effectuées
	dans votre emploi principal ? (Ne pas compter les heures ou jours de congés ordinaires,
	exceptionnels, fériés, ponts, RTT, récupération, congé personnel non rémunéré, chômage partiel,
	activité de formation, grève, conflit du travail)
*/

*** Gender, Age, Education, Avg. Net Income, Industry ***

label define sexe_lbl 0 "Women" 1 "Men"
label values sexe sexe_lbl

forvalues i= 0/1 {
    foreach var in nbhp hhc empnbh {
        estpost summarize `var' if sexe == `i'
        eststo `var'_g`i'
    }
}

foreach v in nbhp hhc empnbh {
    estpost summarize `v'
    eststo `v'_total
}

	esttab ///
    hhc_g0  hhc_g1  hhc_total ///
    empnbh_g0 empnbh_g1 empnbh_total ///
    using "Output/Tables/work_summary.tex", ///
    cells("mean(fmt(2)) sd(par fmt(2)) count(fmt(0))") ///
    noobs nonumber nomtitles booktabs label ///
    collabels("(1)" "(2)" "(3)") ///
    replace

***********************************************
**# APPENDIX : Statistics on the Mismatch *****
***********************************************
use Data/Clean/SYNTH_2, clear 
	keep if in_tepa 
	
	* Gender
	tabstat hplus empnbh wedge, stat(mean sd n) by(sexe)
	ttest hplus, by(sexe)
	ttest empnbh, by(sexe)
	ttest wedge, by(sexe)

	
	* Age
	tabstat hplus empnbh wedge, stat(mean sd n) by(age_g)
		forvalues i=1/5{
			ttest hplus = empnbh if age_g == `i'
		}
		
	* Education
	tabstat hplus empnbh wedge, stat(mean sd n) by(educ_degree)
	forvalues i =1/3 {
		ttest hplus = empnbh if educ_degree == `i'
	}
	
	* Industry 
	tabstat hplus empnbh wedge, stat(mean sd n) by(naf10)
	levelsof naf10, local(codes)
	foreach code of local codes {
		display "Industry `code'"
		ttest hplus = empnbh if naf10 == "`code'"
	}
	
	* Income 
	tabstat hplus empnbh wedge, stat(mean sd n) by(cat_salred1)
	forvalues i=3/10{
			ttest hplus = empnbh if salmet  == `i'
		}
**********************************************
**# APPENDIX : Evolution of the Mismatch *****
**********************************************


*******************************************************
**# APPENDIX : Statistics on Cross-Border Workers *****
*******************************************************

use Data/Clean/SYNTH_2, clear 

 	* Sample 
	keep if in_tepa 
	keep if datdeb < tepa_date
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	keep if in_tepa 
	
	* Dummy 
	gen cross 	= (trans_bel == 1 | trans_ger == 1| trans_swz == 1 | trans_lux == 1)
	gen domestic  	= (dom_bel == 1 | dom_ger == 1 | dom_swz == 1 | dom_lux == 1) & france == 1
	gen rof = domestic !=1 & cross != 1 & france == 1 
	gen others = (cross!=1 & domestic !=1 & rof !=1) 
	
	gen cat =.
	replace cat = 1 if (trans_bel == 1 | trans_ger == 1| trans_swz == 1 | trans_lux == 1) 			// cross-border workers 
	replace cat = 2 if (dom_bel == 1 | dom_ger == 1 | dom_swz == 1 | dom_lux == 1) & france == 1	// domestic workers 
	replace cat = 3 if cat != 1 & cat != 2 & france ==1 											// rest of France 
	tab cat

	* General Characteristics
	tabstat sexe age annees_etudes child nbenfc married ancentr hplus empnbh wedge abs_wedge emphre tplus , by(cat) stat(mean sd) column(statistics)
	
	* T-tests on the means: 
	replace cat = . if cat == 3
	foreach var in sexe age annees_etudes child nbenfc married ancentr hplus empnbh wedge abs_wedge emphre tplus{
		dis "T-test for variable `var'"
		ttest `var', by(cat)
	}
	
	* MISMATCH Distrib
	twoway (hist wedge if cat ==1 & wedge < 20 & wedge >-20, width(1)  percent color(%60) fcolor(red))(hist wedge if cat==2 & wedge < 20 & wedge > -20, width(1)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Cross-border Workers") label(2 "Domestic Workers")) xtitle("Mismatch (Before October 2007)")
	graph export "Output/Figures/Descriptives/XBorder_mismatch.png", as(png) replace
	
	* ABSOLUTE MISMATCH Distrib
	twoway (hist abs_wedge if cat ==1 & wedge < 20 & wedge >-20, width(1)  percent color(%60) fcolor(red))(hist abs_wedge if cat==2 & wedge < 20 & wedge >-20, width(1)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Cross-Border Workers") label(2 "Domestic Workers")) xtitle("Absolute Mismatch (Before October 2007)")
	graph export "Output/Figures/Descriptives/XBorder_abs_mismatch.png", as(png) replace
	
	* HPLUS Distrib 
	twoway (hist hplus if cat ==1 & hplus > 34 & hplus < 71, width(2)  percent color(%60) fcolor(red))(hist hplus if cat==2 & hplus > 34 & hplus < 71, width(2)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Cross-Border Workers") label(2 "Domestic Workers")) xtitle("Desired Hours (Before October 2007)")
	graph export "Output/Figures/Descriptives/XBorder_hplus.png", as(png) replace
	
	* EMPNBH Distrib 
	twoway (hist empnbh if cat ==1 , width(2)  percent color(%60) fcolor(red))(hist hplus if cat==2 , width(2)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Cross-Border Workers") label(2 "Domestic Workers")) xtitle("Actual Hours (Before October 2007)")
	graph export "Output/Figures/Descriptives/XBorder_empnbh.png", as(png) replace
	
	* WAGE DISTRIB
	twoway (hist salred if salred < 10000 & cat==1, percent color(%60) fcolor(red))(hist salred if salred < 10000 & cat == 2, color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Cross-Border Workers") label(2 "Domestic Workers")) xtitle("Net Monthly Income (Before October 2007)")
	graph export "Output/Figures/Descriptives/XBorder_wages.png", as(png) replace
	
	
	* Mom and Dad's Occupation
	* Recategorizing the CSPP and CSPM variabels: 
	
	foreach i in m p {
	gen `i'_socio_group = .

	replace `i'_socio_group = 1 if inlist(csp`i', 10, 69, 71)							// Agriculture 
	replace `i'_socio_group = 2 if inlist(csp`i', 21, 22, 23, 63, 72)					// Artisans & Small Business
	replace `i'_socio_group = 3 if inlist(csp`i', 31, 33, 34, 35, 37, 38, 74)			// Liberal & Executive
	replace `i'_socio_group = 4 if inlist(csp`i', 42, 43, 44, 45, 46, 47, 48, 75)		// Intermediate Professions
	replace `i'_socio_group = 5 if inlist(csp`i', 52, 53, 54, 55, 56, 77)				// Employees
	replace `i'_socio_group = 6 if inlist(csp`i', 62, 64, 65, 67, 68, 78)				// Workers (Ouvriers)
	replace `i'_socio_group = 7 if inlist(csp`i', 00, 81, 82)							// Unemployed or Inactives
	
	label define socio_lbl`i' 	1 "Agricultural and Rural Laborers" 2 "Independent Trades and Small Business Owners" ///
								3 "Senior Professionals and Executives" ///
								4 "Skilled Technicians and Intermediate Occupations" ///
								5 "Administrative, Service and Sales Employees" 6 "Manual and Industrial Workers" ///
								7 "Economically Inactive and Unclassified"
	label values `i'_socio_group socio_lbl`i'
	}

	* Mom's Occupation 
	table m_socio_group if cat ==1, statistic(percent)
	table m_socio_group if cat ==2, statistic(percent)
	table m_socio_group if cat ==3, statistic(percent) 
	
	* Dad's Occupation 
	table p_socio_group if cat ==1, statistic(percent) 
	table p_socio_group if cat ==2, statistic(percent) 
	table p_socio_group if cat ==3, statistic(percent) 
	
******************************************************************
**# APPENDIX : Statistics on Hours and Days Contract Workers *****
******************************************************************

use Data/Clean/SYNTH_2, clear 

 	* Sample 
	keep if in_tepa 
	//keep if datdeb < tepa_date 
	keep if CDI 
	keep if full_time 
	keep if du_hhc
	keep if no_interruption
	keep if mod_agree ==0
 	keep if min_smic
	
	* Dummy
	gen treatment 	= (forfait == 2)
	gen control 	= (forfait ==1)
	drop if (treatment ==. | control ==.) 
	gen post_treatment = post_tepa*treatment 
	
	gen cat2 =.
	replace cat2 = 1 if treatment ==1
	replace cat2 = 2 if control == 1
	replace cat2 = 3 if treatment != 1 & control != 1
	tab cat2

	* General Characteristics
	tabstat sexe age annees_etudes child nbenfc married ancentr hplus empnbh wedge abs_wedge emphre tplus, by(cat2) stat(mean sd) column(statistics)
	
	* T-tests on the means: 
	replace cat2 = . if cat2 == 3
	foreach var in sexe age annees_etudes child nbenfc married ancentr hplus empnbh wedge abs_wedge emphre tplus{
		dis "T-test for variable `var'"
		ttest `var', by(cat2)
	}
	
	
	* MISMATCH Distrib
	twoway (hist wedge if cat2 ==1 & wedge < 20 & wedge >-20, width(1)  percent color(%60) fcolor(red))(hist wedge if cat2==2 & wedge < 20 & wedge >-20, width(1)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Workers on Hours Contracts") label(2 "Workers on Days Contracts")) xtitle(" Mismatch (Before October 2007)")
	graph export "Output/Figures/Descriptives/IDforfait_mismatch.png", as(png) replace
	
	* ABSOLUTE MISMATCH Distrib
	twoway (hist abs_wedge if cat2 ==1 & wedge < 20 & wedge >-20, width(1)  percent color(%60) fcolor(red))(hist abs_wedge if cat2==2 & wedge < 20 & wedge >-20, width(1)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Workers on Hours Contracts") label(2 "Workers on Days Contracts")) xtitle("Absolute Mismatch (Before October 2007)")
	graph export "Output/Figures/Descriptives/IDforfait_abs_mismatch.png", as(png) replace
	
	* HPLUS Distrib 
	twoway (hist hplus if cat2 ==1 & hplus > 34 & hplus < 71, width(2)  percent color(%60) fcolor(red))(hist hplus if cat2==2 & hplus > 34 & hplus < 71, width(2)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Workers on Hours Contracts") label(2 "Workers on Days Contracts")) xtitle("Desired Hours (Before October 2007)")
	graph export "Output/Figures/Descriptives/IDforfait_hplus.png", as(png) replace
	
	* EMPNBH Distrib 
	twoway (hist empnbh if cat2 ==1 , width(2)  percent color(%60) fcolor(red))(hist hplus if cat2==2 , width(2)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Workers on Hours Contracts") label(2 "Workers on Days Contracts")) xtitle("Actual Hours (Before October 2007)")
	graph export "Output/Figures/Descriptives/IDforfait_empnbh.png", as(png) replace
	
	* WAGE DISTRIB
	twoway (hist salred if salred < 10000 & cat2==1 , percent color(%60) fcolor(red))(hist salred if salred < 10000 & cat2 == 2, color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Workers on Hours Contracts") label(2 "Workers on Days Contracts")) xtitle("Net Monthly Income (Before October 2007)")
	graph export "Output/Figures/Descriptives/IDforfait_wages.png", as(png) replace

	
***************************************************
**# APPENDIX : STATISTICS ON OPTIMIZERS (ID2) *****
***************************************************

	* General Characteristics
	tabstat sexe age annees_etudes child nbenfc married ancentr hplus empnbh wedge abs_wedge emphre tplus, by(optim) stat(mean sd) column(statistics)
	
	* T-tests on the means: 
	foreach var in sexe age annees_etudes child nbenfc married ancentr hplus empnbh wedge abs_wedge emphre tplus{
		dis "T-test for variable `var'"
		ttest `var', by(optim)
	}
	
	* MISMATCH Distrib
	twoway (hist wedge if optim ==1 & wedge < 20 & wedge >-20, width(1)  percent color(%60) fcolor(red))(hist wedge if optim==0 & wedge < 20 & wedge >-20, width(1)  color(%60) percent  fcolor(blue)), legend( size(small) position(10) ring(0) label(1 "Work-Schedule Optimizers") label(2 "Work-Schedule Non-Optimizers")) xtitle(" Mismatch")
	graph export "Output/Figures/Descriptives/ID2_OPTIM_mismatch.png", as(png) replace
	
	* ABSOLUTE MISMATCH Distrib
	twoway (hist abs_wedge if optim ==1 & wedge < 20 & wedge >-20, width(1)  percent color(%60) fcolor(red))(hist abs_wedge if cat2==2 & wedge < 20 & wedge >-20, width(1)  color(%60) percent  fcolor(blue)), legend( size(small) position(1) ring(0) label(1 "Work-Schedule Optimizers") label(2 "Work-Schedule Non-Optimizers")) xtitle("Absolute Mismatch")
	graph export "Output/Figures/Descriptives/ID2_OPTIM_abs_mismatch.png", as(png) replace
	
	* HPLUS Distrib 
	twoway (hist hplus if optim ==1 & hplus > 34 & hplus < 71, width(2)  percent color(%60) fcolor(red))(hist hplus if cat2==2 & hplus > 34 & hplus < 71, width(2)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Work-Schedule Optimizers") label(2 "Work-Schedule Non-Optimizers")) xtitle("Desired Hours")
	graph export "Output/Figures/Descriptives/ID2_OPTIM_hplus.png", as(png) replace
	
	* EMPNBH Distrib 
	twoway (hist empnbh if optim ==1 , width(2)  percent color(%60) fcolor(red))(hist hplus if cat2==2 , width(2)  color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Work-Schedule Optimizers") label(2 "Work-Schedule Non-Optimizers")) xtitle("Actual Hours")
	graph export "Output/Figures/Descriptives/ID2_OPTIM_empnbh.png", as(png) replace
	
	* WAGE DISTRIB
	twoway (hist salred if salred < 10000 & optim==1 , percent color(%60) fcolor(red))(hist salred if salred < 10000 & cat2 == 2, color(%60) percent  fcolor(blue)), legend( size(small) position(2) ring(0) label(1 "Work-Schedule Optimizers") label(2 "Work-Schedule Non-Optimizers")) xtitle("Net Monthly Income")
	graph export "Output/Figures/Descriptives/ID2_OPTIM_wages.png", as(png) replace



**********************************************************************
**# APPENDIX : STATISTICS ON WORKERS ACROSS THE WAGE DISTIBUTION *****
**********************************************************************

	
	

***** Playing wit hplus distributions by socio-group *****
twoway (hist w if datdeb < tepa_date, color(%30) fcolor(navy))(hist hplus if datdeb > tepa_date, color(%30) fcolor(maroon)) // if or by()

twoway (hist wedge if treatment ==1, color(%30) fcolor(navy))(hist wedge if treatment==0, color(%30) fcolor(maroon)), legend(label(1 "Treated") label(2 "Control")) 

************************************************************************************************
************************************************************************************************
************************************************************************************************
**# Moved from "4_TEPA_analysis.do " --    
************************************************************************************************
************************************************************************************************
************************************************************************************************
************************************************************************************************



****************************
**# Avg. Wedge by Year *****
**************************** 

use Data/Clean/p_df_wedges, clear
	

	global controls male female age_group educ_degree married not_married child no_child salmet
	
    gen tplus = .
 	replace tplus = 1 if wedge > 0
 	replace tplus = 0 if (wedge == 0 | wedge < 0)
	
	logit tplus i.annee [pweight = extri], cluster(indiv_num) 
	
	margins annee

	marginsplot, ///
				name(margplot, replace) ///
				title(" P(Mimsatch > 0), Non-adjusted Linear Prediction") ///
				ytitle("Non-Adjusted Probability") ///
				xtitle("Year")
				
	graph export "Output/Figures/Wedge/mean_p(mismatch).png", as(png) replace
	
	preserve
	
	egen avg_2003_2007 = mean(wedge) if datdeb < tepa_date
	egen avg_2007_2013 = mean(wedge) if datdeb > tepa_date & datdeb < abrog_date
	egen avg_2013_2019 = mean(wedge) if datdeb > abrog_date
	
	collapse (mean) wedge avg_2003_2007 avg_2007_2013 avg_2013_2019 [pweight=extri], by (datdeb_q)

	twoway ///
    (bar wedge datdeb_q, color(navy))(bar avg_2003_2007 datdeb_q, color(red%25))(bar avg_2007_2013 datdeb_q, color(red%25))(bar avg_2013_2019 datdeb_q, color(red%25)), ///
    xtitle("Quarter") ytitle("Mean Positive Wedge") ///
    xline(`=tq(2007q4)' `=tq(2012q3)', lpattern(dash) lcolor(red)) ///
    title("Wedge Over Time")
	
	graph export "Output/Figures/Wedge/mean_wedge_q.png", as(png) replace
	
	
	restore 

****************************
**# Wedge Distribution *****
****************************

use Data/Clean/p_df_wedges, clear

	forvalues i = 2003/2020{
		
		kdensity wedge if annee == `i', width(1) title(Wedge Distribution (`i'))
	
		graph export "Output/Figures/Distributions/W_Distrib_`i'.png", as(png) replace
		
		kdensity empnbh if annee == `i', width(1) title(Empnbh Distribution (`i'))
			
		graph export "Output/Figures/Distributions/empnbh_Distrib_`i'.png", as(png) replace
	}

	preserve
		collapse (mean) prop_pos_wedge = pos_wedge (count) n=pos_wedge [pweight=extri], by(empnbh_round)
		twoway (line prop_pos_wedge empnbh_round), ///
		ytitle("Proportion with Positive Wedge") ///
		xtitle("Weekly Hours Worked") ///
		title("Positive Wedge Rate by Hours Worked")
	restore 
	
kdensity empnbh if annee > 2007 & annee <=2012, xline(35 40)
	
	graph export "Output/after_tepa_before abrog.png", as(png) replace 
	
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

use Data/Clean/df_wedges, clear
	
	global controls sexe age_group educ_degree married enfant
	
	reghdfe salred i.empnbh_cat $controls if datdeb_q <= tq(2007q3) [pweight=extri], absorb(datdeb) cluster(indiv_num)
	
	margins empnbh_cat, post 
	
	estimates store wage_profile
	
	coefplot wage_profile,	keep(*.empnbh_cat*) vertical ///
							drop(_cons) ///
							ciopts(recast(rcap)) msymbol(O) ///
							mcolor(black) lcolor(black) ///
							ylabel(, angle(horizontal)) ///
							xtitle("Hours Bin (Worked in the Reference Week)") ytitle("Predicted Monthly Wage (Adjusted)") ///
							title("The Wage-Hours Profile (2003-2007, OG Sample)") ///
							xlabel(, angle(45))
		
	graph export "Output/Figures/Wage_Hours_Profile/og_Wage_Hours_Profile_03-07.png", as(png) replace
	
	* log hourly wage 
	


							****** CONFUSED ABOUT THESE GRAPHS *****
					****** Wage Monotonically Decreasing in Hours ??? ******
								    				

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
	
	collapse (count) empnbh [pweight=extri], by(empnbh_cat)
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
	
**********************************************
**# Desired vs Actual Hours Distribution ***** 
**********************************************

use Data/Clean/p_df_wedges, clear
	
	* Pre-2007
	preserve
		keep if annee < 2008 
		twoway(hist empnbh, fraction color(blue%50))(hist hplus, fraction color(red%50)), legend(label(1 "Actual Hours") label(2 "Desired Hours"))
	
		twoway(kdensity empnbh, color(blue%50))(kdensity hplus, color(red%50)), legend(label(1 "Actual Hours") label(2 "Desired Hours"))
	restore 
	
	*TEPA
	preserve 
		keep if in_tepa
		twoway(hist empnbh, fraction color(blue%50))(hist hplus, fraction color(red%50)), legend(label(1 "Actual Hours") label(2 "Desired Hours"))
	
		twoway(kdensity empnbh, color(blue%50))(kdensity hplus, color(red%50)), legend(label(1 "Actual Hours") label(2 "Desired Hours"))
	restore 
		
 
**************
**# CDFs ***** 
**************

use Data/Clean/SYNTH_2, clear

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
			yscale(range(0 1)) ///
			legend(order(1 "Actual Hours" 2 "Mean Actual Hours" 3 "Desired Hours" 4 "Mean Desired Hours")ring(0) position(4))
		
	graph export "Output/Figures/INTRO/CDF_preTEPA.png", as(png) replace
	
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
		collapse (mean) wedge log_empnbh [pweight=extri], by(quantile_empnbh)

		twoway 	(scatter wedge log_empnbh, msymbol(o) mcolor(blue)) ///
				(lfit wedge log_empnbh, lcolor(red%75) lpattern(dash)), ///
				xtitle("Log Weekly Hours") ytitle("Wedge") ///
				title("Scatter Plot with Fitted Line")
	restore 
	
	* Log-Monthly Income
	
	preserve 
		collapse (mean) wedge log_salred [pweight=extri], by(quantile_salred)
	
		twoway 	(scatter wedge log_salred, msymbol(o) mcolor(blue)) ///
				(lfit wedge log_salred, lcolor(red%75) lpattern(dash)), ///
				xtitle("Log Monthly Income") ytitle("Wedge") ///
				title("Scatter Plot with Fitted Line")
	restore 
	
**************************
**# Frish Elasticity *****
**************************

use Data/Clean/df_wedges, clear

	keep if annee < 2008
	
	reghdfe log_empnbh log_salred [pweight=extri], absorb(indiv_num) vce(cluster indiv_num) 
	
	* Alarmingly low value: 0.02 ** 

********************************************************************
**# Revealed Preference Estimation of the Wage–Effort Tradeoff *****
********************************************************************

use Data/Clean/p_df_wedges, clear 

	drop if hhc < 35 
	drop if hhc > 70 
	drop if hourly_salred < smic_h
	drop if hourly_salred > 100
	gen ratio_1 = empnbh / hourly_salred // I take \nu = 2 --> Quadratic disutility of working hours
	
	hist ratio_1 
	tabstat ratio_1, stat(mean p50)
// 	 Variable |      Mean       p50
// -------------+--------------------
//      ratio_1 |  2.899559  2.942265
// ----------------------------------

// found that:
// •	Most workers report wanting more hours (i.e., desired – actual hours > 0)
// •	But the estimated h∗wi\frac{h^*}{w_i}wih∗ ratios don't show a large mass above the mean
// This is still consistent with model. Here's how:
// The actual ratio h∗/wih^*/w_ih∗/wi is not their ideal — it's a uniform outcome from bargaining.
// Workers with high θi/λi\theta_i / \lambda_iθi/λi find the contract unsatisfactory, and report wanting more hours.
// The gap between the ratio and the worker's ideal shows up in stated preferences, not behavior.
// So  estimates are not invalid — they simply show that:
// •	The uniformly bargained hours (perhaps near 35–40) are too low for many
// •	The distribution of true θi/λi\theta_i / \lambda_iθi/λi is right-skewed, but can't be recovered from behavior because the outcome is fixed by the union

**********************
**# WEDGE GRAPHS *****
**********************

use Data/Clean/p_df_wedges, clear
	keep if wedge < 0
	collapse (mean) wedge [pweight=extri], by(datdeb_q)
	twoway (bar wedge datdeb_q), ///
    xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
    xtitle("Quarter") xline(15) ///
    ylabel(-13(1)0)
	
	use Data/Clean/p_df_wedges, clear
	keep if wedge > 0
	collapse (mean) wedge [pweight=extri], by(datdeb_q)
	twoway (bar wedge datdeb_q), ///
    xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
    xtitle("Quarter") xline(15) ///
    ylabel(6(1)11)



