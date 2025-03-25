*******************************
*******************************
********* DESCRIPTIVE *********  
*******************************
*******************************

clear all 

cd "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"

********************************************
********* SAMPLE DESCRIPTIVE STATS *********
********************************************


***** TEPA ***** 

use Data/Clean/df_tepa, clear 

	preserve 
	
		collapse (mean) age sexe annees_etudes hhc hplus stplc am2nb salmee [pweight = new_weight], by (indiv_num)
		
		estpost tabstat age sexe annees_etudes hhc hplus stplc am2nb salmee, stat(mean sd max min n) 
		estimate store summary 
		esttab summary using Output/Tables/summary_stats.tex, replace ///
		cells("age(fmt(%9.2fc)) sexe(fmt(%9.2fc)) annees_etudes(fmt(%9.2fc)) hhc(fmt(%9.2fc)) hplus(fmt(%9.2fc)) stplc(fmt(%9.2fc)) am2nb(fmt(%9.2fc)) salmee(fmt(%9.2fc))") unstack ///
		title("Summary Statistics: TEPA Sample") ///
		addnotes("Note: test") ///
		label nonumb noobs
		
	restore 
	
	gen wedge = hhc - hplus 
	
	order hhc hplus
	gen adummy = (hhc > hplus)
	order adummy
	sort adummy
	order wedge 
	
	tab wedge
	hist wedge 
	
	su wedge 
	
	count if wedge == .
	
	bysort annee: count 
	
	
	
	bysort annee: su hplus salmee wedge 


***** ABROG ***** 

use Data/Clean/df_abrog, clear 

***** MACRON ***** 





*********************************************
********* FIGURES: 2003-2020 TRENDS *********  
*********************************************

use Data/Clean/df4_master_final.dta, clear


		
***** Paid OT Hours in the Reference Week (emphre) ***** 

	preserve 
	
	/*foreach var in emphre valprie salmee{
		
		qui su `var', detail
		local q1 = r(p25)  
		local q3 = r(p75)  
		
		local iqr = `q3' - `q1'
		
		local lower = `q1' - 3 * `iqr' 		// I use 3 * the IQR here to be less strict - compensation data usually has a long right tail 
		local upper = `q3' + 3 * `iqr'
		
		drop if (`var' < `lower' | `var' > `upper')
	}*/
	
		keep if hplus !=.
		collapse (mean) hplus [pweight = new_weight], by(indiv_num datdeb_q)
		collapse (mean) hplus, by(datdeb_q)
		
		twoway (bar hplus datdeb_q, barwidth(0.8)), ///
				xtitle("") ytitle("Desired Weekly Hours", size(small)) ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
				ylabel(, grid labsize(small))

		//graph export "Output/1.retro.paid_OT_overall.png", as(png) replace
	
	restore 
	
	*** By worker type ***
	
	preserve
	
		keep if (stplc == 1 | stplc == 2)
		
		collapse (mean) emphre [pweight=extri], by(indiv_num datdeb_q stplc)           
		collapse (mean) emphre, by(datdeb_q stplc) 

		twoway (bar emphre datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
			   (bar emphre datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
			    by(stplc, title("") note("") legend(off)) ///
				xtitle("") ytitle("Paid Overtime Hours", size(small)) ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
				ylabel(0(1) 7, axis(1) labsize(small)) ///
				ylabel(0(1) 7, axis(2) labsize(small))

		graph export "Output/1.2.paid_OT(bystplc).png", as(png) replace
	
	restore 
	
	*** Across the wage distribution *** 
	
	preserve 
	
		collapse (mean) emphre [pweight = new_weight], by(indiv_num datdeb_q salredtr)
		collapse (mean) emphre, by(salredtr)
		
		graph bar emphre, over(salredtr)
		
		

	
***** Total Hours Worked ***** 

	preserve 

		collapse (mean) emphrc [pweight=new_weight], by(indiv_num datdeb_q)
		collapse (mean) emphrc, by(datdeb_q)
		
		sort datdeb_q
		
		twoway (bar emphrc  datdeb_q), ///
				xtitle("") ///
				ytitle("Total Weekly Working Hours", size(small)) ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
				ylabel(39(0.25) 40.5, labsize(small))
		
		graph export "Output/2.total_hours_worked.png", as(png) replace
		
	restore
	
	*** By Worker Type *** 
	
	preserve 

		collapse (mean) hhc [pweight=new_weight], by(indiv_num datdeb_q stplc)
		collapse (mean) hhc, by (datdeb_q stplc)
		
		sort datdeb_q
		
		keep if (stplc == 1 | stplc == 2)
		
		twoway (bar hhc datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
			   (bar hhc datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
				by(stplc, title("") note("") legend(off)) ///
				xtitle("") ytitle("Total Weekly Working Hours", size(small)) ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
				ylabel(37.5(0.25) 40.5, axis(1) labsize(small)) ///
				ylabel(37.5(0.25) 40.5, axis(2) labsize(small))

		graph export "Output/2.1.total_hours_worked_bystplc.png", as(png) replace 
		
	restore 

	preserve 
		
		collapse (mean) hhc [pweight=new_weight], by(indiv_num datdeb_q stplc)
		collapse (mean) hhc, by (datdeb_q stplc)
		
		sort datdeb_q
		
		keep if (stplc == 1 | stplc == 2)
		
		twoway (bar hhc datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
			   (bar hhc datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
				by(stplc, title("") note("") legend(off)) ///
				xtitle("") ytitle("Total Weekly Working Hours", size(small)) ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
				ylabel(37.5(0.25) 38.5, axis(1) labsize(small)) ///
				ylabel(39.5(0.25) 40.5, axis(2) labsize(small))

		graph export "Output/2.2.total_hours_worked_bystplc.png", as(png) replace 
	
	restore 

	
***** Motivation to Work More/Less Hours (stplc/stmn) ***** 

	preserve
		
		
***** Moonlighters ***** 

	preserve
		
		replace am2nb = 1 if (am2nb == 2 | am2nb == 3) 
		replace am2nb = 0 if am2nb == . 
		
		collapse (count) am2nb [pweight=new_weight], by(indiv_num datdeb_q stplc)
		collapse (percent) am2nb, by (datdeb_q stplc)
		
		twoway (bar am2nb datdeb_q, barwidth(0.8)), ///
			    xtitle("") ytitle("Percent of Moonlighters") ///
				xlabel(#20, format(%tq) angle(45) labsize(small)) ///
				ylabel(, labsize(small))
				
	restore 
	
	*** By Worker Type *** 
	
	preserve
		
		keep if (stplc == 1 | stplc == 2)
		
		replace am2nb = 1 if (am2nb == 2 | am2nb == 3) 
		replace am2nb = 0 if am2nb == . 
		
		collapse (count) am2nb [pweight=new_weight], by(indiv_num datdeb_q stplc)
		collapse (percent) am2nb, by (datdeb_q stplc)
		
		twoway (bar am2nb datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
			   (bar am2nb datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
				by(stplc, title("") note("") legend(off)) ///
				xtitle("") ytitle("Percent of Moonlighters", size(small)) ///
				xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
				ylabel(, axis(1) labsize(small)) ///
				ylabel(, axis(2) labsize(small))
		
		//graph export "", as(png) replace 
	
	restore 
