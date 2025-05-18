***** Generating XGBoost Data *****

use Data/Clean/SYNTH_2, clear 
	* I need a unique ID for every individual date pair 
	gen str_indiv = string(indiv_num)
	gen ID = str_indiv + string(datdeb)
	destring ID, replace
save "Data/Clean/Add_guims_back_here.dta", replace 
	
	* What I'll use to train the model
	keep indiv_num hplus datdeb ID wsalred_lowess wsalred_CMA wsalred_LMA sexe married naf10 empnbh ancentr nivet urban age etranger dimanc samedc horaic maisoc pnai28 zus dep pnai28
save "Data/Clean/df_XGB.dta", replace 

import delimited "Data/Clean/df_hat_guims.csv", clear
	rename id ID
	rename hplus hplus_hat
	keep ID hplus 
save "Data/Clean/df_hat_guims.dta", replace 

use Data/Clean/Add_guims_back_here.dta, clear
	merge 1:1 ID using "Data/Clean/df_hat_guims.dta"
save "Data/Clean/SYNTH_2_hat.dta", replace

use Data/Clean/SYNTH_2_hat.dta, clear
preserve 
	sort datdeb
	collapse (mean) hplus hplus_hat, by(datdeb_q)
	twoway(line hplus datdeb_q)(line hplus_hat datdeb_q), xtitle("") ytitle("Desired Hours (Weekly)") legend(position(11) ring(0) label(1 "True Data") label(2 "XGBoost Estimates") size(small)) title("Avg. Quarterly Values for True and XGBoost Imputed Desired Hours", size(medium))
	export graph "Output/Figures/Synthetic/Qhplus_hat.png"
restore 

preserve 
	sort datdeb
	collapse (mean) hplus hplus_hat, by(datdeb_m)
	twoway(line hplus datdeb_m)(line hplus_hat datdeb_m), xtitle("") ytitle("Desired Hours (Weekly)") legend(position(11) ring(0) label(1 "True Data") label(2 "XGBoost Estimates") size(small)) title("Avg. Monthly Values for True and XGBoost Imputed Desired Hours", size(medium))
	export graph "Output/Figures/Synthetic/Mhplus_hat.png"
restore 
