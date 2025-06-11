******************************
******************************
********* MERGE PREP ********* 
******************************
******************************

******************
**# SMIC NET *****
******************

	* No data on NET SMIC (without CSG CSDS) before July 2005, the data has been taken from INSEE. 
	* I impute the NET value before 2005 by using the NET/ RAW ratio from 2005 onwards.

	*NET
import delimited "Data/SMIC/SMIC_net.csv", clear 
	drop v3
	drop if _n < 5
	split v1, parse("-")
	drop v1
	rename v2 smic_m_net 
	rename v11 annee 
	rename v12 mois
	destring *, replace
	drop if _n > 186 
save "Data/SMIC/smic_net.dta", replace // Only starts July 2005!!!

	* BRUT 
import excel "Data/SMIC/SMIC_brut.xlsx", clear
	split C, parse("/")
	rename C1 mois 
	rename C3 annee 
	rename D smic_h_brut
	rename E smic_m_brut_151
	rename F smic_m_brut_169
	drop A B C G H C2
	drop if _n < 4
	foreach var in smic_h_brut smic_m_brut_151 smic_m_brut_169 {
		replace `var' = "" if `var' == "///"
	}
	destring *, replace
	drop if annee < 2003 | annee > 2020 
	replace smic_m_brut_151 = smic_m_brut_169 * 151.67 / 169 if smic_m_brut_151 == .	
	drop smic_m_brut_169
save "Data/SMIC/smic_brut.dta", replace 

use Data/SMIC/smic_net, clear
	sort annee mois	
	merge 1:1 annee mois using "Data/SMIC/smic_brut.dta"
	drop _merge 
	foreach var in smic_m_brut_151 smic_h_brut {
		replace `var' = `var'[_n-1] if `var' == . 
	}
	order annee mois
	
	* Net / Raw ratio
	gen ratio = smic_m_net / smic_m_brut_151 
	su ratio // mean ~ 0.7844.
	* Hence, for smic_brut values before 2005, I assume smic_net = smic_brut * 0.7844. 
	
	gsort -annee mois
	set obs `=_N + 28'
	replace annee = 2003 if _n > 188 & _n< 200
	replace annee = 2004 if _n > 199 & _n < 211
	replace annee = 2005 if _n > 210 & _n < 217
	
	sort annee mois
	foreach var in smic_m_brut_151 smic_h_brut {
		replace `var' = `var'[_n-1] if `var' == . 
	}
	
	replace mois = _n - 30 if inrange(_n, 31, 36)
	local i = 1
	forvalues obs = 14/24 {
		if `i' == 7 {
			local i = 8
    }
    replace mois = `i' in `obs'
    local ++i
}
	local i = 1
	forvalues obs = 2/12 {
		if `i' == 7 {
			local i = 8
    }
    replace mois = `i' in `obs'
    local ++i
}	
sort annee mois
	
	replace smic_m_net = 957.74 if smic_m_net == . & annee == 2005
	replace smic_m_net = smic_m_brut_151 * 0.7844 if smic_m_net ==.
	
	rename smic_m_brut_151 smic_m_brut 
	drop ratio
	lab var smic_h_brut "SMIC Horaire Brut"
	lab var smic_m_brut "SMIC Mensuel Brut"
	lab var smic_m_net "SMIC Mensuel Net"
save "Data/SMIC/smic_FR.dta", replace

*****************************************************
**# BORDER COUNTRY QUARTERLY ECONOMIC SITUATION *****
*****************************************************

/*"quarterly economic situation measured either by a business climate or by the share
of exports of goods and services in the GDP of the country c at date t (indicators
of the OECD)".*/

import delimited "Data/Border_Country_Controls/OECD_Composite_Business_Confidence.csv", clear 
	keep ref_area time_period obs_value 
	rename obs_value CBC
	rename ref_area border_country
	gen datdeb_m = monthly(time_period, "YM")
	drop time_period
	format %tm datdeb_m
	order border_country datdeb_m
	sort border_country datdeb_m
save "Data/Border_Country_Controls/OECD_CBC_Clean.dta", replace

import delimited "Data/Border_Country_Controls/OECD_g_and_s_trade_by_gdp.csv", clear
	keep location time_period obs_value 
	rename location border_country 
	rename obs_value gs_gdp 
	gen datdeb_q = quarterly(time_period, "YQ")
	format %tq datdeb_q
	drop time_period
	order border_country datdeb_q
save "Data/Border_Country_Controls/OECD_GS_GDP_Clean.dta", replace

**************************************
**# Making the ueq_localise data ***** 
**************************************

import delimited "Data/Unemployment/ueq_localise.csv", clear 

	split libellé, parse(" - ")
	drop if libellé == "Codes"
	drop t1-t4 v9-v88 v161-v176
	
	local year = 2003
	local quarter = 1

	forvalues i = 89/160 {
		local newname = "y`year'q`quarter'"
		rename v`i' `newname'

		* Increment quarter and roll over year
		local quarter = `quarter' + 1
		if `quarter' > 4 {
			local quarter = 1
			local year = `year' + 1
		}
	}
	drop idbank dernièremiseàjour période libellé libellé1 
	order libellé2 
	drop if (_n <16 | _n > 111)

	* Creating a new variable to store the department codes
gen dep = ""

	* Codes based on libellé2
replace dep = "12" if libellé2 == "Aveyron"
replace dep = "01" if libellé2 == "Ain"
replace dep = "02" if libellé2 == "Aisne"
replace dep = "03" if libellé2 == "Allier"
replace dep = "04" if libellé2 == "Alpes-de-Haute-Provence"
replace dep = "05" if libellé2 == "Hautes-Alpes"
replace dep = "06" if libellé2 == "Alpes-Maritimes"
replace dep = "07" if libellé2 == "Ardèche"
replace dep = "08" if libellé2 == "Ardennes"
replace dep = "09" if libellé2 == "Ariège"
replace dep = "10" if libellé2 == "Aube"
replace dep = "11" if libellé2 == "Aude"
replace dep = "13" if libellé2 == "Bouches-du-Rhône"
replace dep = "14" if libellé2 == "Calvados"
replace dep = "15" if libellé2 == "Cantal"
replace dep = "16" if libellé2 == "Charente"
replace dep = "17" if libellé2 == "Charente-Maritime"
replace dep = "18" if libellé2 == "Cher"
replace dep = "19" if libellé2 == "Corrèze"
replace dep = "2.1" if libellé2 == "Corse-du-Sud"
replace dep = "2.2" if libellé2 == "Haute-Corse"
replace dep = "21" if libellé2 == "Côte-d'Or"
replace dep = "22" if libellé2 == "Côtes-d'Armor"
replace dep = "23" if libellé2 == "Creuse"
replace dep = "24" if libellé2 == "Dordogne"
replace dep = "25" if libellé2 == "Doubs"
replace dep = "26" if libellé2 == "Drôme"
replace dep = "27" if libellé2 == "Eure"
replace dep = "28" if libellé2 == "Eure-et-Loir"
replace dep = "29" if libellé2 == "Finistère"
replace dep = "30" if libellé2 == "Gard"
replace dep = "31" if libellé2 == "Haute-Garonne"
replace dep = "32" if libellé2 == "Gers"
replace dep = "33" if libellé2 == "Gironde"
replace dep = "34" if libellé2 == "Hérault"
replace dep = "35" if libellé2 == "Ille-et-Vilaine"
replace dep = "36" if libellé2 == "Indre"
replace dep = "37" if libellé2 == "Indre-et-Loire"
replace dep = "38" if libellé2 == "Isère"
replace dep = "39" if libellé2 == "Jura"
replace dep = "40" if libellé2 == "Landes"
replace dep = "41" if libellé2 == "Loir-et-Cher"
replace dep = "42" if libellé2 == "Loire"
replace dep = "43" if libellé2 == "Haute-Loire"
replace dep = "44" if libellé2 == "Loire-Atlantique"
replace dep = "45" if libellé2 == "Loiret"
replace dep = "46" if libellé2 == "Lot"
replace dep = "47" if libellé2 == "Lot-et-Garonne"
replace dep = "48" if libellé2 == "Lozère"
replace dep = "49" if libellé2 == "Maine-et-Loire"
replace dep = "50" if libellé2 == "Manche"
replace dep = "51" if libellé2 == "Marne"
replace dep = "52" if libellé2 == "Haute-Marne"
replace dep = "53" if libellé2 == "Mayenne"
replace dep = "54" if libellé2 == "Meurthe-et-Moselle"
replace dep = "55" if libellé2 == "Meuse"
replace dep = "56" if libellé2 == "Morbihan"
replace dep = "57" if libellé2 == "Moselle"
replace dep = "58" if libellé2 == "Nièvre"
replace dep = "59" if libellé2 == "Nord"
replace dep = "60" if libellé2 == "Oise"
replace dep = "61" if libellé2 == "Orne"
replace dep = "62" if libellé2 == "Pas-de-Calais"
replace dep = "63" if libellé2 == "Puy-de-Dôme"
replace dep = "64" if libellé2 == "Pyrénées-Atlantiques"
replace dep = "65" if libellé2 == "Hautes-Pyrénées"
replace dep = "66" if libellé2 == "Pyrénées-Orientales"
replace dep = "67" if libellé2 == "Bas-Rhin"
replace dep = "68" if libellé2 == "Haut-Rhin"
replace dep = "69" if libellé2 == "Rhône"
replace dep = "70" if libellé2 == "Haute-Saône"
replace dep = "71" if libellé2 == "Saône-et-Loire"
replace dep = "72" if libellé2 == "Sarthe"
replace dep = "73" if libellé2 == "Savoie"
replace dep = "74" if libellé2 == "Haute-Savoie"
replace dep = "75" if libellé2 == "Paris"
replace dep = "76" if libellé2 == "Seine-Maritime"
replace dep = "77" if libellé2 == "Seine-et-Marne"
replace dep = "78" if libellé2 == "Yvelines"
replace dep = "79" if libellé2 == "Deux-Sèvres"
replace dep = "80" if libellé2 == "Somme"
replace dep = "81" if libellé2 == "Tarn"
replace dep = "82" if libellé2 == "Tarn-et-Garonne"
replace dep = "83" if libellé2 == "Var"
replace dep = "84" if libellé2 == "Vaucluse"
replace dep = "85" if libellé2 == "Vendée"
replace dep = "86" if libellé2 == "Vienne"
replace dep = "87" if libellé2 == "Haute-Vienne"
replace dep = "88" if libellé2 == "Vosges"
replace dep = "89" if libellé2 == "Yonne"
replace dep = "90" if libellé2 == "Territoire de Belfort"
replace dep = "91" if libellé2 == "Essonne"
replace dep = "92" if libellé2 == "Hauts-de-Seine"
replace dep = "93" if libellé2 == "Seine-Saint-Denis"
replace dep = "94" if libellé2 == "Val-de-Marne"
replace dep = "95" if libellé2 == "Val-d'Oise"
	drop libellé2

	reshape long y, i(dep) j(time, string)
	rename y ue_q_dep
	gen annee = real(substr(time, 1, 4))
	gen trim = real(substr(time, 6, 1))
	destring *, replace 
	drop time

save "Data/Unemployment/ueq_localise.dta", replace
		

****************************
**# Making the IV data ***** 
****************************

*** Annual GVA by Region *** (Didn't really work)

import delimited "Data/GVA/annual_values.csv", clear 
	split label, parse(" - ")
	drop if (label == "Codes" | _n <=12 | label2 == "Agriculture" | label2 == "Non-market tertiary sector" | inlist(label4, "Metropolitan France excluding Île-de-France", "Metropolitan France", "Guadeloupe", "Martinique","French Guiana", "La Réunion", "Mayotte", "All the French overseas departments, including Mayotte", "France"))
	drop v5-v17 v36-v37 period idbank lastupdate label label1 label3
	order label4 label2 
	
	local year = 2003
	forvalues i = 18/35 {
		rename v`i' y`year'
		local year = `year' + 1
}
	destring *, replace 
	replace label2 = "ET" if label2 == "Industry"
	replace label2 = "EU" if label2 == "Construction"
	replace label2 = "EV" if label2 == "Market tertiary sector"
	rename label2 naf4
	drop if _n > 39
	
	reshape long y, i(label4 naf4) j(annee)
	rename y GVA
	rename label4 reg2016	
save "Data/GVA/annual_values.dta", replace  

*** Trying Quarterly Value added by sector ***
import delimited "Data/Quarterly_Branch_Account/quarterly_values.csv", clear 
	split label, parse(" - ")
	drop if (label == "Codes" | _n > 192 | label3 =="Volumes chained at previous year prices")
	drop idbank lastupdate period label5 label t1-t4 v9-v220 label4 label3
	rename label2 newnaf
	destring *, replace 
	
	local quarter = tq(2002q1) + (221 - 217)
	forvalues i = 221/281 {
		local qlab : display %tq `quarter'
		rename v`i' t`qlab'
		local quarter = `quarter' + 1
}
	reshape long t, i(label1 newnaf) j(tq, string)
	rename t value
	replace label1 = "GVA" if label1 == "Value added of branches"
	replace label1 = "production" if label1 == "Branch production"
	gen datdeb_q = quarterly(tq, "YQ")
	format datdeb_q %tq
	drop tq
save "Data/Quarterly_Branch_Account/quarterly_values.dta", replace

	keep if _n < 1465
	rename value production 
	replace production = production * 6 	// unit multiplier from "characteristics sheet of OG INSEE dataset"
	replace production = production / (94.72/100) // making base 2015. summarize cpi if annee == 2010 --> 94.72
	drop label1
save "Data/Quarterly_Branch_Account/production_q.dta", replace

use Data/Quarterly_Branch_Account/quarterly_values, clear 
	keep if _n > 1464
	rename value GVA 
	replace GVA = GVA * 6 			// unit multiplier from "characteristics sheet of OG INSEE dataset"
	replace GVA = GVA / (94.72/100) 		// making base 2015. summarize cpi if annee == 2010 --> 94.72
	drop label1
save "Data/Quarterly_Branch_Account/GVA_q.dta", replace
