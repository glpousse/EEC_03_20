*************************************************
*************************************************
********* DATA CLEANING & SAMPLE CREATION *******
*************************************************
*************************************************

cd "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"

/* 
	Potentially usually vars?
		- chpub: Nature de l'employeur de la profession principale
		- typmen5: Type de ménage (5 postes) 
		- dep: department ou habite indiv
		- deparc: departement etablissement 
*/

**********************************
***** Making the master data *****
**********************************

forvalues i = 2003/2010{
	
	use Data/Raw/qc`i'.dta, clear 

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
	
	* CPI merge 
		cap destring annee mois trim, replace
		sort annee mois trim
		merge m:1 annee mois using "Data/Inflation/CPI_base_2015.dta"
		drop if _merge ==2
		drop _merge 
		drop if annee != `i'

	* Stringing everything to make append possible
		quietly tostring *, replace 
		
	save Data/Clean/df_`i'.dta, replace
	}

	* Appending all years (building EEC_03_20)
	clear all
	forvalues i = 2003/2010{
		
		display "appending year `i'"
		append using Data/Clean/df_`i'.dta
	}
	quietly destring * , replace
	
	* Removing variables with no observations 
	foreach var of varlist _all {	
		cap confirm numeric variable `var'
		
		if _rc == 0 {
			qui su `var'
			if r(N) == 0 drop `var'
		}
	}
	
	* Formatting the date and a numeric unique ID
	format datdeb %td 
	gen datdeb_m = ym(year(datdeb), month(datdeb))
	gen datdeb_q = yq(annee, trim)
	format datdeb_m %tm 
	format datdeb_q %tq
	egen indiv_num = group(indiv)
	
	* Adding years of study (from CaCaJole)
	gen annees_etudes = fordat - naia - 6
	replace annees_etudes=. if annees_etudes<0 | annees_etudes>30
	
save Data/Clean/df1_master_notrim.dta, replace  

************************************
***** Trimming the master data ***** 
************************************

use Data/Clean/df1_master_notrim, clear
		 
***** Trimming observations ***** 

	* Metropolitan France 
	drop if (deparc == "97" | deparc == "9A" | deparc == "9C" | deparc == "9E" | deparc == "9D") 
	
	* Active Population 
	keep if acteu == 1					// actif au sens du BIT
	drop if (retrai == 1 | retrai ==2)	// retraité et pré-retraité
	drop if temp !=. 					// Occupation actuelle d'un emploi, pour les plus de 75 ans 
	drop 	temp 
	
	* Drop Agriculture
	drop if nafg4 == "ES" 				// Up until 2008Q4
	drop if nafg4n == "ES"				// Starts 2008Q1
	drop if nafg16 == "EB"				// Industries Agricoles 
	
	* Drop Non-Profit 
	drop if nafg16 == "EQ"				// Up until 2008Q4 -- Education, sante, action sociale 
	drop if nafg16 == "ER"				// Up until 2008Q4 -- Administrations 
	drop if nafg10n == "OQ"				// From 2008Q1 -- Administration publique, enseignement, santé humaine et action sociale -- 
	drop if chpub 	== 1				// Etat
	drop if chpub 	== 3				// Hopitaux publics
	drop if chpub 	== 2				// Collectivités locales, HLM 
	 
	* Harmonizing NAFG4 and NAFG4N
	gen str2 naf4 = ""
	replace naf4 = nafg4 if annee < 2009 
	replace naf4 = nafg4n if annee >2008
	lab var naf4 "Activité économique de l'établissement de l'emploi principal actuel (NAF en 4 postes)"
	
	* Harmonizing NAFG16 and NAFG10N
	gen str2 naf10= "."
	
	foreach val in 00 AZ BE FZ GI JZ KZ LZ MN OQ RU {
		replace naf10 = "`val'" if nafg10n == "`val'" & annee > 2007
	}
	replace naf10 = "00" if nafg16 == "00" & annee < 2008
	replace naf10 = "AZ" if inlist(nafg16, "EA") & annee < 2008
	replace naf10 = "BE" if inlist(nafg16, "EC", "ED", "EE", "EF", "EG", "EB") & annee < 2008
	replace naf10 = "FZ" if nafg16 == "EH" & annee < 2008
	replace naf10 = "GI" if inlist(nafg16, "EJ", "EK") & annee < 2008
	replace naf10 = "KZ" if nafg16 == "EL" & annee < 2008
	replace naf10 = "LZ" if nafg16 == "EM" & annee < 2008
	replace naf10 = "MN" if nafg16 == "EN" & annee < 2008
	replace naf10 = "OQ" if inlist(nafg16, "EQ", "ER") & annee < 2008
	replace naf10 = "RU" if nafg16 == "EP" & annee < 2008	
	replace naf10 = "00" if naf10 == "."
	
/*	Before 2008 -- NAFG16:
		00 - Non renseigné
		EA - Agriculture, sylviculture et pêche
		EB - Industries agricoles
		EC - Industries des biens de consommation
		ED - Industrie automobile
		EE - Industries des biens d'équipement
		EF - Industries des biens intermédiaires
		EG - Energie
		EH - Construction
		EJ - Commerce et réparations
		EK - Transports
		EL - Activités financières
		EM - Activités immobilières
		EN - Services aux entreprises
		EP - Services aux particuliers
		EQ - Education, santé, action sociale
		ER - Administrations 

	After 2008 -- NAFG10N: 
		00 - Non renseigné
		AZ - Agriculture, sylviculture et pêche
		BE - Industrie manufacturière, industries extractives et autres
		FZ - Construction
		GI - Commerce de gros et de détail, transports, hébergement et restauration
		JZ - Information et communication
		KZ - Activités financières et d'assurance
		LZ - Activités immobilières
		MN - Activités spécialisées, scientifiques et techniques et activités de services administratifs et de
		soutien
		OQ - Administration publique, enseignement, santé humaine et action sociale
		RU - Autres activités de services */
	
	* Already restricted to non-agricultural for profit sector
	
	* Sample Dummies 
	gen du_hhc			= (hhc <= 70 & hhc>=35)						// Weekly H on average
	gen du_empnbh 		= (empnbh <= 70 & empnbh>=35)				// H during reference week 
	gen du_nbhp 		= (nbhp <= 70 & nbhp>=35)					// H agreed on 
 	gen min_smic		= (salred >= smic_m_net)					// Makes minimum wage (NET)
	gen no_interruption = (empaff == 4 | empaff ==5 ) 				// uninterrupted workschedule 
	gen mod_agree 		= (redic == 1) 								// modulation agreemnt
	gen CDI		 		= (contra == 1) 							// holds a CDI contract
	gen CDD 			= (contra ==2 )								// holds a CDD contract
	gen full_time 		= (tppred == 1)		 						// full-time workers 
	gen part_time 		= (tppred ==2)								// part-time workers
	
	// SOmething with respect to CHPUB? 
	/*1 - Etat
2 - Collectivités locales, HLM
3 - Hôpitaux publics
4 - Particulier
5 - Entreprise publique (La Poste, EDF-GDF, etc.)
6 - Entreprise privée, association
 - Ensemble */
	
save Data/Clean/df2_master_trimmed.dta, replace  

*****************************************
***** Harmonizing/ Adding Variables ***** 
*****************************************

use Data/Clean/df2_master_trimmed, clear 
	
	* OECD AGE GROUP
	gen age_group 			= . 
	replace age_group 		= 1 if (age <= 24 & age >=18)  	// OECD: just entering the working pop
	replace age_group 		= 2 if (age >= 25 & age <= 54)	// OECD: Prime working age 
	replace age_group 		= 3 if (age >=55 & age <= 64) 	// OECD: Past peak approaching retirement
	label define age_lbl 1 "18 - 35" 2 "25 - 54" 3 "55 - 64"
	label values age_group age_lbl
	drop if age_group == .
	
	* CacaJole Age Groups
	gen age_g = .
	replace age_g = 1 if age >= 18 & age <30 
	replace age_g = 2 if age >29 & age <40
	replace age_g = 3 if age >39 & age <50
	replace age_g = 4 if age >49 & age <60
	replace age_g = 5 if age >59 
	label define lbl_age_g 1 "18-29" 2 "30-39" 3 "40-49" 4 "50-59" 5 "60+" 
	label values age_g lbl_age_g
	
	* My Age Groups 
	gen age_cohort = .
	replace age_cohort = 1 if age >= 18 & age <= 24
	replace age_cohort = 2 if age >= 25 & age <= 34
	replace age_cohort = 3 if age >= 35 & age <= 44
	replace age_cohort = 4 if age >= 45 & age <= 54
	replace age_cohort = 5 if age >= 55 & age <= 64
	replace age_cohort = 6 if age >= 65
	label var age_cohort "Grouped Age Cohort"
	label define age_cohort_lbl 1 "18–24" 2 "25–34" 3 "35–44" 4 "45–54" 5 "55–64" 6 "65+"
	label values age_cohort age_cohort_lbl
	
	* EDUCATION DEGREE 
	gen educ_degree 		= . 
	replace educ_degree 	= 1 if nivp >=41 
	replace educ_degree 	= 2 if (nivp == 30 | nivp == 40)
	replace educ_degree 	= 3 if nivp <= 20 
	label define educ_lbl 1 "No Tertiary Education" 2 "Vocational Training" 3 "Higher Education"
	label values educ_degree educ_lbl
	
	* ETRANGER  
	gen etranger = .
	forvalues i = 1/3{
		replace etranger = `i' if nfr == `i'
	}
	lab var etranger "Nationalité française ou étrangère, redressée"
	
	* CAT: JOB TENURE 
	gen cat_tenure = . 
	replace cat_tenure = 1 if ancentr < 5
	replace cat_tenure = 2 if ancentr < 11 & ancentr > 4
	replace cat_tenure = 3 if ancentr < 21  & ancentr > 10
	replace cat_tenure = 4 if ancentr < 31 & ancentr > 20
	replace cat_tenure = 5 if ancentr > 30
	lab def tenure_lbl 1 "0-4" 2 "5-10" 3 "11-20" 4 "21-30" 5 "30+" 
	lab values cat_tenure tenure_lbl 
	
	* CAT: # OF EMPLOYEES (SMALLER CAT)
	gen nb_salarie = . 
	replace nb_salarie = 1 if nbsala < 5
	replace nb_salarie = 2 if nbsala >=5 & nbsala < 7 
	replace nb_salarie = 3 if nbsala ==7 
	replace nb_salarie = 4 if nbsala > 7  
	lab def newlab 1 "0-19" 2 "20-199" 3 "200-499" 4 "500+"
	lab values nb_salarie newlab 
	
	* CAT: WORK HOURS
	foreach var in empnbh hhc nbhp {
	gen `var'_cat 	= .
	replace `var'_cat = 1  if `var' <  20 
	replace `var'_cat = 2  if `var' >= 20 & `var' < 25
	replace `var'_cat = 3  if `var' >= 25 & `var' < 30
	replace `var'_cat = 4  if `var' >= 30 & `var' < 35
	replace `var'_cat = 5  if `var' >= 35 & `var' < 40
	replace `var'_cat = 6  if `var' >= 40 & `var' < 45
	replace `var'_cat = 7  if `var' >= 45 & `var' < 50
	replace `var'_cat = 8  if `var' >= 50 & `var' < 55
	replace `var'_cat = 9  if `var' >= 55 & `var' < 60
	replace `var'_cat = 10 if `var' >= 60 & `var' < 65
	replace `var'_cat = 11 if `var' >= 65 
	label define `var'_labels 	1 "-20" 2 "20–24" 3 "25–29" 4  "30–34" 5  "35–39" 6  "40–44" ///
								7  "45–49"  8  "50–54" 9   "55–59" 10  "60–64" 11  "65+" 
	label values `var'_cat `var'_labels
	}
   
	* CAT: NET INCOME BRACKET (made from salred)
	gen cat_salred = .
	replace cat_salred = 1  if salred < 500
	replace cat_salred = 2  if salred >= 500  & salred < 1000
	replace cat_salred = 3  if salred >= 1000 & salred < 1250
	replace cat_salred = 4  if salred >= 1250 & salred < 1500
	replace cat_salred = 5  if salred >= 1500 & salred < 2000
	replace cat_salred = 6  if salred >= 2000 & salred < 2500
	replace cat_salred = 7  if salred >= 2500 & salred < 3000
	replace cat_salred = 8  if salred >= 3000 & salred < 5000 
	replace cat_salred = 9  if salred >= 5000 & salred < 8000
	replace cat_salred = 10 if salred >= 8000
	replace cat_salred = 98 if salred == 98
	replace cat_salred = 99 if salred == 99
	label define cat_salred_labels 	1 "500-" 2 "500-1000" 3 "1000-1250" 4 "1250-1500" 5 "1500-2000" 6 "2000-2500" ///
								7 "2500-3000" 8 "3000-5000" 9 "5000-8000" 10 "8000+" 98 "Refus" 99 "Ne sait pas"
	label values cat_salred cat_salred_labels
	label variable cat_salred "Tranche de salaire de l'emploi principal"

	* LABELLING SALMET 
	rename salmet tempvar 
	gen salmet =. 
	replace salmet = 1 if (tempvar == "A")
	replace salmet = 2 if (tempvar == "B")
	replace salmet = 3 if (tempvar == "C")
	replace salmet = 4 if (tempvar == "D")
	replace salmet = 5 if (tempvar == "E")
	replace salmet = 6 if (tempvar == "F")
	replace salmet = 7 if (tempvar == "G")
	replace salmet = 8 if (tempvar == "H")
	replace salmet = 9 if (tempvar == "I")
	replace salmet = 10 if (tempvar == "J")
	replace salmet = 98 if (tempvar == "8") 
	replace salmet = 99 if (tempvar == "99")
	drop tempvar 
	lab var salmet "Tranche de salaire net mensuel de l'emploi principal"
	label define salmet_labels 1 "500-" 2 "500-1000" 3 "1000-1250" 4 "1250-1500" 5 "1500-2000" 6 "2000-2500" 7 "2500-3000" 8 "3000-5000" 9 "5000-8000" 10 "8000+" 98 "Refus" 99 "Ne sait pas"
	label values salmet salmet_labels

	* PETA switches to PAYREG in 2013
	* Had written this when using post 2013 data: keeping convention 
	* Use `france` variable to confirm employer is located in France
	gen payreg = . 
	replace payreg = 1 if peta == 22
	replace payreg = 2 if peta == 23
	replace payreg = 3 if peta == 28
	replace payreg = 4 if peta == 31
	replace payreg = 5 if peta == 21
	replace payreg = 6 if peta == 41
	replace payreg = 7 if peta == 25 
	lab var payreg "Pays frontalier de la France métropolitaine où se situe l'établissement employeur" 

	* Destringing 
	replace deparc = "2.1" if deparc == "2A" 
	replace deparc = "2.2" if deparc == "2B"
	replace dep = "2.1" if dep == "2A" 
	replace dep = "2.2" if dep == "2B"
	destring dep deparc, replace 
	
	* URBAN 
	gen urban = (tuu == 1)
	lab var urban "Situé en Zone Urbaine"
	
save Data/Clean/df3_master_harmonized, replace 

*****************
**# DUMMIES *****
*****************
	
	* Code also generates shock dummies for 2013, 2019 shocks
	* Left as a ressource to study other shocks in the future

	* Shock dates
	gen tepa_date 	= date("2007-10-01", "YMD")
	gen abrog_date 	= date("2012-08-01", "YMD")
	gen macron_date = date("2019-01-01", "YMD")
	format tepa_date macron_date abrog_date %td

	* Individuals in panel before and after the shock
	bysort indiv (datdeb): gen obs_count = _N
	foreach shock in tepa abrog macron {
		gen pre_`shock' 	= (datdeb < `shock'_date) 
		gen post_`shock' 	= (datdeb >= `shock'_date) 
		bysort indiv: gen temp_`shock' = (sum(pre_`shock') != obs_count & sum(post_`shock') != obs_count)
		egen in_`shock' = min(temp_`shock'), by(indiv)
		drop temp_`shock'
	}

	* Has worked OT before (after) shock 
	gen pre_tepa_OT 		= 1 if (emphsc == 1 & post_tepa == 0)
	replace pre_tepa_OT 	= 0 if pre_tepa_OT == . 
	gen post_tepa_OT 		= 1 if (emphsc == 1 & post_tepa == 1)
	replace post_tepa_OT 	= 0 if post_tepa_OT == . 
	
	gen pre_abrog_OT 		= 1 if (emphsc == 1 & post_abrog == 0)
	replace pre_abrog_OT 	= 0 if pre_abrog_OT == .
	gen post_abrog_OT 		= 1 if (emphsc == 1 & post_abrog == 1)
	replace post_abrog_OT 	= 0 if post_abrog_OT == . 
	
	gen pre_macron_OT 		= 1 if (emphsc == 1 & post_macron == 0)
	replace pre_macron_OT 	= 0 if pre_macron_OT == .
	gen post_macron_OT 		= 1 if (emphsc == 1 & post_macron == 1)
	replace post_macron_OT 	= 0 if post_macron_OT == . 

	* Domestic Border Workers (TREATED)
	gen dom_bel 	= 1 if 	(dep == 59 | dep == 02 | dep == 08 | dep == 55 | dep == 54) & france == 1
	replace dom_bel = 0 if 	dom_bel == . 
	gen dom_ger 	= 1 if 	(dep == 57 | dep == 67 | dep == 68) & france == 1
	replace dom_ger = 0 if 	dom_ger == .
	gen dom_uk 		= 1 if 	(dep == 75 | dep == 62) & france == 1
	replace dom_uk 	= 0 if 	dom_uk == .
	gen dom_sp 		= 1 if 	(dep == 64 | dep == 65 | dep == 31 | dep == 09 | dep == 66) & france == 1
	replace dom_sp 	= 0 if 	dom_sp == . 
	gen dom_it 		= 1 if 	(dep == 74 | dep == 73 | dep == 05 | dep == 04 | dep == 06) & france == 1
	replace dom_it 	= 0 if 	dom_it == . 
	gen dom_swz 	= 1 if 	(dep == 74 | dep == 01 | dep == 39 | dep == 25 | dep == 90 | dep == 68) & france == 1 
	replace dom_swz = 0 if 	dom_swz == . 
	gen dom_lux 	= 1 if 	(dep == 54 | dep == 57) & france == 1 
	replace dom_lux = 0 if 	dom_lux == .
	
	gen dom_border = (dom_bel == 1 | dom_ger == 1 | dom_uk == 1 | dom_sp == 1 | dom_it == 1 | dom_swz == 1 | dom_lux == 1)

	* Trans-border Workers (per neighbor country)
	gen trans_bel 		= 1 if 	(dep == 59 | dep == 02 | dep == 08 | dep == 55 | dep == 54) & payreg == 2
	replace trans_bel 	= 0 if 	trans_bel == . 
	gen trans_ger 		= 1 if 	(dep == 57 | dep == 67 | dep == 68) & payreg == 1
	replace trans_ger 	= 0 if 	trans_ger == . 
	gen trans_uk 		= 1 if 	(dep == 75 | dep == 62) & payreg == 3
	replace trans_uk 	= 0 if 	trans_uk == . 
	gen trans_sp 		= 1 if 	(dep == 64 | dep == 65 | dep == 31 | dep == 09 | dep == 66) & payreg == 4
	replace trans_sp 	= 0 if 	trans_sp == . 
	gen trans_it 		= 1 if 	(dep == 74 | dep == 73 | dep == 05 | dep == 04 | dep == 06) & payreg == 5
	replace trans_it	= 0 if 	trans_it == . 
	gen trans_swz 		= 1 if 	(dep == 74 | dep == 01 | dep == 39 | dep == 25 | dep == 90 | dep == 68) & payreg == 6
	replace trans_swz 	= 0 if 	trans_swz == . 
	gen trans_lux 		= 1 if 	(dep == 54 | dep == 57) & payreg == 7
	replace trans_lux 	= 0 if 	trans_lux == . 

	* Trans-border Workers (all neighbors)
	gen trans_border = (trans_bel == 1 | trans_ger == 1 | trans_uk == 1 | trans_sp == 1 | trans_it == 1 | trans_swz == 1 | trans_lux == 1)
	
	* OPTIMIZERS (CaCaJole)
	gen optim 		= . 
	replace optim  	= 1 if (cse == 34 |cse == 35 | cse==37 | cse==38 | cse==46 | cse==48) // manager 
	replace optim 	= 0 if (cse == 62 | cse == 63 | cse == 65 | cse == 67 | cse == 68)	  // laborer 
	
	* Generating country indicators to match the border country economic data 
	gen str border_country = "FRA" 
	replace border_country = "DEU" if trans_ger == 1
	replace border_country = "CHE" if trans_swz == 1	
	replace border_country = "LUX" if trans_lux == 1 
	replace border_country = "BEL" if trans_bel == 1
	replace border_country = "ITA" if trans_it == 1
	replace border_country = "ESP" if trans_sp == 1 
	replace border_country = "GBR" if trans_uk == 1 

save Data/Clean/df4_master_final.dta, replace 

*******************************
 **# Making the WEDGE data ***** 
*******************************

use Data/Clean/df4_master_final, clear
	
	* HPLUS ASSUMPTION 1 
	gen hplus1 = hplus
	sort indiv_num datdeb
	bysort indiv_num: replace hplus1 = hplus1[_n-1] if missing(hplus1) & !missing(hplus1[_n-1])
	bysort indiv_num: replace hplus1 = hplus1[_n+1] if missing(hplus1) & !missing(hplus1[_n+1])
	
	* SALRED ASSUMPTION 1
	bysort indiv_num (datdeb): egen salred1 = mean(salred)

	* WORKING VARS 
	gen wedge 					= hplus - empnbh	
	gen abs_wedge 		      	= abs(hplus - empnbh) 
	
	gen mismatch				= . 
	replace mismatch 			= 1 if wedge != 0 & wedge !=.
	replace mismatch 			= 0 if wedge == 0 & wedge !=. 
	
	gen tplus = .
 	replace tplus = 1 if wedge > 0
 	replace tplus = 0 if (wedge == 0 | wedge < 0)

	gen worker_type				= . 
	replace worker_type 		= 1 if (hplus - empnbh) > 0 
	replace worker_type 		= 2 if (hplus - empnbh) == 0 
	replace worker_type 		= 3 if (hplus - empnbh) < 0
	label define worker_type_lbl 1 "Underworked" 2 "No Wedge" 3 "Overworked"  
	label values worker_type worker_type_lbl 
	
	foreach var in salred salred1 wsalred_CMA_2015 wsalred_LMA_2015 wsalred_lowess {
		cap drop weekly_`var' hourly_`var' log_`var' log_hourly_`var' `var'_sq
		gen weekly_`var'     	= `var'/ 4.33
		gen hourly_`var' 	  	= weekly_`var' / empnbh 
		gen log_`var' 	      	= log(`var')
		gen log_hourly_`var'  	= log(hourly_`var')
		gen `var'_sq = `var'^2 
	}
	
	gen hourly_salmee 	  	= salmee/ (4.33*empnbh)
	gen log_hourly_salmee 	= log(hourly_salmee)
	
	gen log_empnbh 	      	= log(empnbh)
	gen empnbh_round 		= ceil(empnbh)
		
	* DEMOGRAPHIC VARS
	gen married           	= (mpr == 2) 
	gen not_married 		= (married == 0)
	gen age_sq 				= age^2 
	replace sexe 			= 0 if sexe == 2
	cap drop male
	gen male 				= (sexe == 1)
	gen female 				= (sexe == 0)
	gen child 				= em1 
	
	* Encoding cateogoricals
	encode naf10, gen(cat_naf10)
	encode nafg16, gen(cat_nafg16)
	
save Data/Clean/df_wedges.dta, replace

****************************
**# MERGING EXTRA DATA ***** 
****************************

use Data/Clean/df_wedges, clear
	
	* NATIONAL UE
	merge m:1 annee trim using "Data/Unemployment/ueq_FR.dta" 
	drop if _merge ==2
	drop _merge
	
	* LOCAL UE
	cap drop _merge
	sort annee trim dep
	merge m:1 annee trim dep using "Data/Unemployment/ueq_localise.dta" 
	drop if _merge ==2
	drop _merge 
save "Data/Clean/df_wedges.dta", replace

	* SMIC NET & BRUT  
	sort annee mois
	merge m:1 annee mois using "Data/SMIC/smic_FR.dta"
	drop if _merge ==2
	drop _merge
	lab var smic_h_brut "SMIC Horaire Brut"
	lab var smic_m_brut "SMIC Mensuel Brut"
	lab var smic_m_net "SMIC Mensuel Net"
save "Data/Clean/df_wedges.dta", replace

	* BORDER COUNTRY QUARTERLY ECONOMIC SITUATION
	order border_country datdeb_m 
	sort border_country datdeb_m
	cap drop _merge
	merge m:1 border_country datdeb_m using "Data/Border_Country_Controls/OECD_CBC_Clean.dta"
	drop if _merge == 2
	drop _merge
	sort border_country datdeb_q
	merge m:1 border_country datdeb_q using "Data/Border_Country_Controls/OECD_GS_GDP_Clean.dta"
	drop if _merge == 2 
	drop _merge 
save "Data/Clean/df_wedges.dta", replace

******************************************
**# FINAL DUMMIES FOR IDENTIFICATION ***** 
******************************************



****************************************
**# INFLATOR/ DEFLATOR - BASE 2015 ***** POTENSH SLAP AT END OF SYNTH VARS
****************************************

use Data/Clean/* insert final dataset*/, clear

	* Question sur 999999 <- 13 obs for salmee. Want to confirm, outliers to be dropped?
	
	foreach var in salmee salred salred1 wsalred_CMA wsalred_LMA wsalred_lowess salsee smic_h smic_m  {
		cap noisily replace `var' = . if (`var' == 9999998 | `var' == 9999999 | `var' == 999999)
		cap drop `var'_2015
		gen `var'_2015 = `var' * (100/cpi)
	}
	