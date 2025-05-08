*************************************************
*************************************************
********* DATA CLEANING & SAMPLE CREATION *******
*************************************************
*************************************************

cd "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"

**********************************
***** Making the master data *****
**********************************

forvalues i = 2003/2020{
	
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
	
	* CPI, UE, merge
		cap destring annee mois trim, replace
		merge m:1 annee mois using "Data/Inflation/CPI_base_2015.dta"
		drop _merge 
		drop if annee != `i'
		merge m:1 annee trim using "Data/Unemployment/ueq_FR.dta" // Finding data by departement? 
		drop _merge
		drop if annee != `i'
		merge m:1 annee mois using "Data/SMIC/smic_FR.dta"
		drop _merge 
		drop if annee != `i'
		lab var smic_h "SMIC Horaire"
		lab var smic_m "SMIC Mensuel"

	* Stringing everything to make append possible
		quietly tostring *, replace 
		
	save Data/Clean/old_2003-2020/df_`i'.dta, replace
	}

	* Appending all years (building EEC_03_20)

clear all
	
	forvalues i = 2003/2020{
		
		display "appending year `i'"
		append using Data/Clean/old_2003-2020/df_`i'.dta
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
	
save Data/Clean/old_2003-2020/df1_master_notrim.dta, replace  


************************************
***** Trimming the master data ***** 
************************************

use Data/Clean/old_2003-2020/df1_master_notrim, clear
	
***** Dropping vars I won't need (dataset is 16.82GB)***** 

	drop ///
		presnoi* 								/// Présence dans le logement d'un indiv avec NOI égal à *
		dnai dnaim dnaip    					/// departementlieu de naisssance de l'individu/mére/père 
		///lnais lnaism lnaisp naimer naiper	 	/// lieu de naisssance de l'individu/mére/père 
		otravm otravp							/// Type de l'emploi de la mère/ père 
		spr sexeprm 							/// Sexe personne de reference du menage 
		///scj sexeprmcj 						/// Sexe du conjoint 
		noech 									/// numero de l'échantillon 
		mob 									/// Mouvement de la personne par rapport à l'enquête précédente
		noi* ident 								/// Les numeros d'identifiants, nous ne voullons que indiv
		rea 									/// Rang d'interrogation de l'aire (3 modalites)
		creant 									/// Condition de creation del'entreprise
		enbsaa enbsab 							/// Nbre salaries dans l'etablissement/entreprise un an avant
		durstg rstg durcs durlo durlp dursc		/// duree/ raison d'autres activités 
		ageq agq age3 age5 ag ag5 ag3 			/// misc age vars 
		ag3b agcj agecj ageprm ageprmcj 		/// misc age vars 
		agprm agprmcj agepr 					/// misc age vars 
		opa 									/// Orientation des productions agricoles, pour les exploitants agricoles
		nm* 									/// recherche de travail 
		rgi rga rgl 							/// rang d'interrogation de l'individu/aire/ logement 
		idaire tur5	tuu2010r					/// Numéro d'aire anonymisé/ Tranche d'unité urbaine (5 postes)
		mra mrb mrc mrd mre mrh mri mrj mrn mro /// recherche d'emploi 
		mrmie mrop mrbbis mrdbis mrgbis mrpassa /// recherche d'emploi 
		mrpassb mrpassc mrs mrsae 				/// recherche d'emploi 
		mvl 									/// a change de logement 
		res datcoll 							/// survey info  
		dmm* 									/// DOM related stats
		pent 									/// Code de la 1ere profession dans l'entreprise
		nafant* 								/// activite economique anterieure
		datult 									/// Date de debut de l'emploi ulterieur
		ancrech ancinact  						/// Anciennete de la recherche / inactivité (8 postes )
		eaidfonc 								// Réalisation un an auparavant de travaux de secrétariat, etc.
		 
		 
***** Trimming observations ***** 

	* Metropolitan France 
	
	drop if (metrodom != 1 & annee >= 2013)  						
	
	drop if (dep == "9A" | dep == "9B" | dep == "9C" | dep == "9D" | dep == "9E" | dep == "9F" | dep == "9G" | dep == "9H" | dep == "9I" | dep == "9J" | dep == "9K" | dep == "9L" | dep == "9M") 
	
	drop if (reg == 1 | reg == 2 | reg == 3 | reg == 4)
	
	drop if (deparc == "97" | deparc == "9A" | deparc == "9C" | deparc == "9E" | deparc == "9D") 
	
	drop if (depeta == "9A" | depeta == "9B" | depeta == "9C" | depeta == "9D" | depeta == "9E" | depeta == "9F" | depeta == "9G" | depeta == "9H" | depeta == "9I" | depeta == "9J" | depeta == "9K" | depeta == "9L" | depeta == "9M")

	* Active Population 
	
	keep if (acteu == 1 | acteu ==.)  
	drop if retrai == 1 						// in case  
	drop if (res == 16 | res == 14) 			// ménage composé exclusivement d'inactifs de 65 ans et plus
	
	* Should I drop the people receiving a retraite even if they are still active? 
	bysort indiv_num (datdeb): replace ret = 1 if ret[_n-1] ==1
	bysort indiv_num (datdeb): replace ret = 1 if ret[_n+1] ==1
	
	drop if temp !=. 							// Occupation actuelle d'un emploi, pour les plus de 75 ans 
	drop 	temp 
		
		
/*	Before being able to keep the for-profit non-agricultural sector, I need to harmonize naf10: 
	
	Job Industry is only categorized into 10 categories (nafg10n) starting 2008
		
	Before this, it is categorized into 16 categories 
		
	Before 2008:
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

	After 2008: 
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
		
	* Harmonizing Job Industry (4 categories) which changes name in 2008 and 2013 
	gen str2 naf4 = "." 
	replace naf4 = nafg4 if annee < 2008 
	replace naf4 = nafg4n if (2008 <= annee &  annee < 2013)
	replace naf4 = nafg004n if 2013 <= annee
	drop nafg4 nafg4n nafg004n
	lab var naf4 "Activité économique de l'établissement de l'emploi principal actuel (NAF en 4 postes)"
	
	* Harmonizing Job Industry (10 categories)
	gen str2 naf10= "."
	
	foreach val in 00 AZ BE FZ GI JZ KZ LZ MN OQ RU {
		replace naf10 = "`val'" if nafg010n == "`val'"
		replace naf10 = "`val'" if nafg10n == "`val'"
	}
	
	replace naf10 = "00" if nafg16 == "00" 
	replace naf10 = "AZ" if inlist(nafg16, "EA")
	replace naf10 = "BE" if inlist(nafg16, "EC", "ED", "EE", "EF", "EG", "EB")
	replace naf10 = "FZ" if nafg16 == "EH"
	replace naf10 = "GI" if inlist(nafg16, "EJ", "EK")
	replace naf10 = "KZ" if nafg16 == "EL"
	replace naf10 = "LZ" if nafg16 == "EM"
	replace naf10 = "MN" if nafg16 == "EN"
	replace naf10 = "OQ" if inlist(nafg16, "EQ", "ER")
	replace naf10 = "RU" if nafg16 == "EP"
	
	* Sampled workers: 
		
	gen non_agr1 		= (naf10 != "AZ")  				 			// non-agricultural 
	gen non_agri2		= (naf4 != "ES") 							// non-agricultural 
	gen for_profit 		= (naf10 != "OQ") 							// for-profit 
	gen hours			= (hhc <= 70 & hhc>=35)						// working >=35 & <= 70 hours 
	gen min_smic 		= (salmee >= smic_m)						// employee makes minimum wage 
	gen interruption 	= (pastra == 1) 							// interrupted workschedule 
	gen mod_agree 		= (redic == 1 ) 							// modulation agreemnt (dissapears after 2013!)
	gen ue_retired 		= (acteu != 1 | retrai == 1 | ret == 1)  	// ue or retired 
	gen misc_contracts 	= (contra != 1 & contra != 2) 				// holds a miscallaneous work contract (something else than CDI or CDD)
	gen full_time 		= (tppred == 1)		 						// full-time workers 
	
	gen sample_worker = (non_agr1 == 1 & non_agri2 ==1 & for_profit == 1 /*& hours ==1 & min_smic == 1*/ & interruption != 1 & /*mod_agree != 1 &*/ ue_retired != 1 & misc_contracts != 1 & full_time == 1) 

//	SHOULD PROBABLY USE THIS? 
// 	* Create individual-level flag
// egen flag = max(indicator), by(id)
//
// * Drop all observations for flagged individuals
// drop if flag == 1

	
	keep if sample_worker == 1 
	
save Data/Clean/old_2003-2020/df2_master_trimmed.dta, replace  


**********************************************
***** Harmonizing Variables Across Years ***** 
**********************************************

use Data/Clean/old_2003-2020/df2_master_trimmed, clear 

	* I need the retropoled weights on pre-2013 data if comparing pre-post 2013.
	
	gen new_weight = .
	replace new_weight = extri_retropole 	if annee <= 2010 
	replace new_weight = extri14_retropole 	if (annee == 2011 | annee == 2012) 
	replace new_weight = extri14 			if annee == 2013
	replace new_weight = extri15 			if annee == 2014
	replace new_weight = extri 				if annee >= 2013
	
	* Harmonizing Indicator for children & married 
	
	cap drop enfant 
	gen enfant 		= . 
	replace enfant 	= 1 if (em1 == 1 | enfred == 1)
	replace enfant 	= 0 if (em1 ==2 |enfred ==2)
	*drop em1 enfred
	lab var enfant "Présence des enfants de la personne dans le logement"

	* Generating an urban/ rural indicator 
	
	gen urban = (tuu == 1 | typuu2010 == "U")
	drop tuu typuu2010
	lab var urban "Situé en Zone Urbaine"

	* Generating age deciles
	
	drop if age > 70
	gen agd = .
	replace agd = 1 if age > 0 & age <= 10
	replace agd = 2 if age > 10 & age <= 20
	replace agd = 3 if age > 20 & age <= 30
	replace agd = 4 if age > 30 & age <= 40
	replace agd = 5 if age > 40 & age <= 50
	replace agd = 6 if age > 50 & age <= 60
	replace agd = 7 if age > 60 & age <= 70
	lab var agd "Tranche d'Age en Decennie" 

	* Generating foreigner indicator 
	
	gen etranger = .
	
	forvalues i = 1/3{
		
		replace etranger = `i' if nfr == `i'
		replace etranger = `i' if nfrred == `i'
	}
	lab var etranger "Nationalité française ou étrangère, redressée"
	drop nfr nfrred

	* Harmonizing pre & post-2013 income bracket 
	
	rename salmet tempvar 
	gen salmet =. 
	replace salmet = 1 if (tempvar == "A" | tempvar == "1")
	replace salmet = 2 if (tempvar == "B" | tempvar == "2")
	replace salmet = 3 if (tempvar == "C" | tempvar == "3") 
	replace salmet = 4 if (tempvar == "D" | tempvar == "4")
	replace salmet = 5 if (tempvar == "E" | tempvar == "5")
	replace salmet = 6 if (tempvar == "F" | tempvar == "6")
	replace salmet = 7 if (tempvar == "G" | tempvar == "7")
	replace salmet = 8 if (tempvar == "H") | (tempvar == "8" & annee >= 2013)
	replace salmet = 9 if (tempvar == "I" | tempvar == "9")
	replace salmet = 10 if (tempvar == "J" | tempvar == "10")
	replace salmet = 98 if (tempvar == "8" & annee < 2013) | (tempvar == "98")
	replace salmet = 99 if (tempvar == "99")
	drop tempvar 
	lab var salmet "Tranche de salaire de l'emploi principal"

	
	label define salmet_labels 1 "500-" 2 "500-1000" 3 "1000-1250" 4 "1250-1500" 5 "1500-2000" 6 "2000-2500" 7 "2500-3000" 8 "3000-5000" 9 "5000-8000" 10 "8000+" 98 "Refus" 99 "Ne sait pas"
	label values salmet salmet_labels
	
  
	* Harmonizing pre- & post-2013 desired hours worked 
	
	rename hplus tempvar 
	gen hplus = . 
	replace hplus = tempvar if annee < 2013
	replace hplus = hplusa if annee >= 2013 
	drop tempvar hplusa 
	lab var hplus " Nombre heures travail hebdo souhaité"

	* Harmonizing pre- & post-2013 category of primary job
	
	rename chpub tempvar 
	gen chpub = . 
	replace chpub = 3 if tempvar == 1 & annee < 2013
	replace chpub = 4 if tempvar == 2 & annee < 2013
	replace chpub = 5 if tempvar == 3 & annee < 2013
	replace chpub = 7 if tempvar == 4 & annee < 2013
	replace chpub = 2 if tempvar == 5 & annee < 2013
	replace chpub = 1 if tempvar == 6 & annee < 2013
	
	forvalues i = 1/7{
		
		replace chpub = `i' if (tempvar == `i' & annee >= 2013)
	}
	drop tempvar 
	lab var chpub "Nature de l'employeur prof princ"

	* typmen5 = 5 is split into typmen7 = 5, 6, 9 post-2013
	* This keeps typmen7 for post 2013 analysis, but harmonizes typmen5 throughout
	
	rename typmen5 tempvar 
	gen typmen5 = .
	
	forvalues i = 1/5{
		
		replace typmen5 = `i' if tempvar == `i'
	}

	forvalues i = 1/4{
		replace typmen5 = `i' if typmen7 == `i'
	}

	replace typmen5 = 5 if (typmen7 == 5 | typmen7 == 6 | typmen7 == 9)
	drop tempvar 
	lab var typmen5 "Type de ménage (5 postes)" 

	* Harmonizing peta which switches to payreg in 2013
	* Use `france` variable to confirm employer is located in France
	
	rename payreg tempvar 
	gen payreg = . 
	
	forvalues i = 1/8{
		replace payreg = `i' if tempvar == `i' 
	}

	replace payreg = 1 if peta == 22
	replace payreg = 2 if peta == 23
	replace payreg = 3 if peta == 28
	replace payreg = 4 if peta == 31
	replace payreg = 5 if peta == 21
	replace payreg = 6 if peta == 41
	replace payreg = 7 if peta == 25 
	drop tempvar peta
	lab var payreg "Pays frontalier de la France métropolitaine où se situe l'établissement employeur" 

	* Harmonizing deparc which switches to depeta in 2013
	
	replace depeta = "2.1" if depeta == "2A" 
	replace depeta = "2.2" if depeta == "2B"
	replace deparc = "2.1" if deparc == "2A" 
	replace deparc = "2.2" if deparc == "2B"
	replace dep = "2.1" if dep == "2A" 
	replace dep = "2.2" if dep == "2B"
	destring depeta dep deparc, replace 
	rename depeta tempvar
	gen depeta = . 
	replace depeta = tempvar if annee >= 2013 
	replace depeta = deparc if annee < 2013 
	drop tempvar deparc
	lab var depeta "Code du département de l'établissement employeur (principal)"
	order depeta

	* Past 2013, most time variables switch units from the decimals being minutes to fractions of hours, this loop corrects for this.
	* The current code yeilds the exact conversion. However, post-2013 values are only full of half integers. The code in /**/ below would correct for this, such that all values are either full or half-integers. 
	
	foreach var in hplus hhc emphre empnbh emphnh emphrc{
		
		replace `var' = floor(`var') + /*round*/(`var' - floor(`var'))/*)*/ * 100 / 60 /* * 2) /2 */ if annee < 2013
	}

	/* sou soua soub souc are non harmonizable between pre- and post-2013 due to the new collection method, 
	the same applies for pre-2013 creacc and post_2013 creaccm*. OTOH, pre-2013 creacc can be harmonized with post-2013 creaccp */
	
	rename creaccp tempvar 
	gen creaccp = . 
	forvalues i = 1/13 {
	
		replace creaccp = `i' if tempvar == `i'
	}
	replace creaccp = 1 if creacc == 1
	replace creaccp = 2 if creacc == 2
	replace creaccp = 5 if creacc == 3
	replace creaccp = 6 if creacc == 4
	replace creaccp = 7 if creacc == 5
	replace creaccp = 8 if creacc == 6
	replace creaccp = 9 if creacc == 7
	replace creaccp = 13 if creacc == 8
	drop tempvar creacc
	lab var creaccp "Raison principale de la recherche d'un autre emploi"

	/*raistp(pre-2013) --> raistp & raistf (post2013): Here we can harmonize but would loose some of the post 2013 precision. Should we? */
	
	gen raisg = . 
	replace raisg = 1 if raistf == 1 | (raistp == 2 & annee < 2013)
	replace raisg = 2 if raistf == 2 | (raistp == 4 & annee < 2013)
	replace raisg = 3 if raistf == 3 | raistf == 4 | (raistp == 5 & annee < 2013)
	replace raisg = 4 if (raistp == 2 & annee >= 2013) | (raistp == 3 & annee >= 2013) | (raistp == 1 & annee < 2013)
	replace raisg = 5 if (raistp == 3 & annee < 2013) 
	replace raisg = 6 if (raistp == 5 & annee >= 2013)
	replace raisg = 7 if (raistp == 4 & annee >= 2013)
	replace raisg = 8 if (raistp == 1 & annee >= 2013)
	replace raisg = 9 if raistf == 5 | (raistp == 6 & annee < 2013)
	lab var raisg "Raisons generales travail temps partiel" 

	* Generating unpaid OT hours
	
	gen emphnr = emphnh - emphre // Heures sup non-remunérées?

	* Cleaning nivet
	
	replace nivet = "." if nivet == "In"
	destring nivet, replace
	
save Data/Clean/old_2003-2020/df3_master_harmonized, replace 

****************************************
**# INFLATOR/ DEFLATOR - BASE 2015 *****
****************************************

use Data/Clean/old_2003-2020/df3_master_harmonized, clear

	* Question sur 999999 <- 13 obs for salmee. Want to confirm, outliers to be dropped?
	foreach var in valprie salmee salred salsee smic_h smic_m  /*INSERT ALL MONETARY VARIABLES*/{
		cap noisily replace `var' = . if (`var' == 9999998 | `var' == 9999999 | `var' == 999999)
		gen `var'_2015 = `var' * (100/cpi)
	}

*******************
***** DUMMIES *****
*******************

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

 	* Managers & Laborers (from CaCaJole)
	gen optim 		= . 
	replace optim  	= 1 if (cse == 34 |cse == 35 | cse==37 | cse==38 | cse==46 | cse==48) // manager 
	replace optim 	= 0 if (cse == 62 | cse == 63 | cse == 65 | cse == 67 | cse == 68)	  // laborer 

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
	
	* Male = 1, Female = 0, generating years studied
	replace sexe = 0 if sexe == 2
	
	* Generating country indicators to match the border country economic data 
	gen str border_country = "FRA" 
	replace border_country = "DEU" if trans_ger == 1
	replace border_country = "CHE" if trans_swz == 1
	replace border_country = "LUX" if trans_lux == 1 
	replace border_country = "BEL" if trans_bel == 1
	replace border_country = "ITA" if trans_it == 1
	replace border_country = "ESP" if trans_sp == 1 
	replace border_country = "GBR" if trans_uk == 1 

save Data/Clean/old_2003-2020/df4_master_final.dta, replace 

*****************************************************
**# BORDER COUNTRY QUARTERLY ECONOMIC SITUATION *****
*****************************************************

/*quarterly economic situation measured either by a business climate or by the share
of exports of goods and services in the GDP of the country c at date t (indicators
of the OECD).*/

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

use Data/Clean/old_2003-2020/df4_master_final.dta, clear
	order border_country datdeb_m 
	sort border_country datdeb_m
	merge m:1 border_country datdeb_m using "Data/Border_Country_Controls/OECD_CBC_Clean.dta"
	drop if _merge == 2
	drop _merge
	sort border_country datdeb_q
	merge m:1 border_country datdeb_q using "Data/Border_Country_Controls/OECD_GS_GDP_Clean.dta"
	drop if _merge == 2 
	drop _merge 
save "Data/Clean/old_2003-2020/df4_master_final.dta", replace 	
	
*******************************
**# Making the WEDGE data ***** 
*******************************

use Data/Clean/old_2003-2020/df4_master_final, clear

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
	
	gen wedge 					= hplus - empnbh	
	gen abs_wedge 		      	= abs(hplus - empnbh) 		// Assumption: marginal utility gain from a wedge closer to 0 is the same for under-worked and over-worked workers 
	gen mismatch				= . 
	replace mismatch 			= 1 if wedge != 0 & wedge !=.
	replace mismatch 			= 0 if wedge == 0 & wedge !=. 
	gen pos_wedge				= (wedge > 0)
	gen neg_wedge				= (wedge < 0)

	gen worker_type				= . 
	replace worker_type 		= 1 if (hplus - empnbh) > 0 
	replace worker_type 		= 2 if (hplus - empnbh) == 0 
	replace worker_type 		= 3 if (hplus - empnbh) < 0
	label define worker_type_lbl 1 "Underworked" 2 "No Wedge" 3 "Overworked"  
	label values worker_type worker_type_lbl 
	
	gen log_salred 	      	= log(salred)
	
	gen weekly_salred     	= salred/ 4.33
	
	gen hourly_salred 	  	= weekly_salred / empnbh 
	
	ge log_hourly_salred  	= log(hourly_salred)
	
	gen hourly_salmee 	  	= salmee/ (4.33*empnbh)
		
	gen log_hourly_salmee 	= log(hourly_salmee)
	
	gen log_empnbh 	      	= log(empnbh)
	
	gen empnbh_round = ceil(empnbh)
	
	gen married           	= (mpr == 2 | matriprm == 2)
	
	gen salred_sq = salred^2 
	
	encode naf10, gen(cat_naf10)
	
	gen age_group 			= . 
	replace age_group 		= 1 if (age <= 24 & age >=18)  	// OECD: just entering the working pop
	replace age_group 		= 2 if (age >= 25 & age <= 54)	// OECD: Prime working age 
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
	label define empnbh_labels 	1 "-20" 2 "20–24" 3 "25–29" 4  "30–34" 5  "35–39" 6  "40–44" ///
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
	
	drop if hhc < 35	// Full time workers work at least 35 hours in France 


	* Triple Difference Dummies
	
	gen pepa_elig = (salred < 3*smic_m)
	
	gen underworked = (wedge > 0)

save Data/Clean/old_2003-2020/test.dta, replace

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

	* Create a new variable to store the department codes
gen dep = ""

* Assign codes based on libellé2
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

	* Merging 

use Data/Clean/old_2003-2020/test.dta, clear
	cap drop _merge
	merge m:1 annee trim dep using "Data/Unemployment/ueq_localise.dta" 
	drop if _merge !=3
	drop _merge 

save "Data/Clean/old_2003-2020/test.dta", replace


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
	
use "Data/Clean/old_2003-2020/test.dta", clear
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
	
	merge m:1 reg2016 naf4 annee using "Data/GVA/annual_values.dta"
	
	
save "Data/Clean/old_2003-2020/test.dta", replace

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
	
use "Data/Clean/old_2003-2020/test.dta", clear
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
	
	drop _merge
	merge m:1 newnaf datdeb_q using "Data/Quarterly_Branch_Account/GVA_q.dta"
	rename _merge gva_merge 

	merge m:1 newnaf datdeb_q using "Data/Quarterly_Branch_Account/production_q.dta"
	
	/* 
		nafg36 data only goes to 2009. That's fine I only need until 2008. I provide the nafg36 codes to
		be able to understand the reclassification. In the end 2822 observations were not matched 
		these were observations with no reported nafg36 code.	
	*/
save "Data/Clean/old_2003-2020/test.dta", replace
		

*****************************
**# Making the HAD data ***** 
*****************************

use "Data/Clean/old_2003-2020/test.dta", clear

* Drop any existing vars to avoid conflicts
drop pre_tepa post_tepa in_tepa obs_count   

* Count number of observations per individual
bysort indiv (datdeb): gen obs_count = _N

* Create pre and post indicators
gen pre_tepa  = (datdeb < tepa_date)
gen post_tepa = (datdeb >= tepa_date)

* Count pre and post obs per individual
bysort indiv (datdeb): gen pre_count_tepa  = sum(pre_tepa)
bysort indiv (datdeb): gen post_count_tepa = sum(post_tepa)

* Create flag for individuals with at least 2 pre and 1 post obs
bysort indiv: gen temp_tepa = (pre_count_tepa[_N] >= 2 & post_count_tepa[_N] >= 1)

* Individual-level indicator for sample inclusion
egen in_tepa = min(temp_tepa), by(indiv)

* Clean up temp vars
drop temp_tepa pre_count_tepa post_count_tepa





	
********************************
***** Making the TEPA data ***** 
********************************

	preserve 
	
	* Removing variables with no observations 
	foreach var of varlist _all {	
		cap confirm numeric variable `var'
		
		if _rc == 0 {
			qui su `var'
			if r(N) == 0 drop `var'
		}
	}
	keep if in_tepa == 1 
	gen dummy1 = 1
	bysort indiv_num: egen balance_dummy = total(dummy1)
	// keep if balance_dummy == 6 ?
	save Data/Clean/old_2003-2020/df_tepa.dta, replace
	restore 

*********************************
***** Making the ABROG data *****
*********************************

	preserve 
	* Removing variables with no observations 
	foreach var of varlist _all {	
		cap confirm numeric variable `var'
		
		if _rc == 0 {
			qui su `var'
			if r(N) == 0 drop `var'
		}
	}
	keep if in_abrog == 1 
	gen dummy1 = 1
	bysort indiv_num: egen balance_dummy = total(dummy1)
	// keep if balance_dummy == 6 ?
	save Data/Clean/old_2003-2020/df_abrog.dta, replace
	restore 

**********************************
***** Making the MACRON data *****
**********************************

	preserve 
	* Removing variables with no observations 
	foreach var of varlist _all {	
		cap confirm numeric variable `var'
		
		if _rc == 0 {
			qui su `var'
			if r(N) == 0 drop `var'
		}
	}
	keep if in_macron == 1
	gen dummy1 = 1
	bysort indiv_num: egen balance_dummy = total(dummy1)
	// keep if balance_dummy == 6 ?
	save Data/Clean/old_2003-2020/df_macron.dta, replace
	restore
	
	bysort annee: tab nbenfind
*****************
***** FIXES *****
*****************

foreach df in df2_master_trimmed df3_master_harmonized df4_master_final df_tepa df_abrog df_macron {
	
	
	use Data/Clean/old_2003-2020/`df', clear
	
	cap drop enfant 
	
	gen enfant 		= . 
	replace enfant 	= 1 if (em1 == 1 | enfred == 1)
	replace enfant 	= 0 if (em1 ==2 |enfred ==2)
	//drop em1 enfred
	lab var enfant "Présence des enfants de la personne dans le logement "


	save Data/Clean/old_2003-2020/`df', replace
}
	
