*************************************************
*************************************************
********* DATA CLEANING & PREP .DO FILE *********  
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
		
	save Data/Clean/df_`i'.dta, replace
	
	}

	* Appending all years (building EEC_03_20)

clear all
	
	forvalues i = 2003/2020{
		
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
	
***** Dropping vars I won't need (dataset is 16.82GB)***** 

	drop ///
		presnoi* 								/// Présence dans le logement d'un indiv avec NOI égal à *
		dnai dnaim dnaip    					/// departementlieu de naisssance de l'individu/mére/père 
		lnais lnaism lnaisp naimer naiper	 	/// lieu de naisssance de l'individu/mére/père 
		otravm otravp							/// Type de l'emploi de la mère/ père 
		spr sexeprm 							/// Sexe personne de reference du menage 
		scj sexeprmcj 							/// Sexe du conjoint 
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
		eaidfonc 								// 
		 
		 
***** Trimming observations ***** 

	* Metropolitan France 
	
	drop if (metrodom != 1 & annee >= 2013)  						
	
	drop if (dep == "9A" | dep == "9B" | dep == "9C" | dep == "9D" | dep == "9E" | dep == "9F" | dep == "9G" | dep == "9H" | dep == "9I" | dep == "9J" | dep == "9K" | dep == "9L" | dep == "9M") 
	
	drop if (reg == 1 | reg == 2 | reg == 3 | reg == 4)
	
	drop if (deparc == "97" | deparc == "9A" | deparc == "9C" | deparc == "9E") 
	
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
		
	gen non_agr 		= (naf10 != "AZ" ) 							// non-agricultural 
	gen for_profit 		= (naf10 != "OQ") 							// for-profit 
	//gen hours			= (hhc <= 70 & hhc>=35)						// working >=35 & <= 70 hours 
	//gen min_smic 		= (salmee >= smic_m)						// employee makes minimum wage 
	gen interruption 	= (pastra == 1) 							// interrupted workschedule 
	gen mod_agree 		= (redic == 1 ) 							// modulation agreemnt (dissapears after 2013!)
	gen ue_retired 		= (acteu != 1 | retrai == 1 | ret == 1)  	// ue or retired 
	gen misc_contracts 	= (contra != 1 & contra != 2) 				// holds a miscallaneous work contract (something else than CDI or CDD)
	gen full_time 		= (tppred == 1)		 						// full-time workers 
	
	gen sample_worker = (non_agr == 1 & for_profit == 1 /*& hours ==1*/ & min_smic == 1 & interruption != 1 & mod_agree != 1 & ue_retired != 1 & misc_contracts != 1 & full_time == 1) 
	
	keep if sample_worker == 1 
	
save Data/Clean/df2_master_trimmed.dta, replace  


**********************************************
***** Harmonizing Variables Across Years ***** 
**********************************************

use Data/Clean/df2_master_trimmed, clear 

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

	* Harmonizing Job Industry (4 categories) which changes name in 2008 and 2013 
	
	gen str2 naf4 = "." 
	replace naf4 = nafg4 if annee < 2008 
	replace naf4 = nafg4n if (2008 <= annee &  annee < 2013)
	replace naf4 = nafg004n if 2013 <= annee
	drop nafg4 nafg4n nafg004n
	lab var naf4 "Activité économique de l'établissement de l'emploi principal actuel (NAF en 4 postes)"

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
	
	/* 
		Harmonizing hplus and salred across survey rounds.
		
		ASSUMPTION:
		
			- If only one answer is given across survey waves, that answer applies to all waves. 
			
			- If multiple answers are given, the new answer signals a change. Hence, the most 
			recently given value carries over until reaching the new entry. From there, the new 
			entry is carried over. 
	
	
	foreach var in hplus salred {
		
		bysort indiv_num (datdeb): replace `var' = `var'[_n-1] if missing(`var') & !missing(`var'[_n-1])
	
		bysort indiv_num (datdeb): replace `var' = `var'[_n+1] if missing(`var') & !missing(`var'[_n+1])
	}
	*/
	
save Data/Clean/df3_master_harmonized, replace 

****************************************
**# INFLATOR/ DEFLATOR - BASE 2015 *****
****************************************

use Data/Clean/df3_master_harmonized, clear

	/* Question sur 999999 <- 13 obs for salmee. Outlier or millionaires?
	
	foreach var in valprie salmee salred salsee smic_h smic_m  /*INSERT ALL MONETARY VARIABLES*/{
		cap noisily replace `var' = . if (`var' == 9999998 | `var' == 9999999 | `var' == 999999)
		gen `var'_2015 = `var' * (100/cpi)
	}*/

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

	* PEPA Bonus Eligibility
	
	/*replace valprie = 0 if valprie == . 
	replace valprie_2015 = 0 if valprie_2015 == .
	gen just_wage_2015 = salmee_2015 - valprie_2015 if prim ==1 
	
	gen elig_pepa = (just_wage_2015 < (smic_m * 3) & datdeb > date("01jan2019", "DMY"))
	order elig_pepa*/

 	* Managers & Laborers (from CaCaJole)
	
	gen manager 	= 1 if 	(cse == 34 |cse == 35 | cse==37 | cse==38 | cse==46 | cse==48 )
	replace manager = 0 if 	manager == . 
	gen laborer 	= 1 if 	(cse == 62 | cse == 63 | cse == 65 | cse == 67 | cse == 68)
	replace laborer = 0 if 	laborer == . 

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

save Data/Clean/df4_master_final.dta, replace 

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

use Data/Clean/df4_master_final.dta, clear
	
	order border_country datdeb_m 
	
	sort border_country datdeb_m

	merge m:1 border_country datdeb_m using "Data/Border_Country_Controls/OECD_CBC_Clean.dta"
	
	drop if _merge == 2
	
	drop _merge
	
	sort border_country datdeb_q

	merge m:1 border_country datdeb_q using "Data/Border_Country_Controls/OECD_GS_GDP_Clean.dta"
	
	drop if _merge == 2 
	
	drop _merge 

save "Data/Clean/df4_master_final.dta", replace 

*********************************
***** Making the SHOCK data ***** 
*********************************

use Data/Clean/df4_master_final, clear 

	/*foreach var in salmee valprie emphre {
		
		qui su `var', detail
		local q1 = r(p25)  
		local q3 = r(p75)  
		
		local iqr = `q3' - `q1'
		
		local lower = `q1' - 3 * `iqr' 		// I use 3 * the IQR here to be less strict - compensation data usually has a long right tail 
		local upper = `q3' + 3 * `iqr'
		
		drop if (`var' < `lower' | `var' > `upper')
	}
	
	IQR method relies on assumption that the data is symmetric. Here my data is not symmetric. 
	
	I am not sure how to remove outliers, determining the values past which obs are outliers seems inconsistent. 

	*/ 
	
	
*******************************
**# Making the WEDGE data ***** 
*******************************

	
	
	
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
	save Data/Clean/df_tepa.dta, replace
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
	save Data/Clean/df_abrog.dta, replace
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
	save Data/Clean/df_macron.dta, replace
	restore
	
	bysort annee: tab nbenfind
*****************
***** FIXES *****
*****************

foreach df in df2_master_trimmed df3_master_harmonized df4_master_final df_tepa df_abrog df_macron {
	
	
	use Data/Clean/`df', clear
	
	cap drop enfant 
	
	gen enfant 		= . 
	replace enfant 	= 1 if (em1 == 1 | enfred == 1)
	replace enfant 	= 0 if (em1 ==2 |enfred ==2)
	//drop em1 enfred
	lab var enfant "Présence des enfants de la personne dans le logement "


	save Data/Clean/`df', replace
}
	
