clear all
cd /Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/2_Data/EEC_03_20/

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%% Data Prep/Cleaning Script %%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// EEC_03_20 Data Prep
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// -----------------------
// Making the master data 
// -----------------------

forvalues i = 2003/2020{
	use Raw/qc`i'.dta, clear 
	display "running on year `i'"
	
	// Making sure all vars are lower case 
	quietly foreach var of varlist * {
    local lowername = lower("`var'")
    rename `var' `lowername'
}
	// Generating unique ID and a month var for CPI merge: 
	cap drop indiv
	gen indiv = ident + noi 
	label variable indiv "NI d'individu"
	cap drop mois
	cap tostring datdeb, replace
	gen mois = substr(datdeb, 5, 2) 
	label variable mois "Mois de l'enquête"
	
	//Converting "datdeb" to actual date
	gen var1 = date(datdeb, "YMD")
	format var1 %td
	drop datdeb 
	rename var1 datdeb
	order indiv annee mois datdeb 
	
	// CPI, UE, merge
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
	cap destring metrodom reg, replace
	cap keep if metrodom == 1 
	cap drop if deparc > "95" 
	cap drop if dep > "95"
	foreach j in 9A  9B  9C  9D  9E  9F  9G  9H  9I  9J  9K  9L  9M {
		cap drop if dep == "`j'"
	}
	foreach j in 9A  9B  9C  9D  9E  9F  9G  9H  9I  9J  9K  9L  9M {
		cap drop if depeta == "`j'"
	}
	cap drop if (reg == 1 | reg == 2 | reg == 3 | reg == 4)
	
	// Population Active 
	cap destring retrai acteu reg res, replace 
	cap keep if (acteu == 1 | acteu ==.)  
	cap drop if retrai == 1 // in case  
	cap drop if (res == 16 | res == 14 | ret == 1) // ménage composé exclusivement d'inactifs de 65 ans et plus
	
	// Data set appending all years is too big to quickly run commands. So I'm dropping some variables I won't be needing: 
	foreach var in acteu_drap dnai scj noech otravm otravp index cateaav2020 compq dens2020 spr sexeprm sexeprmcj mob noi* ident rea creant redem acessep fodeba fodebm fofina fofinm fordur fortyp fc9a enbsaa opa enbsab durstg age3 age5 ag ag5 ag3 ag3b agcj agecj ageprm ageprmcj agprm agprmcj agepr metrodom ageq agq ancrech ancinact temp nm* form zus rgi rga rgl rstg datult nafant* dmm* presnoi* lnaisp lnaism pent dnaip dnaim naimer naiper idaire tur5 mra mrb mrc mrd mre mrh mrj mrn mro forniv cspp cspm res datcoll spr00 spr01 spr02 spr03 spr04 spr05 spr06 spr07 spr08 spr09 spr10 spr11 spr12 cov* nresid nfrp nfrm salpar{
	cap drop `var'
	}
	save Clean/df_`i'.dta, replace
}

// stringing everything to make append possible 
forvalues i = 2003/2020{
	display "stringing year `i'"
	use Clean/df_`i'.dta, clear
	quietly tostring *, replace
	save, replace
}

// Appending all years (building EEC_02_22)
clear all
forvalues i = 2003/2020{
	display "appending year `i'"
	append using Clean/df_`i'.dta
}

quietlyt destring * , replace
format datdeb %td 
gen datdeb_m = ym(year(datdeb), month(datdeb))
format datdeb_m %tm  
gen datdeb_q = yq(annee, trim)
format datdeb_q %tq
egen indiv_num = group(indiv)
replace sexe = 0 if sexe == 2 
gen annees_etudes = fordat - naia - 6
replace annees_etudes=. if annees_etudes<0 | annees_etudes>30

save Clean/df_master.dta, replace  

// -------------------------
// Harmonizing Variables Across Years 

*get fc5b back in!!
// -------------------------

use Clean/df_master, clear 

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

*Harmonizing pre & post-2013 income bracket 
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

/*label define salmet_labels 1 "€ < 500" 2 "500 <= € <1000" 3 "1000 <= € < 1250" 4"1250 <= € < 1500" 5 "1500 <= € < 2000" 6 "2000 <= € < 2500" 7 "2500 <= 7 < 3000" 8 "3000 <= € < 5000" 9 "5000 <= € < 8000" 10 "8000 <= €" 98 "Refus" 99 "Ne sait pas"
label values salmet salmet_labels*/
  
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

/* Job Industry is only categorized into 10 categories (nafg10n) starting 2008
* Before this, it is categorized into 16 categories 
* This harmonization needs to be checked in terms of the nafg16 recategorization! 
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
RU - Autres activités de services
*/
gen str2 naf10= "."
foreach val in 00 AZ BE FZ GI JZ KZ LZ MN OQ RU {
	replace naf10 = "`val'" if nafg010n == "`val'"
	replace naf10 = "`val'" if nafg10n == "`val'"
}
replace naf10 = "00" if nafg16 == "00"
replace naf10 = "AZ" if inlist(nafg16, "EA", "EB")
replace naf10 = "BE" if inlist(nafg16, "EC", "ED", "EE", "EF", "EG")
replace naf10 = "FZ" if nafg16 == "EH"
replace naf10 = "GI" if inlist(nafg16, "EJ", "EK")
replace naf10 = "KZ" if nafg16 == "EL"
replace naf10 = "LZ" if nafg16 == "EM"
replace naf10 = "MN" if nafg16 == "EN"
replace naf10 = "OQ" if inlist(nafg16, "EQ", "ER")
replace naf10 = "RU" if nafg16 == "EP"

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

/* sou soua soub souc are non harmonizable between pre- and post-2013 due to the new collection method, the same applies for pre-2013 creacc and post_2013 creaccm*. OTOH, pre-2013 creacc can be harmonized with post-2013 creaccp */
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

save Clean/df1_master.dta, replace 


// -----------------------
// INFLATOR/ DEFLATOR - BASE 2015
// -----------------------

* Question sur 999999 <- 13 obs for salmee. Outlier or millionaires?
foreach var in valprie salmee salred salsee smic_h smic_m  /*INSERT ALL MONETARY VARIABLES*/{
	cap noisily replace `var' = . if (`var' == 9999998 | `var' == 9999999)
	gen `var'_2015 = `var' * (100/cpi)
}

// -----------------------
// DUMMIES 
// -----------------------

* Shock dates
gen tepa_date = date("2007-10-01", "YMD")
gen abrog_date = date("2012-08-01", "YMD")
gen macron_date = date("2019-01-01", "YMD")
format tepa_date macron_date abrog_date %td

* Individuals in panel before and after the shock
bysort indiv (datdeb): gen obs_count = _N
foreach shock in tepa abrog macron {
	gen pre_`shock' = (datdeb < `shock'_date) 
	gen post_`shock' = (datdeb >= `shock'_date) 
	bysort indiv: gen temp_`shock' = (sum(pre_`shock') != obs_count & sum(post_`shock') != obs_count)
	egen in_`shock' = min(temp_`shock'), by(indiv)
	drop temp_`shock'
}

* Has worked OT before (after) shock 
gen pre_tepa_OT = 1 if (emphsc == 1 & post_tepa == 0)
replace pre_tepa_OT = 0 if pre_tepa_OT == . 
gen post_tepa_OT = 1 if (emphsc == 1 & post_tepa == 1)
replace post_tepa_OT = 0 if post_tepa_OT == . 
gen pre_abrog_OT = 1 if (emphsc == 1 & post_abrog == 0)
replace pre_abrog_OT = 0 if pre_abrog_OT == .
gen post_abrog_OT = 1 if (emphsc == 1 & post_abrog == 1)
replace post_abrog_OT = 0 if post_abrog_OT == . 
gen pre_macron_OT = 1 if (emphsc == 1 & post_macron == 0)
replace pre_macron_OT = 0 if pre_macron_OT == .
gen post_macron_OT = 1 if (emphsc == 1 & post_macron == 1)
replace post_macron_OT = 0 if post_macron_OT == . 

* PEPA Bonus Eligibility
replace valprie = 0 if valprie == . 
replace valprie_2015 = 0 if valprie_2015 == .
gen just_wage_2015 = salmee_2015 - valprie_2015
gen elig_pepa = (just_wage_2015 < (smic_m_2015*3) & datdeb > date("01jan2019", "DMY"))
order elig_pepa

/* Sampled workers:  We have eliminated employees who work under the lump-sum-of-days regime, as most managers do. For this category, it is not so much the weekly duration of work that is sensitive to detaxation as it is the total number of days worked during the year.*/
gen non_agr = (naf10 != "AZ" ) // non-agricultural (dependant on the naf10 recategorization - line 239)
gen for_profit = (naf10 != "OQ") // for-profit sector
gen interruption = (pastra == 1) // interrupted workschedule 
gen mod_agree = (redic == 1 ) // modulation agreemnt (dissapears after 2013!!!)
gen ue_retired = (acteu != 1 | retrai == 1 | ret == 1) // ue or retired 
gen misc_contracts = (contra != 1 & contra != 2) // holds a miscallaneous work contract (something else than CDI or CDD)
gen full_time = (hhc >= 35 & hhc <=70) // full time workers? 
gen sample_worker = (non_agr == 1 & for_profit == 1 & interruption != 1 & mod_agree != 1 & ue_retired != 1 & misc_contracts != 1 & full_time == 1) 
keep if sample_worker == 1 

// Managers & Laborers 
gen manager = 1 if (cse == 34 |cse == 35 | cse==37 | cse==38 | cse==46 | cse==48 )
replace manager = 0 if manager == . 
gen laborer = 1 if (cse == 62 | cse == 63 | cse == 65 | cse == 67 | cse == 68)
replace laborer = 0 if laborer == . 

* Domestic Border Workers (per neighbor country)
gen dom_bel = 1 if (dep == 59 | dep == 02 | dep == 08 | dep == 55 | dep == 54) & france == 1
replace dom_bel = 0 if dom_bel == . 
gen dom_ger = 1 if (dep == 57 | dep == 67 | dep == 68) & france == 1
replace dom_ger = 0 if dom_ger == .
gen dom_uk = 1 if (dep == 75 | dep == 62) & france == 1
replace dom_uk = 0 if dom_uk == .
gen dom_sp = 1 if (dep == 64 | dep == 65 | dep == 31 | dep == 09 | dep == 66) & france == 1
replace dom_sp = 0 if dom_sp == . 
gen dom_it = 1 if (dep == 74 | dep == 73 | dep == 05 | dep == 04 | dep == 06) & france == 1
replace dom_it = 0 if dom_it == . 
gen dom_swz = 1 if (dep == 74 | dep == 01 | dep == 39 | dep == 25 | dep == 90 | dep == 68) & france == 1 
replace dom_swz = 0 if dom_swz == . 
gen dom_lux = 1 if (dep == 54 | dep == 57) & france == 1 
replace dom_lux = 0 if dom_lux == .

* Domestic Border Workers (all neighbors)
gen dom_border = (dom_bel == 1 | dom_ger == 1 | dom_uk == 1 | dom_sp == 1 | dom_it == 1 | dom_swz == 1 | dom_lux == 1)

* Trans-border Workers (per neighbor country)
gen trans_bel = 1 if (dep == 59 | dep == 02 | dep == 08 | dep == 55 | dep == 54) & payreg == 2
replace trans_bel = 0 if trans_bel == . 
gen trans_ger = 1 if (dep == 57 | dep == 67 | dep == 68) & payreg == 1
replace trans_ger = 0 if trans_ger == . 
gen trans_uk = 1 if (dep == 75 | dep == 62) & payreg == 3
replace trans_uk = 0 if trans_uk == . 
gen trans_sp = 1 if (dep == 64 | dep == 65 | dep == 31 | dep == 09 | dep == 66) & payreg == 4
replace trans_sp = 0 if trans_sp == . 
gen trans_it = 1 if (dep == 74 | dep == 73 | dep == 05 | dep == 04 | dep == 06) & payreg == 5
replace trans_it = 0 if trans_it == . 
gen trans_swz = 1 if (dep == 74 | dep == 01 | dep == 39 | dep == 25 | dep == 90 | dep == 68) & payreg == 6
replace trans_swz = 0 if trans_swz == . 
gen trans_lux = 1 if (dep == 54 | dep == 57) & payreg == 7
replace trans_lux = 0 if trans_lux == . 

* Trans-border Workers (all neighbors)
gen trans_border = (trans_bel == 1 | trans_ger == 1 | trans_uk == 1 | trans_sp == 1 | trans_it == 1 | trans_swz == 1 | trans_lux == 1)

save Clean/df2_master.dta, replace 

// -----------------------
// Making the TEPA data 
// -----------------------

use Clean/df2_master.dta, clear 

preserve 
keep if (annee == 2006 | annee == 2007 | annee == 2008)
// keep if in_tepa == 1 
save Clean/df_tepa.dta, replace
restore 

// -----------------------
// Making the ABROG data 
// -----------------------

preserve 
keep if (annee == 2011 | annee == 2012 | annee == 2013)
// keep if in_abrog == 1 
save Clean/df_abrog.dta, replace
restore 

// -----------------------
// Making the MACRON data 
// -----------------------

preserve 
keep if (annee == 2018 | annee == 2019 | annee == 2020)
// keep if in_macron == 1
save Clean/df_macron.dta, replace
restore

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// TABLES
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Tables 
	- Who wants to work more? 
	- Average desired hours by income deciles (& income decile composition?)
		- by what means (table 1 tuda (2020))
	- Hours worked, part-time (motivated vs. non-motivated)
	- Hours of labor supply, including overtime and second jobs, by age group (figure 3 in Graversen & Smith 1998 - also look at table 1)
	
	*/
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// OVERALL: 2003-2020
use Clean/df2_master.dta, clear

* Average desired hours worked by income groups
bysort salmet: su hplus
bysort salmet: su hhc
bysort salmet: su emphre

// BY SHOCK: TEPA 
// BY SHOCK: ABROG
// BY SHOCK: MACRON
// -----------------------
// TEPA
// -----------------------
use Clean/df_tepa.dta, clear 
keep if in_tepa == 1 
keep if (stplc == 1 | stplc == 2)

foreach var in sexe agd hhc emphre annees_etudes { // I need these to eb categorical
	tabstat stplc, by(`var') stats(mean sd) nototal 
}
tabstat stplc, by(sexe) stats(mean sd) nototal 
tabstat stplc, by(agd) stats (mean sd) nototal
tabstat stplc, 
tabstat sexe age hhc emphre annees_etudes, by(stplc) stats(mean sd) nototal

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// FIGURES
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
PROBLEM: 
- Les champs couvert par les variables `emphre, ' ont "toutefoise
été élargis à partir de 2013. 
- Les questions posées pour les variables ` ' sont posées en toute 
génnéralité à partir de 2013. 
- 

Il va falloir reussir à harmoniser cela. 
*/
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// OVERALL: 2003-2020
use Clean/df2_master.dta, clear

* Paid Overtime 
preserve 
	collapse (mean) emphre [pweight=extri], by(indiv_num datdeb_q)
	collapse (mean) emphre, by(datdeb_q)
	twoway (bar emphre datdeb_q, barwidth(0.8)), ///
		   xtitle("") ytitle("Paid Overtime Hours", size(small)) ///
		   xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
		   ylabel(0(0.5) 6.5, grid labsize(small))

	graph export "Output/1.1.paid_OT_overall.png", as(png) replace
restore

* Paid Overtime, by worker type
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

* Total Hours Worked
preserve 
	collapse (mean) hhc [pweight=extri], by(indiv_num datdeb_q stplc)
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
	collapse (mean) hhc [pweight=extri], by(indiv_num datdeb_q stplc)
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


* Desired Weekly Working Hours
preserve 
	keep if (stplc == 1 | stplc == 2)
	collapse (mean) hplus [pweight=extri], by (indiv_num datdeb_q stplc)
	collapse (mean) hplus, by (datdeb_q stplc)
	sort datdeb_q	
	* Bar Chart
	twoway (bar hplus datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
		   (bar hplus datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
           by(stplc, title("") note("") legend(off)) ///
           xtitle("") ytitle("Desired Weekly Working Hours", size(small)) ///
           xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
           ylabel(41(0.5) 44, axis(1) labsize(small)) ///
		   ylabel(32(0.5) 36, axis(2) labsize(small))

	graph export "Output/3.1.desired_WH_bar.png", as(png) replace
	* Line Chart
	twoway (line hplus datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
		   (line hplus datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
           by(stplc, title("") note("") legend(off)) ///
           xtitle("") ytitle("Desired Weekly Working Hours", size(small)) ///
           xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
           ylabel(41(0.5) 44, axis(1) labsize(small)) ///
		   ylabel(32(0.5) 36, axis(2) labsize(small))
	
	graph export "Output/3.2.desired_WH_line.png", as(png) replace
restore 

* Bonuses (extensive)
* Overall 	
preserve	
	collapse (count) valprie [pweight = extri], by(datdeb_q)
	twoway (bar valprie datdeb_q, barwidth(0.8)), ///
		   xtitle("") ytitle("Number of Bonuses", size(small)) ///
		   xlabel(#20, format(%tq) angle(45) labsize(small)) ///
		   ylabel(, grid labsize(small))
		   
	graph export "Output/4.1.Quarterly_bonus_extensive_overall.png", as(png) replace
restore 
* By stplc
preserve 
	keep if (stplc == 1 | stplc == 2)
	collapse (count) valprie [pweight = extri], by(datdeb_q stplc) 
	twoway (bar valprie datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
		   (bar valprie datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
           by(stplc, title("") note("") legend(off)) ///
           xtitle("") ytitle("Number of Bonuses", size(small)) ///
           xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
           ylabel(, axis(1) labsize(small)) ///
		   ylabel(, axis(2) labsize(small))
	
	graph export "Output/4.2Quarterly_bonuse_extensive_bystplc.png", as(png) replace
restore 

* Bonuses(intensive) - 2015 values 
* Overall 
preserve	
	collapse (mean) valprie_2015 [pweight = extri], by(datdeb_q)
	twoway (bar valprie datdeb_q, barwidth(0.8)), ///
		   xtitle("") ytitle("Bonus Amouts (2015 €)", size(small)) ///
		   xlabel(#20, format(%tq) angle(45) labsize(small)) ///
		   ylabel(, grid labsize(small))
		   
	graph export "Output/5.1.Quarterly_bonus_intensive_overall.png", as(png) replace
restore 
* By stplc 
preserve 
	keep if (stplc == 1 | stplc == 2)
	collapse (mean) valprie_2015 [pweight = extri], by(datdeb_q stplc)
	twoway (bar valprie_2015 datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
		   (bar valprie_2015 datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
           by(stplc, title("") note("") legend(off)) ///
           xtitle("") ytitle("Bonus Amounts (2015 €)", size(small)) ///
           xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
           ylabel(, axis(1) labsize(small)) ///
		   ylabel(, axis(2) labsize(small))
	
	graph export "Output/5.2.Quarterly_Bonus_intensive_bystplc.png", as(png) replace
restore 


* Income Distribution
preserve
	keep if (stplc == 1 | stplc == 2)
	collapse (count) count_var = indiv_num, by(salmet stplc)
	drop if salmet == .
	bysort stplc: gen total = sum(count_var)
	bysort stplc: replace total = total[_N]
	gen percent = (count_var / total) * 100
	sort salmet
		graph bar percent, over(salmet) by(stplc, note("")) ///
		ytitle("Percentage of Individuals") ylabel(0(5)30, labsize(small)) 
	
	graph export "Output/Income_distribution_by_motivation.png", as(png) replace
restore

* Moonlighting 
*Overall
preserve 
	gen one_job = (am2nb == 1) 
	collapse (sum) one_job [pweight = extri], by(datdeb_q)
	twoway (bar one_job datdeb_q, barwidth(0.8)), ///
		   xtitle("") ytitle("Moonlighters", size(small)) ///
		   xlabel(#20, format(%tq) angle(45) labsize(small)) ///
		   ylabel(, grid labsize(small))

graph export "Output/6.1.Moonlighters_overall.png", as(png) replace
restore
* By stplc 
preserve 
	keep if (stplc == 1 | stplc == 2)
	gen one_job = (am2nb == 1) 
	collapse (sum) one_job [pweight = extri], by(datdeb_q stplc)
	twoway (bar one_job datdeb_q if stplc == 1, yaxis(1) mstyle(none)) ///
		   (bar one_job datdeb_q if stplc == 2, yaxis(2) mstyle(none)), ///
           by(stplc, title("") note("") legend(off)) ///
           xtitle("") ytitle("Moonlighters", size(small)) ///
           xlabel(#20, format(%tq) angle(45) labsize(vsmall)) ///
           ylabel(, axis(1) labsize(small)) ///
		   ylabel(, axis(2) labsize(small))

graph export "Output/6.2.Moonlighters_bystplc.png", as(png) replace
restore
	
use Clean/df2_master.dta, clear
// BY SHOCK: TEPA 
// BY SHOCK: ABROG
// BY SHOCK: MACRON

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// Regressions 
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// --------
// Y = stplc & Y = stmn 
// --------

// OVERALL: 2003-2020

//What impacts motivation?


// BY SHOCK: TEPA 
// BY SHOCK: ABROG
// BY SHOCK: MACRON




// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// Miscallenaous Commands
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// Counting columns in datasets: 
forvalues i = 2003/2020{
	use qc`i'.dta, clear
	qui desc
	local num_vars = r(k)
	display "qc`i'.dta has `num_vars' variables"
}

// Identifying which ones are common across 2003-2020
local files: dir . files "*.dta"  // Get all .dta files in the directory
local first = 1  // Track first dataset

tempname common_vars
tempfile common_vars_file

foreach file in `files' {
    use "`file'", clear
    unab vars: _all  // Get all variable names
    
    if `first' == 1 {
        // Save variables from the first dataset
        local common_vars `vars'
        local first = 0
    }
    else {
        // Keep only variables that exist in both the previous and current dataset
        local new_common_vars
        foreach var in `common_vars' {
            if strpos(" `vars' ", " `var' ") {
                local new_common_vars `new_common_vars' `var'
            }
        }
        local common_vars `new_common_vars'
    }
}

display "Common variables across all datasets: "
display "`common_vars'"

// Going from indiv back to indent + noi
gen noi = substr(indiv, -2, .)   
gen ident = substr(indiv, 1, length(indiv) - 2)  
order indiv ident noi
drop indiv
save, replace
