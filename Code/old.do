********************
***** Old Code *****
********************


clear all
cd "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%% Data Prep/Cleaning Script %%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

**********************************
***** Making the master data ***** 
**********************************

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


