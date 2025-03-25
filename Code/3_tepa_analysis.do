*********************************
*********************************
********* TEPA ANALYSIS *********  
*********************************
*********************************

use Data/Clean/df_tepa, clear




	/* 
		Variables of Interest:
			- stmn stplc 
			- autsal 
			- empnbh 
			- hplus is important:
				ASSUMPTION: hplus == . --> worker is satisfied with hours worked 
							hplus != . --> worker is not satisfied with hours worked
	*/ 

	
	
********************************************
***** CacaJole Identification Strategy *****
********************************************

use Data/Clean/df_tepa, clear
	
	drop if (trans_border == 0 & dom_border == 0) // workers not in the border regions	
	
	gen treatment = dom_border
	
	gen post_treatment = post_tepa * treatment
	
	global controls 
	

***** Regressions ***** 
	
	xtset indiv_num datdeb 
	
	xtreg post_treatment treatment, fe robust cluster(indiv_num)
	
	xtreg empnbh post_treatment post_tepa, fe robust cluster(indiv_num)
	
	xttab post_tepa if post-tepa==1 & manager==1 & post_tepa_OT==1 & emphre!=. & treatment==1 
