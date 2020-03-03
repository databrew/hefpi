////////////////////////////////////////////////////////////////////////////////
* This do file is an example of the computation of the financial protection 
* indicators using the Mali 2015-2016 "Enquête Modulaire et Permanente Auprès 
* des ménages 2015" (MLI_2015_EMOP_v01_M). 
*
* The fpro ado files can be installed with the following command:
* net install http://fmwww.bc.edu/RePEc/bocode/f/fpro.pkg 
*
////////////////////////////////////////////////////////////////////////////////
///////         MALI 2015 EMOP SURVEY                                ///////////
///////         Data preparation for financial protection analysis   ///////////		
////////////////////////////////////////////////////////////////////////////////

* set up																		
**********
tempfile tpf1 tpf2 

*******************************************************************************************
				*** IMPORT AND MERGE FILES AT HH LVL ***
*******************************************************************************************

*** Import raw dataset
*** Raw data has quarterly expenditure at consumption item level. 
use "D:/FP-DO FILE/MALI/Depenses_CONSO_MENAGE_FINALE_2015-2016.dta", clear
save `tpf1', replace

*** First step: Aggregate consumption exp. across consumption items at household level/quarter:
********************************************************************************
*** (1) Quarterly exp
use `tpf1', clear
collapse (sum) DEPTRI13 (mean) taille poidmen, by(grappe menage passage)
sort grappe menage passage
rename DEPTRI13 deptot
save `tpf2', replace

use `tpf1', clear
keep if fonction==6
collapse (sum) DEPTRI13, by(grappe menage passage)
sort grappe menage passage
rename DEPTRI13 health
sort grappe menage passage
merge 1:1 grappe menage passage using `tpf2'
replace health = 0 if _m==2
drop _m
save `tpf2', replace

use `tpf1', clear
keep if fonction==1
collapse (sum) DEPTRI13, by(grappe menage passage)
sort grappe menage passage
rename DEPTRI13 food
sort grappe menage passage
merge 1:1 grappe menage passage using `tpf2'
replace food = 0 if _m==2
drop _m
replace taille 	= 0 if passage<4
replace poidmen = 0 if passage<4
save `tpf2', replace

********************************************************************************

*** Second step: Aggregate consumption exp. (annual) at household level:	
********************************************************************************
*** (2) Annual exp.
* Somme des depenses trimestrielles
* Taille des menage = taille au dernier trimestre (passage==4)
* poids des menages = poids au dernier trimestre
collapse (sum) deptot health food taille poidmen, by(grappe menage)

sort grappe menage
save `tpf2', replace

use `tpf1', clear
keep grappe menage idse region milieu  
duplicates drop grappe menage, force
sort grappe menage
merge 1:1 grappe menage using `tpf2'
drop _m
drop if taille==0

********************************************************************************
																				
********************************************************************************
					*** STANDARDIZATION OF VARIABLES ***
********************************************************************************
																																								
* Household ID																	
egen hh_id = group(grappe menage)
																				
* Health exp (daily LCU per household)																	
gen hh_hexpd = health/365
																																																											
* Total household expenditure (daily LCU per household)											
gen hh_expd = deptot/365
																																																									
* Survey variables																
gen hh_size = taille
gen hh_strata = region
gen hh_psu = grappe
gen hh_sampleweight = poidmen
gen popweight = int(hh_size*hh_sampleweight)
																																																		
*** PPP Factor 
gen PPP = 309.11511

*** POVERTY LINES (expressed in daily LCU per capita)
gen PL190 = 1.90*PPP
gen PL320 = 3.20*PPP

********************************************************************************

********************************************************************************
					*** FINALIZE DATASET ***
********************************************************************************
										
#delimit ;
local vars 
"
hh_id hh_size hh_strata hh_psu hh_sampleweight popweight hh_expd hh_hexpd PPP PL*
"
;
#delimit cr
keep `vars'
order `vars'

la var hh_id				"Household ID"
la var hh_size 				"Household size"
la var hh_strata 			"Strata"
la var hh_psu 				"PSU"
la var hh_sampleweight		"Household sample weight"
la var popweight			"hh_sampleweight*hh_size"
la var hh_expd 				"Total hh exp. (LCU/hh/day)"
la var hh_hexpd 			"Total hh health exp. (LCU/hh/day)"
la var PPP 					"PPP factor"
la var PL190				"Poverty line ($1.90) in LCU per capita/day"
la var PL320				"Poverty line ($3.20) in LCU per capita/day"


********************************************************************************
																				
** Save																				
saveold "D:/FP-DO FILE/MALI/FP_MLI_2015_EMOP_v01_M.dta",replace

********************************************************************************
					*** FP INDICATORS ***
********************************************************************************

*** CATA (10% ; 25%)
fpcata , totexp(hh_expd) hexp(hh_hexpd) thresh(0.1 0.25) hhsize(hh_size) hhweight(hh_sampleweight)
/*
------------------------------------------------------------------
            -- INCIDENCE OF CATASTROPHIC PAYMENTS --             |
------------------------------------------------------------------
Level:        |Threshold:    |Incidence (%):     |CI (p-value):  |
------------------------------------------------------------------
   Population |          10% |              3.34 | -0.03 (0.43)  |
------------------------------------------------------------------
   Population |          25% |              0.33 |  0.13 (0.30)  |
------------------------------------------------------------------
(All results are accessible by typing -return list-)             |
------------------------------------------------------------------
*/
local cata10 = r(cata_pop_10)*100
local cata25 = r(cata_pop_25)*100


*** IMPOV ($1.9 ; $3.1)
fpimpov , totexp(hh_expd) hexp(hh_hexpd) pline(PL190 PL320) hhsize(hh_size) hhweight(hh_sampleweight)
/*

----------------------------------------------------------------------
            -- INCIDENCE OF IMPOVERISHING PAYMENTS --                |
----------------------------------------------------------------------
Level:        |Pov. line (LCU/cap/d):|Incidence (%):  |CI (p-value): |
----------------------------------------------------------------------
   Population |                 587.3|            1.22|  0.5 (0.00)  |
----------------------------------------------------------------------
   Population |                 989.2|            0.72| 0.84 (0.00)  |
----------------------------------------------------------------------
(All results are accessible by typing -return list-)                 |
----------------------------------------------------------------------

*/
local imp190 = r(impov_pop_PL1)*100
local imp320 = r(impov_pop_PL2)*100


********************************************************************************
////////////////////////////////////////////////////////////////////////////////
////////////////////         E N D           ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////



