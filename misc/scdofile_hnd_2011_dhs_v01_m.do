
/*
This model dofile generates population and quintile means as well as the concentration index and its
standard error for a set of outcomes variables from the 2011 Honduras DHS micro-data. The dofile is limited
to those outcomes which can be generated from most DHS. The dofile uses the original DHS file-names, i.e. 
no file renaming is required after download.   
07 August 2018

CONTENTS
	[0] STATA PREPARATION
	[1] PREPARATION OF ORIGINAL MICRO-DATASETS
	[2] CHILD VARIABLES
	[3] WOMAN/ADULT VARIABLES
	[4] WEALTH DATA
	[5] LABEL RELEVANT VARIABLES
	[6] POPULATION & QUINTILE MEAN OUTCOMES AND CONCENTRATION INDEX
	[7] COLLAPSE MICRO-DATASET TO SINGLE OBSERVATION PER INDICATOR MESO-DATASET
	[8] INFANT & UNDER-5 MORTALITY
	[9] APPEND INFANT & UNDER-5 MORTALITY MESODATASET TO OTHER OUTCOME MESO-DATASET
*/

/********************************************************************
	[0] STATA PREPARATION
********************************************************************/
	version 12.1
	clear all
	set matsize 3956, permanent
	set more off, permanent
	set maxvar 32767, permanent
	capture log close
	sca drop _all
	matrix drop _all
	macro drop _all

	global root "E:/Dropbox/Work/World Bank/03_Projects/01 UHC/05 Shared/MEASURE UHC DATA/RAW DATA"
	* Define path for data sources
	global SOURCE "${root}/DHS/DHS-Honduras2011/"

	/***Installation of required programs
	ssc install distinct
	ssc install wbopendata
	ssc install kountry
	ssc install mat2txt
	ssc install syncmrates
	ssc install conindex
	ssc install concindex
	ssc install egenmore */

/********************************************************************
	[1] PREPARATION OF ORIGINAL MICRO-DATASETS
********************************************************************/

	tempfile ind2 ind3 wi2 hh2 hiv2 men2

	use "${SOURCE}/HNIR62FL.dta", clear				/* creating dataset with womendata to append later on*/
	keep v001 v002 v003 v005 b3_01 v201 v008 v130 v131 v149 /*							/*keep all w_data variables which will be used later on*/
	*/ v301 v302 v501 v705 v717 v729 v133 v011 v005 /*
	*/ caseid ml101 v463z v437 v438 v213 v307* v761 v626a/*
	*/ v761b v766* v012 v313 s1011 s1011a s1012* /*
	*/ v008 v502 v020 v312 v605 v215 m6_1 v225 m10_1 v528 v502 v512 v302* v605 v3a08d v000 v375a v376 v215 v605
	ren b3_01 w_doblastchild
	ren v001 hv001
	ren v002 hv002
	ren v003 hv003
	sort hv001 hv002 hv003
	gen w_data=1
	bysort hv001 hv002 hv003: gen test=_N			/* 2 people with exactly the same id but different responses on some vars*/
	drop if test==2|test==3|test==4
	drop test
	save `ind2', replace

	use "${SOURCE}/HNPR62FL.dta", clear
	drop hv003
	keep hv001 hv002 hvidx hv105 hv106 hv104 hv005 sh90
	ren hvidx hv003
	merge m:1 hv001 hv002 hv003 using `ind2'
	drop _merge
	drop if hv105<=5			/*drop those younger than 5*/
	save `ind3', replace

	use "${SOURCE}/HNHR62FL.dta", clear				/* creating hh dataset to merge and sorting hh data for the merge later on*/
	keep hv227 hv001 hv002 hv003 hv005 hhid hv009 hv021 hv025 hv205 hv024 hv270 hv271 hv201 	/* shquint shscore */ 	/*Specific for Vietnam*/
	bysort hv001 hv002: gen test=_N
	drop if test==2|test==3|test==4
	sort hv001 hv002 hv003 
	save `hh2', replace


/*********************************************
	[2] CHILD VARIABLES
*********************************************/

*** Prepare harmonized child variables

	use "${SOURCE}/HNIR62FL.dta", clear	
	
	#delimit;
		keep caseid v000 v001 v002 v003 v005 v201 v008 v011 b16*
		b3* b4* 											/*(dob and sex)*/
		b5*													/*(Alive)*/
		b11*												/* birth interval*/
		bord*												/* order of birth*/	
		hidx* h11* h12* h13* h14* h15*						/*(Diarrhea)*/
		h22*												/*(Fever)*/
		h31* h31b* h32* 									/*(Cough)*/
		m19*												/*(Birth weight)*/
		hwidx* hw1* hw2* hw3* hw5* hw8* hw11* hw13*	hw15*	/*(Malnutrition)*/
		h0* h1* h2* h3* h4* h5* h6* h7* h8* h9* 	 		/*(Vaccination)*/	
		midx* m1* m2* m14* m42*								/*(Antenatal care)*/
		m3* m15*											/*(Birth attendance)*/
		v131												/* maternal characteristics */
		v101 v102  v116 v119 v130 v131 v134 v008 v133
		v149 v151 v152 v501 v705 v717 v729 v133 v394
		v301 v302 v307*	v761 v761b v766a					/* contraception - v307* v761b v766a not available for Vietnam2002*/
		b7*													/* age at death*/
		v103 v104 v105 
		v437 v438 v445										/* underweight + obesity (WOMEN)*/
		v213 												/* currently pregnant*/
	    v463a v463b v463c* v463z 							/* smokes cigarettes, smokes pipe, smokes other tobacco (WOMEN) - Not available for Vietnam2002*/
		m4_* 												/* breastfeeding*/
		v225 v367											/* family planning (current pregnancy wanted + wanted last child)*/
	/*  ml0*  */											/* type of bednet(s) child slept under last night - Not available for Bangladesh2007, Philippines2008, Morocco2003-04 and Vietnam2002*/
	    ml101 									    		/* type of bednet(s) slept under last night (WOMEN) - Not available for Morocco2003-04 and Vietnam2002*/
		v535 												/* have ever been married (WOMEN)*/
	    h34*  	m54* 										/* vitamin a  - Not available for Vietnam2002*/
		; 										

		
	for X in num 1/6:	ren hidx_X chidxX \
						ren hwidx_X chwidxX \
						ren midx_X cmidxX;
				
	for X in num 1/9:	ren b3_0X b3_X \
						ren b4_0X b4_X \
						ren b5_0X b5_X \
						ren b11_0X b11_X \
						ren bord_0X bord_X \
						ren b16_0X b16_X \
						ren b7_0X b7_X;	

	for X in num 1/20: 	ren b3_X c_dobX \
						gen c_maleX=1 if b4_X==1 \
						replace c_maleX=0 if b4_X==2 \
						ren b5_X c_aliveX \
						ren b11_X c_birthintervalX \
						ren bord_X c_birthorderX \
						ren b16_X b16X \
						ren b7_X c_age_mthsdeathX;

*** Age of child in months *****;

	for X in num 1/20:	gen c_age_mthsX = (v008 - c_dobX) ;		


*** Dummies for vaccination REPORTED or RECORDED (WITH OR WITHOUT HEALTH CARD)/CRUDE ***; 
	for X in num 1/6: 	gen c_measles_vaccX =.\
						replace c_measles_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h9_X==1 | h9_X==2 | h9_X==3) \
						replace c_measles_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h9_X==0 \
							
						gen c_dtp1_vaccX = .\
						replace c_dtp1_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h3_X==1 | h3_X==2 | h3_X==3) \
						replace c_dtp1_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h3_X==0 \
				
						gen c_dtp2_vaccX = .\
						replace c_dtp2_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h5_X==1 | h5_X==2 | h5_X==3) \
						replace c_dtp2_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h5_X==0 \
				
						gen c_dtp3_vaccX = .\
						replace c_dtp3_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h7_X==1 | h7_X==2 | h7_X==3) \
						replace c_dtp3_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h7_X==0 \
	
						gen c_bcg_vaccX = .\
						replace c_bcg_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h2_X==1 | h2_X==2 | h2_X==3) \
						replace c_bcg_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h2_X==0 \

						gen c_polio1_vaccX = .\ 
						replace c_polio1_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h4_X==1 | h4_X==2 | h4_X==3) \
						replace c_polio1_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h4_X==0 \
				
						gen c_polio2_vaccX = .\ 
						replace c_polio2_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h6_X==1 | h6_X==2 | h6_X==3) \
						replace c_polio2_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h6_X==0 \
				
						gen c_polio3_vaccX = .\ 
						replace c_polio3_vaccX = 1 if c_age_mthsX>=15&c_age_mthsX<=23 & (h8_X==1 | h8_X==2 | h8_X==3) \
						replace c_polio3_vaccX = 0 if c_age_mthsX>=15&c_age_mthsX<=23 & h8_X==0 \
						
						gen c_fullimmX=. \									
						replace c_fullimmX=1 if (c_measles_vaccX==1 & c_dtp1_vaccX==1 & c_dtp2_vaccX==1 & c_dtp3_vaccX==1 & c_bcg_vaccX==1 & c_polio1_vaccX==1 & c_polio2_vaccX==1 & c_polio3_vaccX==1) \
						replace c_fullimmX=0 if (c_measles_vaccX==0 | c_dtp1_vaccX==0 | c_dtp2_vaccX==0 | c_dtp3_vaccX==0 | c_bcg_vaccX==0 | c_polio1_vaccX==0 | c_polio2_vaccX==0 | c_polio3_vaccX==0) \
						replace c_fullimmX=. if c_aliveX==0 ;
						
*** Insecticide treated bed-net use (variable missing from Honduras 2011 DHS;
						gen c_ITNX=.  ;
							
*** 4+ antenatal care visits (births in last 2 years);
	for X in num 1/6: 	gen c_ancX=m14_X \
						replace c_ancX=. if c_ancX==98 | c_ancX==99 \
						recode c_ancX (0/3=0) (4/95=1) \
						replace c_ancX=. if c_age_mthsX>23;
							
*** Skilled birth attendance;
	for X in num 1/6: 	gen c_sbaX = . \
						replace c_sbaX = 1 if (m3a_X==1 | m3b_X==1 | m3c_X==1 | m3d_X==1 | m3e_X==1 | m3f_X==1) \
						replace c_sbaX = 0 if c_sbaX==. \
						replace c_sbaX = . if m3a_X==. & m3b_X==. & m3c_X==. & m3d_X==. & m3e_X==. & m3f_X==.  \
						replace c_sbaX=. if c_age_mthsX>23;

*** Weight of child in kilograms;
	for X in num 1/6: 	ren hw2_X c_weightX \
						replace c_weightX=. if c_weightX>900 \
						replace c_weightX=c_weightX/10 ;

*** Height of child in centimeters;
	for X in num 1/6:	ren hw3_X c_heightX \
						replace c_heightX=. if c_heightX>9000 \
						replace c_heightX=c_heightX/10 ;

*** Child measured lying down or standing up;			
	for X in num 1/6: 	ren hw15_X c_measureX \
						replace c_measureX=. if c_measureX==0 \
						recode c_measureX (1=0) (2=1);
						
*** ORS received when diarrhea;
	for X in num 1/6: 	gen c_treatdiarrheaX=(h13_X==1|h13_X==2|h13b_X==1) \
						replace c_treatdiarrheaX=. if (h13_X==8|h13_X==9 | h13_X==.)&(h13b_X==8|h13b_X==9 | h13b_X==.) ;
										
*** Consultation with formal provider when cough and rapid breathing;			  
	for X in num 1/6:	gen c_coughX=(h31_X==1|h31_X==2) \				
						replace c_coughX=. if h31_X==8|h31_X==9|h31_X==. \
						gen c_rapidbreathX=1 if h31b_X==1  \
						replace c_rapidbreathX=0 if h31b_X==0 | c_coughX==0;
	forval i=1/6 {;
	gen c_treatARI`i'=.;
	replace c_treatARI`i'=0 if c_rapidbreath`i'==1;
	foreach x in a b c d e f g h j l m n  {;
	replace c_treatARI`i'=1 if c_rapidbreath`i'==1 & h32`x'_`i'==1;
	replace c_treatARI`i'=. if c_rapidbreath`i'==1 & h32a_`i'==9;
	};
	drop c_rapidbreath`i' c_cough`i';
	};

*** Reshaping from mother to child level;
	keep caseid v000 v001 v002 v003 v008 v011 v201 v005 chidx* chwidx* cmidx* b16*
			c_age_mths* c_anc* c_sba* c_male* v131 v011 c_dob* c_alive*
			v101 v102  v116 v119 v130 v131 v134 v133 v149 v151 v152 v301 v302 v761 v761b v766a v501 
			c_measles_vacc* c_dtp* c_bcg_vacc* c_polio* c_fullimm* v705 v717 v729 v133 v103 v104
			v105 v445 v213 v535 v225 v367 c_treat* c_weight* c_ITN* c_height* c_measure*
			v463a v463b v463c v463z ml10 ml101 v437 v438;
			
	reshape long b16 chidx chwidx cmidx c_age_mths c_dob c_alive c_male c_sba c_anc
			c_measles_vacc c_dtp1_vacc c_dtp2_vacc c_dtp3_vacc c_bcg_vacc c_polio1_vacc c_polio2_vacc c_polio3_vacc c_fullimm
			c_age_mthsdeath c_weight c_ITN c_height c_measure c_treatdiarrhea c_treatARI, i(caseid) j(child);
			
	#delimit cr

	
*** Drop children with missing age, those over 5 and the dead	
	drop if c_age_mths==.
	drop if c_age_mths>59 | c_alive==0

*** Child anthropometric variables
	tempfile cantro
	save `cantro'
	use "${SOURCE}/HNPR62FL.dta", clear
	keep hvidx hv001 hv002 hv103 hc70 hc71
	drop if hv103==0
	ren hv001 v001 
	ren hv002 v002
	ren hvidx b16
	foreach var in hc70 hc71 {
	replace `var'=. if `var'>900
	replace `var'=`var'/100
	}
	replace hc70=. if hc70<-6 | hc70>6
	replace hc71=. if hc71<-6 | hc71>5
	gen c_stunted=1 if hc70<-2
	replace c_stunted=0 if hc70>=-2 & hc70!=.
	gen c_underweight=1 if hc71<-2
	replace c_underweight=0 if hc71>=-2 & hc71!=.
	
	* Merge back with main dataset
	merge 1:m v001 v002 b16 using `cantro'
	drop _ 

*** Child and child data identifiers
	egen c_id=group(caseid child) 
	gen c_data=1
	
*** Rename identifiers for later merge	
	ren v001 hv001
	ren v002 hv002
	ren v003 hv003
	
*** Limit skilled birth attendance and anc to women currently aged 18-49 (like in WHS)
	tempfile tfw_age
	save `tfw_age'
	use "${SOURCE}/HNIR62FL.dta", clear	
	keep caseid v012
	merge 1:m caseid using `tfw_age'
	keep if _==3
	drop _
	foreach var of varlist c_sba c_anc {
	replace `var'=. if v012<18
	}
	drop v012

*** Skilled birth attendance and antenatal care only for last born children
	foreach var of varlist c_anc c_sba {
	replace `var'=. if child!=1
	}	
	
/*********************************************
	[3] WOMAN/ADULT VARIABLES
*********************************************/
	
***	Append individuals over 5 years
	append using `ind3' 
	replace c_data=0 if c_data==.
	replace w_data=0 if w_data==.
	
*** Create helper variables
	rename v201 w_totalchildren		
	rename v008 doi
	rename v213 w_pregnant
	rename v501 w_maritalstatus
	gen w_married=(w_maritalstatus==1|w_maritalstatus==2)
	replace w_married=. if w_maritalstatus==.
	rename v011 w_dob
	rename v012 w_age
	rename v005 sampleweight
	replace sampleweight=sampleweight/1000000
	encode caseid,gen(w_id)
	replace c_age_mths=(doi-w_dob) if w_data==1

*** Woman's BMI, overweight, obesity, and height
	foreach var of varlist v437 v438 {
	replace `var'=. if `var'==9999
	}
	replace v437=v437/10
	replace v438=v438/1000
	
	replace v437=. if v437>250
	replace v438=. if v438>2.50
		
	gen w_BMI=v437/(v438)^2
	replace w_BMI=. if (doi-w_doblastchild)<=3 | w_pregnant==1
	replace w_BMI=. if w_BMI<12 | w_BMI>70
		
	gen w_overweight_1549=1 if w_BMI>25
	replace w_overweight_1549=0 if w_BMI<=25
	replace w_overweight_1549=. if w_BMI==.
	
	gen w_obese_1549=1 if w_BMI>30
	replace w_obese_1549=0 if w_BMI<=30
	replace w_obese_1549=. if w_BMI==.

	rename v438 w_height_1549
	rename w_BMI w_BMI_1549

*** Contraceptive prevalence rate (CPR) of women married or living in union
	gen w_CPR=(v313==3)
	replace w_CPR=. if v313==.
	replace w_CPR=. if w_married!=1
	
*** Condom use in last intercourse of at risk women (2+ sexual partners in last 12 months)
	replace v766b=. if v766b==98|v766b==99
	gen w_concurrent=1 if v766b>1&v766b!=.
	replace w_concurrent=0 if v766b==0|v766b==1
	rename v761 w_condom
	replace w_condom=. if w_condom==8|w_condom==9
	replace w_condom=. if v766b==0 | v766b==.	
	gen w_condom_conc=1 if w_condom==1&w_concurrent==1
	replace w_condom_conc=0 if w_condom==0&w_concurrent==1
	replace w_condom_conc=. if hv105<18
	drop w_condom w_concurrent

*** Cervical cancer screening (pap smear test in past 2 years (women aged 30-49)
	replace s1011a=. if s1011a==98|s1011a==99
	gen w_papsmear=1 if (s1011==1&s1011a<=23)
	replace w_papsmear=0 if s1011==0
	replace w_papsmear=. if s1011==9
	replace w_papsmear=. if w_age<30|w_age>49
	rename w_papsmear w_pap_3049_2y
	
** Breast cancer screening (mammogram ever before survey (women aged 40-49)
	gen w_mammogram=1 if s1012c==1
	replace w_mammogram=0 if s1012c==0|s1012b==0
	replace w_mammogram=. if w_age<40|w_age>49
	rename w_mammogram w_mam_4049_ever
	
*** CREATE UNMET NEED FOR FAMILY PLANNING (REVISED DEFINITION)
	tempfile tempun
	save `tempun'
	use "${SOURCE}/HNIR62FL.dta", clear

	/* 	Stata program to create Revised unmet need variable as described in
		Analytical Study 25: Revising Unmet Need for Family Planing
		by Bradley, Croft, Fishel, and Westoff, 2012, published by ICF International
		measuredhs.com/pubs/pdf/AS25/AS25.pdf
	** 	Program written by Sarah Bradley and edited by Trevor Croft, last updated 23 January 2011
		SBradley@icfi.com
	**  Corrections 19 March 2013 for Cambodia 2010 - Trevor Croft.
	**  Correction 25 May 2013 (updated 7 July 2013) for Tanzania 2010 - Trevor Croft.
	**  Correction 10 May 2017 to work with DHS7 datasets.  
	**  Two changes: checks for v000 now look for "7", and tsinceb now calculated using v222 which is based on century day codes in DHS7 */

	g unmet=.
	**Set unmet need to NA for unmarried women if survey only included ever-married women or only collected necessary data for married women
	replace unmet=98 if v502!=1 & (v020==1 | substr(v000,3,1)=="2" | ///
		v000=="MA4" | v000=="TR2" | (v000=="CI3" & v007==94) | ///
		v000=="HT3" | v000=="IA5" | v000=="NP5")

	** CONTRACEPTIVE USERS - GROUP 1
	* using to limit if wants no more, sterilized, or declared infecund
	recode unmet .=4 if v312!=0 & (v605>=5 & v605<=7)
	* using to space - all other contraceptive users
	recode unmet .=3 if v312!=0

	**PREGNANT or POSTPARTUM AMENORRHEIC (PPA) WOMEN - GROUP 2
	* Determine who should be in Group 2
	* generate time since last birth
	g tsinceb=v222
	* generate time since last period in months from v215
	g tsincep	=	int((v215-100)/30) 	if v215>=100 & v215<=190
	replace tsincep =   int((v215-200)/4.3) if v215>=200 & v215<=290
	replace tsincep =   (v215-300) 			if v215>=300 & v215<=390
	replace tsincep =	(v215-400)*12 		if v215>=400 & v215<=490
	* initialize pregnant or postpartum amenorrheic (PPA) women
	g pregPPA=1 if v213==1 | m6_1==96
	* For women with missing data or "period not returned" on date of last menstrual period, use information from time since last period
	* 	if last period is before last birth in last 5 years
	replace pregPPA=1 if (m6_1==. | m6_1==99 | m6_1==97) & tsincep>tsinceb & tsinceb<60 & tsincep!=. & tsinceb!=.
	* 	or if said "before last birth" to time since last period in the last 5 years
	replace pregPPA=1 if (m6_1==. | m6_1==99 | m6_1==97) & v215==995 & tsinceb<60 & tsinceb!=.
	* select only women who are pregnant or PPA for <24 months
	g pregPPA24=1 if v213==1 | (pregPPA==1 & tsinceb<24)

	* Classify based on wantedness of current pregnancy/last birth
	* current pregnancy
	g wantedlast = v225
	* recode 'God's will' (survey-specific response) as not in need for Niger 1992
	recode wantedlast (4 = 1) if v000=="NI2"
	* last birth
	replace wantedlast = m10_1 if (wantedlast==. | wantedlast==9) & v213!=1
	* recode 'not sure' and 'don't know' (survey-specific responses) as unmet need for spacing for Cote D'Ivoire 1994 and Madagascar 1992
	recode wantedlast (4 8 = 2)
	* no unmet need if wanted current pregnancy/last birth then/at that time
	recode unmet .=7  if pregPPA24==1 & wantedlast==1
	* unmet need for spacing if wanted current pregnancy/last birth later
	recode unmet .=1  if pregPPA24==1 & wantedlast==2
	* unmet need for limiting if wanted current pregnancy/last birth not at all
	recode unmet .=2  if pregPPA24==1 & wantedlast==3
	* missing=missing
	recode unmet .=99 if pregPPA24==1 & (wantedlast==. | wantedlast==9)

	**NO NEED FOR UNMARRIED WOMEN WHO ARE NOT SEXUALLY ACTIVE
	* determine if sexually active in last 30 days
	g sexact=1 if v528>=0 & v528<=30
	* older surveys used code 95 for sex in the last 4 weeks (Tanzania 1996)
	recode sexact .=1 if v528==95
	* if unmarried and not sexually active in last 30 days, assume no need
	recode unmet .=97 if v502!=1 & sexact!=1

	**DETERMINE FECUNDITY - GROUP 3 (Boxes refer to Figure 2 flowchart in report)
	**Box 1 - applicable only to currently married
	* married 5+ years ago, no children in past 5 years, never used contraception, excluding pregnant and PPA <24 months
	g infec=1 			if v502==1 & v512>=5 & v512!=. & (tsinceb>59 | tsinceb==.) & v302==0  & pregPPA24!=1
	* in DHS VI, v302 replaced by v302a
	cap replace infec=1 if v502==1 & v512>=5 & v512!=. & (tsinceb>59 | tsinceb==.) & v302a==0 & pregPPA24!=1 & (substr(v000,3,1)=="6" | substr(v000,3,1)=="7")
	* survey-specific code for Cambodia 2010
	cap replace infec=1 if v502==1 & v512>=5 & v512!=. & (tsinceb>59 | tsinceb==.) & s313==0  & pregPPA24!=1 & v000=="KH5" & (v007==2010 | v007==2011)
	* survey-specific code for Tanzania 2010
	cap replace infec=1 if v502==1 & v512>=5 & v512!=. & (tsinceb>59 | tsinceb==.) & s309b==0 & pregPPA24!=1 & v000=="TZ5" & (v007==2009 | v007==2010)
	**Box 2
	* declared infecund on future desires for children
	replace infec=1 if v605==7
	**Box 3
	* menopausal/hysterectomy on reason not using contraception - slightly different recoding in DHS III and IV+
	* DHS IV+ surveys
	cap replace infec=1 if 	v3a08d==1 & (substr(v000,3,1)=="4" | substr(v000,3,1)=="5" | substr(v000,3,1)=="6" | substr(v000,3,1)=="7")
	* DHSIII surveys
	cap replace infec=1 if  v375a==23 & (substr(v000,3,1)=="3" | substr(v000,3,1)=="T")
	* special code for hysterectomy for Brazil 1996, Guatemala 1995 and 1998-9  (code 23 = menopausal only)
	cap replace infec=1 if 	v375a==28 & (v000=="BR3" | v000=="GU3")
	* reason not using did not exist in DHSII, use reason not intending to use in future
	cap replace infec=1 if  v376==14 & substr(v000,3,1)=="2"
	* below set of codes are all survey-specific replacements for reason not using contraception.
	* survey-specific code for Cote D'Ivoire 1994
	cap replace infec=1 if     v000== "CI3" & v007==94 & v376==23
	* survey-specific code for Gabon 2000
	cap replace infec=1 if     v000== "GA3" & s607d==1
	* survey-specific code for Haiti 1994/95
	cap replace infec=1 if     v000== "HT3" & v376==23
	* survey-specific code for Jordan 2002
	cap replace infec=1 if     v000== "JO4" & (v376==23 | v376==24)
	* survey-specific code for Kazakhstan 1999
	cap replace infec=1 if     v000== "KK3" & v007==99 & s607d==1
	* survey-specific code for Maldives 2009
	cap replace infec=1 if     v000== "MV5" & v376==23
	* survey-specific code for Mauritania 2000
	cap replace infec=1 if     v000== "MR3" & s607c==1
	* survey-specific code for Tanzania 1999
	cap replace infec=1 if     v000== "TZ3" & v007==99 & s607d==1
	* survey-specific code for Turkey 2003
	cap replace infec=1 if     v000== "TR4" & v375a==23
	**Box 4
	* Time since last period is >=6 months and not PPA
	replace infec=1 if tsincep>=6 & tsincep!=. & pregPPA!=1
	**Box 5
	* menopausal/hysterectomy on time since last period
	replace infec=1 if v215==994
	* hysterectomy has different code for some surveys, but in 3 surveys it means "currently pregnant" - Yemen 1991, Turkey 1998, Uganda 1995)
	replace infec=1 if v215==993 & v000!="TR3" & v000!="UG3" & v000!="YE2"
	* never menstruated on time since last period, unless had a birth in the last 5 years
	replace infec=1 if v215==996 & (tsinceb>59 | tsinceb==.)
	**Box 6
	*time since last birth>= 60 months and last period was before last birth
	replace infec=1 if v215==995 & tsinceb>=60 & tsinceb!=.
	* Never had a birth, but last period reported as before last birth - assume code should have been 994 or 996
	replace infec=1 if v215==995 & tsinceb==.
	* exclude pregnant and PP amenorrheic < 24 months
	replace infec=. if pregPPA24==1
	recode unmet .=9 if infec==1

	**FECUND WOMEN - GROUP 4
	* wants within 2 years
	recode unmet .=7 if v605==1
	* survey-specific code: treat 'up to god' as not in need for India (different codes for 1992-3 and 1998-9)
	recode unmet .=7 if v605==9 & v000=="IA3"
	recode unmet .=7 if v602==6 & v000=="IA2"
	* wants in 2+ years, wants undecided timing, or unsure if wants
	* survey-specific code for Lesotho 2009
	recode v605  .=4 if v000=="LS5"
	recode unmet .=1 if v605>=2 & v605<=4
	* wants no more
	recode unmet .=2 if v605==5
	recode unmet .=99
	  la def unmet ///
		1 "unmet need for spacing" ///
		2 "unmet need for limiting" ///
		3 "using for spacing" ///
		4 "using for limiting" ///
		7 "no unmet need" ///
		9 "infecund or menopausal" ///
		97 "not sexually active" ///
		98 "unmarried - EM sample or no data" ///
		99 "missing"
	  la val unmet unmet
	recode unmet (1/2=1 "unmet need") (else=0 "no unmet need"), g(unmettot)

	**Turkey 2003 -  section 6 only used if cluster even, household number even or cluster odd, household number odd
	drop if v000=="TR4" & (mod(v001,2) != mod(v002,2))

	**ADDED TO STANDARD CODE - 29 DEC 2015
	** Unmet need for family planning (revised definition)
	gen w_unmet_fp=1 if unmet==1|unmet==2
	replace w_unmet_fp=0 if unmet==3|unmet==4|unmet==7|unmet==9|unmet==97 
	keep w_unmet_fp caseid
	merge 1:m caseid using `tempun'
	drop if _m == 1
	drop _
	replace w_unmet_fp = . if w_married!=1
	sort hv001 hv002

*** Ensure women variables only defined for women
	foreach var of varlist w_age w_BMI_1549 w_condom_conc w_CPR w_dob w_doblastchild w_height_1549 /*
	*/ w_married w_obese_1549 w_overweight_1549 w_pregnant w_unmet_fp w_pap* w_mam* {
	replace `var'=. if w_data!=1
	}

*** Ensure child variables only defined for women
	foreach var of varlist c_age_mths c_alive c_fullimm c_anc c_bcg_vacc c_sba /*
	*/ c_dob c_dtp* c_height c_ITN c_measles_vacc c_measure c_polio* /*
	*/ c_treat* c_weight {
	replace `var'=. if c_data!=1
	}	
	
		
*** inpatient care use of adults (last 12 months)
	replace sh90=. if sh90==8|sh90==9
	ren sh90 a_inpatient_1yr
	replace a_inpatient_1yr = . if hv105<18

*** HIV-prevalence (missing from Honduras 2011)
	gen a_hiv=.

/*********************************************
	[4] WEALTH DATA
*********************************************/
	drop hv005
	merge m:1 hv001 hv002 using `hh2'
	tab _merge
	keep if _merge==3
	drop _merge

*** Wealth quintiles
	ren hv270 hhwealthquintile
	qui tab hhwealthquintile, gen (hhwealthq)
	ren hv271 hh_wealthscore_old
	egen hh_wealthscore_oldmin=min(hh_wealthscore_old) 
	gen hh_wealthscore=hh_wealthscore_old-hh_wealthscore_oldmin
	replace hh_wealthscore=hh_wealthscore/1000000
	replace hv005=hv005/1000000
	xtile hh_quintile_cat=hh_wealthscore [aweight=hv005],nq(5)
	label define hh_quintile_cat 1 "poorest" 2 "poorer" 3 "middle" 4 "richer" 5 "richest"
    label value hh_quintile_cat hh_quintile_cat
	
*** Country and year information and reference-id
	gen country = "Honduras"
	gen year = 2011
	gen referenceid = "HND_2011_DHS_v01_M"
	gen survey = "DHS"
	gen iso3c = "HND"

*** Determine final outcome variables
	#delimit;
	global DHSvars
	"a_hiv
	a_inpatient_1yr
	c_anc
	c_fullimm
	c_ITN
	c_measles_vacc
	c_sba
	c_stunted
	c_treatARI
	c_treatdiarrhea
	c_underweight
	w_BMI_1549
	w_condom_conc
	w_CPR
	w_height_1549
	w_mam_4049_ever
	w_obese_1549
	w_overweight_1549
	w_pap_3049_2y
	w_unmet_fp"
	;
	#delimit cr

*** Keep relevant variables	
	keep $DHSvars hh_quintile_cat hh_wealthscore sampleweight country iso3c year survey referenceid 
	
	
/*********************************************
	[5] LABEL RELEVANT VARIABLES
*********************************************/
	label var a_hiv "adult's hiv prevalence (1/0)"
	label var a_inpatient_1yr "household member was hospitalized in last 12 months (1/0)"
	label var c_anc "mother had 4+ antenatal care visits (1/0)"
	label var c_fullimm "child aged 15-23 months received BCG, measles, and three doses of polio & DPT (1/0)"
	label var c_ITN "child slept under insecticide treated net (ITN) last night (1/0)"
	label var c_measles_vacc "child aged 15-23 months received measles vaccination (1/0)"
	label var c_sba	"skilled birth attendance (1/0)"
	label var c_stunted "child's Height-for-Age: <-2 std.dev. from median (according to 2006 WHO) (1/0)"
	label var c_treatARI "Consultation with formal provider for ARI (1/0)"
	label var c_treatdiarrhea "child with diarrhea received ORS (1/0)"
	label var c_underweight "child's Weight-for-Age: <-2 std.dev. from median (according to 2006 WHO) (1/0)"
	label var hh_quintile_cat "poorest(1) to richest(5) wealth quintile"
	label var hh_wealthscore "wealth score from PCA"
	label var sampleweight "sample weight"
	label var w_BMI_1549 "BMI of woman not currently pregnant&not in last 3 months)"
	label var w_condom_conc "woman with 2+ sexual partners in last 12 months used condom during last intercourse (1/0)"
	label var w_CPR "woman married or living in union is currently using a modern method of contraception (1/0)"
	label var w_height_1549 "woman's height (in meters)"
	label var w_mam_4049_ever "woman(40-49) ever received a mammogram (1/0)"
	label var w_obese_1549 "woman with BMI above 30 (not currently pregnant&not in last 3 months)(1/0)"
	label var w_overweight_1549 "woman with BMI above 25 (not currently pregnant&not in last 3 months)(1/0)"
	label var w_pap_3049_2y "woman(30-49) had PAP smear test in last 2 years (1/0)"
	label var w_unmet_fp "married women (sexually active) with unmet need for family planning (1/0)"
	label var country "country"
	label var year "year"
	label var referenceid "survey id"
	label var survey "survey"
	label var iso3c "iso3c country code"


/************************************************************************
	[6] POPULATION & QUINTILE MEAN OUTCOMES AND CONCENTRATION INDEX
*************************************************************************/

*** Generate empty variables 	
	foreach y in $DHSvars {
		gen pop`y' = .
		gen N`y' = .
		gen CI`y' = .
		gen stderror_CI`y' = .
		forvalues i=1/5 {
			gen Q`i'`y' = .
			gen N`i'`y'=.
		}
	}	

*** Generate population means
	foreach y in $DHSvars {
			sum `y'  [aw=sampleweight] , de 
			replace N`y' = r(N) 
			replace pop`y' = r(mean)
			replace pop`y' =. if N`y'<100
	}

*** Generate quintile means, concentration index and its SE
	foreach y in $DHSvars {
		cap conindex `y'  [aw=sampleweight] , rankvar(hh_wealthscore) robust truezero /* CI HHs ranked by pc income  */ 
		replace CI`y' = r(CI) 
		replace stderror_CI`y' = r(CIse)  
		tabstat `y' [aw=sampleweight] , by(hh_quintile_cat) save 
		forval j=1/5 {
			matrix a = r(Stat`j') 
			replace Q`j'`y' = a[1,1]
		}
		tabstat `y' [aw=sampleweight], s(N) by(hh_quintile_cat) save
		forval j=1/5 {
			matrix a = r(Stat`j') 
			replace N`j'`y' = a[1,1]
		}
		forval j=1/5 {
			replace Q`j'`y'=. if N`j'`y'<25
			drop N`j'`y'
		}
	}
	

*** Adjust cancer screening rates to preferred recall periods
** Pap-smear rates adjusted to 5-year recall 
	foreach z in pop Q1 Q2 Q3 Q4 Q5 {
		gen `z'w_pap_5y = .
		replace `z'w_pap_5y = 1-(1-`z'w_pap_3049_2y)^(5/2)
	}
	foreach z in N CI stderror_CI {
	gen `z'w_pap_5y = .
	replace `z'w_pap_5y = `z'w_pap_3049_2y
	}
** Mammography rates adjusted to 2-year recall	
	foreach z in pop Q1 Q2 Q3 Q4 Q5 {
		gen `z'w_mam_2y = .
		replace `z'w_mam_2y = 1-(1-`z'w_mam_4049_ever)^(2/(((40+49)/2)-50))
	}
	foreach z in N CI stderror_CI {
		gen `z'w_mam_2y = .
		replace `z'w_mam_2y =`z'w_mam_4049_ever
	}

	
/**********************************************************************************
	[7] COLLAPSE MICRO-DATASET TO SINGLE OBSERVATION PER INDICATOR MESO-DATASET
**********************************************************************************/
	keep in 1
	gen id = 1
	reshape long pop N Q1 Q2 Q3 Q4 Q5 CI stderror_CI, i(id) j(indic) string
	
** Drops points with inadmissable mammogram and pap smear age-ranges
	foreach y in pop N Q1 Q2 Q3 Q4 Q5 CI stderror_CI {
	replace `y' = . if pop<0 | pop==.
	}
** Drops original cancer screening variables
	drop if inlist(indic,"w_mam_4049_ever","w_pap_3049_2y")	
	
** keep relevant variables
	keep CI N Q1 Q2 Q3 Q4 Q5 indic pop referenceid stderror_CI survey country year iso3c
	order referenceid iso3c country year survey indic pop Q* CI std*

** Temporarily saves dataset to be appended to child mortality data below
	tempfile data1
	save `data1', replace

	
/*********************************************
	[8] INFANT & UNDER-5 MORTALITY
*********************************************/
	sca drop _all
	mat drop _all

	/*syncmrates is the mother programme with 3 sub-routines:
		- syncmrates_1: child mortality rates with bootstrap standard errors
		- syncmrates_2: test differences in mortality rates between groups
		- syncmrates_3: mortality trends with CIs over time*/

	use "${SOURCE}/HNBR62FL.DTA" , clear

	tempfile tpfHonduras2011 tpf1Honduras2011 tpf2Honduras2011 tpf3Honduras2011 tpf4Honduras2011 tpf5Honduras2011 tpffinalHonduras2011

	ge mnths_born_bef_int = v008 - b3 /* months born before interview  */ 
	recode b5 (1=0)(0=1) , ge(dead)   
	label var dead "died" 
	label define yesno 0 "No" 1 "Yes"
	label values dead yesno 
	tab b5 dead , mi nol 

	ge age_at_dth_mnths = b7
	replace age_at_dth_mnths = . if b13~=0
	ge age_alive_mnths = mnths_born_bef_int 
	ge time = age_at_dth_mnths 
	replace time = age_alive_mnths if dead==0
	replace time = 0 if time<0

	replace v005 = v005/1000000

	ltable time dead if inrange(mnths_born_bef_int,1,61), saving (`tpfHonduras2011') 

	preserve
	use `tpfHonduras2011' , clear 
	qui sum start if t0==0 , de
	sca N = r(mean)
	di N

	restore

	syncmrates v008 b3 b7 [iw=v005], t0(61) t1(1) /* previous 5 years of births */ 
	mat a = r(table)
	sca imr = a[1,3] 
	sca seimr = a[2,3]
	sca u5mr = a[1,5] 
	sca seu5mr = a[2,5]

	forval i = 1/5 {
		di "`i'"
		ltable time dead if v190==`i' & inrange(mnths_born_bef_int,1,121), saving (`tpf`i'Honduras2011')  
		cap syncmrates v008 b3 b7 [iw=v005] if v190==`i' , t0(121) t1(1) /* previous 10 years of births */ 
		mat a = r(table)
		sca imr`i' = a[1,3] 
		sca seimr`i' = a[2,3]
		sca u5mr`i' = a[1,5] 
		sca seu5mr`i' = a[2,5]
	}	

	preserve
	forval i = 1/5 {
		use `tpf`i'Honduras2011' , clear 
		sum start if t0==0 , de
		sca N`i' = r(mean)
	}
	restore

	sca li _all

	mat NN = N , N1 , N2 , N3 , N4 , N5 
	mat imrs = imr , imr1 , imr2 , imr3 , imr4 , imr5 
	mat seimr = seimr , seimr1 , seimr2 , seimr3 , seimr4 , seimr5 
	mat u5mrs = u5mr , u5mr1 , u5mr2 , u5mr3 , u5mr4 , u5mr5 
	mat seu5mr = seu5mr , seu5mr1 , seu5mr2 , seu5mr3 , seu5mr4 , seu5mr5 

	mat res = NN' , imrs' , seimr' , u5mrs' , seu5mr' 
	mat li res

	preserve 
	clear
	svmat res 
	drop if _n==1
	rename res1 N 
	rename res2 imr 
	rename res3 seimr 
	rename res4 u5mr 
	rename res5 seu5mr

	ge wealth=_n

	concindc imr [aw=N], welf(wealth) sig(seimr)
	sca ci_imr = r(concindex)
	sca se_ci_imr = r(stdci)

	concindc u5mr [aw=N], welf(wealth) sig(seu5mr)
	sca ci_u5mr = r(concindex)
	sca se_ci_u5mr = r(stdci)
	restore 

	sum v007 , de 
	sca year=r(min) 

	mat imrs = N , imrs , ci_imr , se_ci_imr , year 
	mat u5mrs = N , u5mrs , ci_u5mr , se_ci_u5mr , year 
	mat res2 = imrs \ u5mrs 
	mat li res2 

	clear
	svmat res2
	ge indic=""
	replace indic="c_imr" if _n==1 
	replace indic="c_u5mr" if _n==2
	rename res21 N 
	rename res22 pop 
	rename res23 Q1 
	rename res24 Q2 
	rename res25 Q3
	rename res26 Q4
	rename res27 Q5
	rename res28 CI
	rename res29 stderror_CI
	rename res210 year 
	for var Q* pop  : replace X=X*1000 

/************************************************************
	[9] APPEND INFANT & UNDER-5 MORTALITY MESODATASET TO OTHER OUTCOME MESO-DATASET
************************************************************/
	ge survey="DHS"
	gen iso3c = "HND"
	gen country ="Honduras"
	ge year_s = year
	tostring year_s , replace 
	gen referenceid=iso3c+"_"+year_s+"_"+survey+"_v01"+"_M"
	order iso3c year survey referenceid indic Q* pop CI stderror_CI N 
	drop year_s
	append using `data1'





