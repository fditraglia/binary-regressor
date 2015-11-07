#delimit;
clear;
set more off;
set mem 50m;


****MERGE DATASETS AND CREATE SOME VARIABLES; 

use PV_survey, clear;
	sort respid;
	merge respid using Followup_survey;
	tab _merge;
	drop _merge;
sort clinicid;
	merge clinicid using clinic_level_baselinedata;
	drop _merge;

gen PV_sday = date(PV_surveydate, "MDY");
gen PV_lastvisitday = date(PV_lastvisit, "DMY");
gen PV_time_since_last_visit=PV_sday-PV_lastvisitday;
	replace PV_time_since=. if (PV_time_since < 0 | PV_time_since > 300);
gen PV_dayofweek=dow(PV_sday);
	label var PV_dayofweek "Day of the week at which survey administered";
gen PV_timesinceprogstart=PV_sday- timestart;
	label var  PV_timesinceprogstart "# Days between start of bednet program in clinic and prenatal visit survey";
gen FOL_gap=FOL_surveydate-PV_sday;
	label var FOL_gap "# Days between Prenatal Visit survey and followup survey";

save all_data, replace;



********************************************************************;
********************            TABLE 1     ************************;
********************************************************************;

#delimit;
use clinic_level_baselinedata, clear;

egen avefirstvisits=rmean(firstvisits_02-firstvisits_09) ;
egen avetotvisits=rmean(tot_anc_02-tot_anc_09); 

#delimit;
gen netprice10=0;  replace netprice10=1 if netprice==10;
gen netprice20=0;  replace netprice20=1 if netprice==20;
gen netprice40=0;  replace netprice40=1 if netprice==40;
gen netprice50=0;  replace netprice50=1 if netprice==.;
gen netprice0=0;  replace netprice0=1 if netprice==0;


gen var="";
for any control_mean control_sd free_mean p10_mean p20_mean p40_mean
pC_0 pC_10 pC_20 pC_40 pF_10 pF_20 pF_40 p10_20 p10_40 p20_40 pjointeq pjoint0: gen X=.;
local vars=6;
for any
avefirstvisits avetotvisits anc_fee hivtesting
clinicswithin10km disttoclosest
\ num 1/`vars':
 replace var="X" if _n==Y \
 reg X netprice0 if netprice0==1|netprice50==1  \
  test netprice0=0 \
 replace pC_0=r(p) if _n==Y \
 reg X netprice10 if netprice10==1|netprice50==1  \
  test netprice10=0 \
 replace pC_10=r(p) if _n==Y \
 reg X netprice20 if netprice20==1|netprice50==1  \
  test netprice20=0 \
 replace pC_20=r(p) if _n==Y \
 reg X netprice40 if netprice40==1|netprice50==1  \
  test netprice40=0 \
 replace pC_40=r(p) if _n==Y \
 reg X netprice10 if netprice10==1|netprice0==1  \
  test netprice10=0 \
 replace pF_10=r(p) if _n==Y \
 reg X netprice20 if netprice20==1|netprice0==1  \
  test netprice20=0 \
 replace pF_20=r(p) if _n==Y \
 reg X netprice40 if netprice40==1|netprice0==1  \
  test netprice40=0 \
 replace pF_40=r(p) if _n==Y \
 reg X netprice20 if netprice20==1|netprice10==1  \
  test netprice20=0 \
 replace p10_20=r(p) if _n==Y \
 reg X netprice40 if netprice40==1|netprice10==1  \
  test netprice40=0 \
 replace p10_40=r(p) if _n==Y \
 reg X netprice40 if netprice40==1|netprice20==1  \
  test netprice40=0 \
 replace p20_40=r(p) if _n==Y \

 sum X if netprice==. \
 replace control_mean=r(mean) if _n==Y \
 replace control_sd=r(sd) if _n==Y \
 sum X if netprice==0  \
 replace free_mean=r(mean) if _n==Y \
 sum X if netprice==10  \
 replace p10_mean=r(mean) if _n==Y \
 sum X if netprice==20  \
 replace p20_mean=r(mean) if _n==Y \
 sum X if netprice==40  \
 replace p40_mean=r(mean) if _n==Y \

reg X netprice0 netprice10 netprice20 netprice40\
testparm  netprice0 netprice10 netprice20 netprice50, equal\
 replace pjointeq=r(p) if _n==Y \
test  netprice0 netprice10 netprice20 netprice40\
 replace pjoint0=r(p) if _n==Y \

;
for var control_mean-pjoint0: replace X=round(X, 0.001);
outsheet var control_mean-pjoint0 if _n<=`vars' using table1.xls,replace;

*****************************;
****RAW DATA FOR APPENDIX 3;
*****************************;
preserve;
	collapse avetotvisits, by(clinicid netprice);
	sort netprice clinicid;
outsheet using TableApp3row0.out, replace;
restore;


***********************************************************************;
****************   TABLE 2: FISHERIAN PERMUTATIONS     ****************;
***********************************************************************;


#delimit;
set matsize 10000;
set seed 298483;


**********************************************;
****************COLUMNS 1-6 IN TABLE 2;
**********************************************;
use clinic_level_nets_sales, clear;
merge clinicid using clinic_level_baselinedata;
drop _merge;

gen avetotvisits=(  tot_anc_02+ tot_anc_03+ tot_anc_04 +  tot_anc_05+ tot_anc_06+ tot_anc_07+ tot_anc_08+ tot_anc_09)/8 ;

* PROBLEM OF MISSING BASELINE DATA FOR CLINIC 23;
	egen avetotvisits23=rmean(tot_anc_01-tot_anc_06) if clinicid==23;
		replace avetotvisits=avetotvisits23 if clinicid==23;

gen netprice10=0;  replace netprice10=1 if netprice==10;
	gen netprice20=0;  replace netprice20=1 if netprice==20;
	gen netprice40=0;  replace netprice40=1 if netprice==40;
gen group10=.; replace group10=1 if netprice==0|netprice==10;
	gen group20=.; replace group20=1 if netprice==0|netprice==20;
	gen group40=.; replace group40=1 if netprice==0|netprice==40;	

global indepvar=" avetotvisits";

save forperm2, replace;
mat RANDINF2=J(10000,7,0);

sum weeklynetsales if netprice==0;
gen mean=r(mean);

mat RANDINF2[1,1]=0;
quietly xi: reg weeklynetsales i.netprice if group10==1;
mat RANDINF2[1,2]=_b[_Inetprice_10];
outreg using forfisher_tests2.xls, se bdec(2) 3aster addstat ("Mean in Free Group", mean) replace;
quietly xi: reg weeklynetsales i.netprice ${indepvar} if group10==1; 
mat RANDINF2[1,5]=_b[_Inetprice_10];
outreg using forfisher_tests2.xls, se nonotes bdec(2) 3aster append;

quietly xi: reg weeklynetsales i.netprice if group20==1;
mat RANDINF2[1,3]=_b[_Inetprice_20];
outreg using forfisher_tests2.xls, se nonotes bdec(2) 3aster append;
quietly xi: reg weeklynetsales i.netprice ${indepvar} if group20==1; 
mat RANDINF2[1,6]=_b[_Inetprice_20];
outreg using forfisher_tests2.xls, se nonotes bdec(2) 3aster append;

quietly xi: reg weeklynetsales i.netprice if group40==1;
mat RANDINF2[1,4]=_b[_Inetprice_40];
outreg using forfisher_tests2.xls, se nonotes bdec(2) 3aster append;
quietly xi: reg weeklynetsales i.netprice ${indepvar} if group40==1; 
mat RANDINF2[1,7]=_b[_Inetprice_40];
outreg using forfisher_tests2.xls, se nonotes bdec(2) 3aster append;

gen randprice=0;

local j 2;
while `j'<=10000 {;
       mat RANDINF2[`j',1]=`j';
       quietly gen rand=uniform();
	
       sort group10 rand;
       quietly replace randprice=10 if _n>5&group10==1;
       quietly xi: reg weeklynetsales i.randprice if group10==1;
       mat RANDINF2[`j',2]=_b[_Irandprice_10];
	 quietly xi: reg weeklynetsales i.randprice ${indepvar} if group10==1; 
       mat RANDINF2[`j',5]=_b[_Irandprice_10];
	 quietly replace randprice=0;

       sort group20 rand;
       quietly replace randprice=20 if _n>5&group20==1;
       quietly xi: reg weeklynetsales i.randprice if group20==1;
       mat RANDINF2[`j',3]=_b[_Irandprice_20];
	 quietly xi: reg weeklynetsales i.randprice ${indepvar} if group20==1; 
       mat RANDINF2[`j',6]=_b[_Irandprice_20];
	 quietly replace randprice=0;

       sort group40 rand;
       quietly replace randprice=40 if _n>5&group40==1;
       quietly xi: reg weeklynetsales i.randprice if group40==1;
       mat RANDINF2[`j',4]=_b[_Irandprice_40];
	 quietly xi: reg weeklynetsales i.randprice ${indepvar} if group40==1; 
       mat RANDINF2[`j',7]=_b[_Irandprice_40];

       quietly drop rand ;
	 quietly  replace randprice=0;
       di "iteration `j' completed";
       local j=`j'+1;
       };

#delimit;
svmat RANDINF2;
keep  RANDINF21- RANDINF27;
drop if _n>10000;

rename RANDINF21 iteration;
rename RANDINF22 weekly_coeff10;
rename RANDINF23 weekly_coeff20;
rename RANDINF24 weekly_coeff40;
rename RANDINF25 weekly_coeff10_controls;
rename RANDINF26 weekly_coeff20_controls;
rename RANDINF27 weekly_coeff40_controls;

gen true=0;
replace true=1 if iteration==0;
gen obs=1;
save working, replace;

mat PVAL=J(40,7,0);
mat PVAL[10,1]=10;
mat PVAL[20,1]=20;
mat PVAL[40,1]=40;

foreach price in 10 20 40 {;
	use working, clear;
	mat PVAL[`price',2]=weekly_coeff`price'[1];
	collapse (sum) true, by( weekly_coeff`price') ;
	replace weekly_coeff`price'=-weekly_coeff`price' if weekly_coeff`price' <0;
	sort weekly_coeff`price';
	gen rank=_n if true==1;
	gen pval_`price'=1-rank/_N if true==1;
	mat PVAL[`price',4]=_N-1;
	keep if true==1;
	mat PVAL[`price',3]=pval_`price'[1];

	use working, clear;
	mat PVAL[`price',5]=weekly_coeff`price'_controls[1];
	collapse (sum) true, by( weekly_coeff`price'_controls) ;
	replace weekly_coeff`price'_controls=-weekly_coeff`price'_controls if weekly_coeff`price'_controls <0;
	sort weekly_coeff`price'_controls;
	gen rank=_n if true==1;
	gen pval_`price'=1-rank/_N if true==1;
	mat PVAL[`price',7]=_N-1;
	keep if true==1;
	mat PVAL[`price',6]=pval_`price'[1];
	di `price';

};

svmat PVAL;
keep PVAL1-PVAL7;
rename PVAL1 price;
rename PVAL2 coeff;
rename PVAL3 pval;
rename PVAL4 obs;
rename PVAL5 coeff_controls;
rename PVAL6 pval_controls;
rename PVAL7 obs_controls;
keep if _n==10|_n==20|_n==40;
outsheet using fisher_pvalues2.xls, replace;

**************************************************;
**********COLUMNS 7 to 18 in TABLE 2 ************;
**************************************************;
#delimit;
use all_data, clear;

drop if retroactive==1;

gen effec_cov=FOL_hanging;
	replace effec_cov=0 if PV_buynet==0;

gen avefirstvisits=( firstvisits_02+ firstvisits_03+ firstvisits_04+ firstvisits_05+ firstvisits_06+ firstvisits_07+ firstvisits_08+ firstvisits_09)/8 ;
gen avetotvisits=(  tot_anc_02+ tot_anc_03+ tot_anc_04 +  tot_anc_05+ tot_anc_06+ tot_anc_07+ tot_anc_08+ tot_anc_09)/8 ;

collapse PV_buynet effec_cov netprice PV_dayofweek PV_timesinceprogstart anc_fee  any_CT clinicswithin10km  disttoclosestprogramclinic avefirstvisits avetotvisits, by(district clinicid);

global indepvar=" avetotvisits";

gen group10=.; replace group10=1 if netprice==0|netprice==10;
	gen group20=.; replace group20=1 if netprice==0|netprice==20;
	gen group40=.; replace group40=1 if netprice==0|netprice==40;	

save forperm2, replace;
mat RANDINF=J(10000,7,0);
mat RANDINF1=J(10000,7,0);

sum PV_buynet if netprice==0;
gen mean=r(mean);

mat RANDINF[1,1]=0;
quietly xi: reg PV_buynet i.netprice if group10==1;
mat RANDINF[1,2]=_b[_Inetprice_10];
outreg using forfisher_tests.xls, se bdec(2) 3aster addstat ("Mean in Free Group", mean) replace;
quietly xi: reg PV_buynet i.netprice ${indepvar} if group10==1; 
mat RANDINF[1,5]=_b[_Inetprice_10];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;

quietly xi: reg PV_buynet i.netprice if group20==1;
mat RANDINF[1,3]=_b[_Inetprice_20];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;
quietly xi: reg PV_buynet i.netprice ${indepvar} if group20==1; 
mat RANDINF[1,6]=_b[_Inetprice_20];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;

quietly xi: reg PV_buynet i.netprice if group40==1;
mat RANDINF[1,4]=_b[_Inetprice_40];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;
quietly xi: reg PV_buynet i.netprice ${indepvar} if group40==1; 
mat RANDINF[1,7]=_b[_Inetprice_40];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;

drop mean;
sum effec_cov if netprice==0;
gen mean=r(mean);

mat RANDINF1[1,1]=0;
quietly xi: reg effec_cov i.netprice if group10==1;
mat RANDINF1[1,2]=_b[_Inetprice_10];
outreg using forfisher_tests.xls, se bdec(2) 3aster addstat ("Mean in Free Group", mean) append;
quietly xi: reg effec_cov i.netprice ${indepvar} if group10==1; 
mat RANDINF1[1,5]=_b[_Inetprice_10];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;

quietly xi: reg effec_cov i.netprice if group20==1;
mat RANDINF1[1,3]=_b[_Inetprice_20];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;
quietly xi: reg effec_cov i.netprice ${indepvar} if group20==1; 
mat RANDINF1[1,6]=_b[_Inetprice_20];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;

quietly xi: reg effec_cov i.netprice if group40==1;
mat RANDINF1[1,4]=_b[_Inetprice_40];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;
quietly xi: reg effec_cov i.netprice ${indepvar} if group40==1; 
mat RANDINF1[1,7]=_b[_Inetprice_40];
outreg using forfisher_tests.xls, se nonotes bdec(2) 3aster append;


**********S.E. for the ratios in Table 2;

xi: sureg (PV_buynet i.netprice)  (effec_cov i.netprice) if group10==1;
nlcom [effec_cov]_Inetprice_10/[PV_buynet]_Inetprice_10 ;

xi: sureg (PV_buynet i.netprice)  (effec_cov i.netprice) if group20==1;
nlcom [effec_cov]_Inetprice_20/[PV_buynet]_Inetprice_20 ;

xi: sureg (PV_buynet i.netprice)  (effec_cov i.netprice) if group40==1;
nlcom [effec_cov]_Inetprice_40/[PV_buynet]_Inetprice_40 ;

xi: sureg (PV_buynet i.netprice ${indepvar})  (effec_cov i.netprice ${indepvar}) if group10==1;
nlcom [effec_cov]_Inetprice_10/[PV_buynet]_Inetprice_10 ;

xi: sureg (PV_buynet i.netprice ${indepvar})  (effec_cov i.netprice ${indepvar}) if group20==1;
nlcom [effec_cov]_Inetprice_20/[PV_buynet]_Inetprice_20 ;

xi: sureg (PV_buynet i.netprice ${indepvar})  (effec_cov i.netprice ${indepvar}) if group40==1;
nlcom [effec_cov]_Inetprice_40/[PV_buynet]_Inetprice_40 ;


gen randprice=0;

local j 2;
while `j'<=10000 {;
       mat RANDINF[`j',1]=`j';
       mat RANDINF1[`j',1]=`j';
       quietly gen rand=uniform();
	
       sort group10 rand;
       quietly replace randprice=10 if _n>5&group10==1;
       quietly xi: reg PV_buynet i.randprice if group10==1;
       mat RANDINF[`j',2]=_b[_Irandprice_10];
	 quietly xi: reg PV_buynet i.randprice ${indepvar} if group10==1; 
       mat RANDINF[`j',5]=_b[_Irandprice_10];
	       quietly xi: reg effec_cov i.randprice if group10==1;
	       mat RANDINF1[`j',2]=_b[_Irandprice_10];
		 quietly xi: reg effec_cov i.randprice ${indepvar} if group10==1; 
	       mat RANDINF1[`j',5]=_b[_Irandprice_10];
	 quietly replace randprice=0;

       sort group20 rand;
       quietly replace randprice=20 if _n>5&group20==1;
       quietly xi: reg PV_buynet i.randprice if group20==1;
       mat RANDINF[`j',3]=_b[_Irandprice_20];
	 quietly xi: reg PV_buynet i.randprice ${indepvar} if group20==1; 
       mat RANDINF[`j',6]=_b[_Irandprice_20];
 	       quietly xi: reg effec_cov i.randprice if group20==1;
	       mat RANDINF1[`j',3]=_b[_Irandprice_20];
		 quietly xi: reg effec_cov i.randprice ${indepvar} if group20==1; 
	       mat RANDINF1[`j',6]=_b[_Irandprice_20];
	 quietly replace randprice=0;


       sort group40 rand;
       quietly replace randprice=40 if _n>5&group40==1;
       quietly xi: reg PV_buynet i.randprice if group40==1;
       mat RANDINF[`j',4]=_b[_Irandprice_40];
	 quietly xi: reg PV_buynet i.randprice ${indepvar} if group40==1; 
       mat RANDINF[`j',7]=_b[_Irandprice_40];
	       quietly xi: reg effec_cov i.randprice if group40==1;
	       mat RANDINF1[`j',4]=_b[_Irandprice_40];
		 quietly xi: reg effec_cov i.randprice ${indepvar} if group40==1; 
	       mat RANDINF1[`j',7]=_b[_Irandprice_40];

       quietly drop rand ;
	 quietly  replace randprice=0;
       di "iteration `j' completed";
       local j=`j'+1;
       };

svmat RANDINF;
svmat RANDINF1;
keep  RANDINF1- RANDINF7 RANDINF11-RANDINF17;
drop if _n>10000;

rename RANDINF1 iteration;
rename RANDINF2 buynet10;
rename RANDINF3 buynet20;
rename RANDINF4 buynet40;
rename RANDINF5 buynet10_controls;
rename RANDINF6 buynet20_controls;
rename RANDINF7 buynet40_controls;

rename RANDINF12 effec_cov10;
rename RANDINF13 effec_cov20;
rename RANDINF14 effec_cov40;
rename RANDINF15 effec_cov10_controls;
rename RANDINF16 effec_cov20_controls;
rename RANDINF17 effec_cov40_controls;

gen true=0;
replace true=1 if iteration==0;
gen obs=1;
save working, replace;

mat PVAL=J(40,7,0);
mat PVAL[10,1]=10;
mat PVAL[20,1]=20;
mat PVAL[40,1]=40;

foreach price in 10 20 40 {;
	use working, clear;
	mat PVAL[`price',2]=buynet`price'[1];
	collapse (sum) true, by( buynet`price') ;
	replace buynet`price'=-buynet`price' if buynet`price' <0;
	sort buynet`price';
	gen rank=_n if true==1;
	gen pval_`price'=1-rank/_N if true==1;
	mat PVAL[`price',4]=_N-1;
	keep if true==1;
	mat PVAL[`price',3]=pval_`price'[1];

	use working, clear;
	mat PVAL[`price',5]=buynet`price'_controls[1];
	collapse (sum) true, by( buynet`price'_controls) ;
	replace buynet`price'_controls=-buynet`price'_controls if buynet`price'_controls <0;
	sort buynet`price'_controls;
	gen rank=_n if true==1;
	gen pval_`price'=1-rank/_N if true==1;
	mat PVAL[`price',7]=_N-1;
	keep if true==1;
	mat PVAL[`price',6]=pval_`price'[1];
	di `price';

};

svmat PVAL;
keep PVAL1-PVAL7;
rename PVAL1 price;
rename PVAL2 coeff;
rename PVAL3 pval;
rename PVAL4 obs;
rename PVAL5 coeff_controls;
rename PVAL6 pval_controls;
rename PVAL7 obs_controls;
keep if _n==10|_n==20|_n==40;
outsheet using fisher_pvalues_buy.xls, replace;


mat PVAL1=J(40,7,0);
mat PVAL1[10,1]=10;
mat PVAL1[20,1]=20;
mat PVAL1[40,1]=40;

foreach price in 10 20 40 {;
	use working, clear;
	mat PVAL1[`price',2]=effec_cov`price'[1];
	collapse (sum) true, by( effec_cov`price') ;
	replace effec_cov`price'=-effec_cov`price' if effec_cov`price' <0;
	sort effec_cov`price';
	gen rank=_n if true==1;
	gen pval_`price'=1-rank/_N if true==1;
	mat PVAL1[`price',4]=_N-1;
	keep if true==1;
	mat PVAL1[`price',3]=pval_`price'[1];

	use working, clear;
	mat PVAL1[`price',5]=effec_cov`price'_controls[1];
	collapse (sum) true, by( effec_cov`price'_controls) ;
	replace effec_cov`price'_controls=-effec_cov`price'_controls if effec_cov`price'_controls <0;
	sort effec_cov`price'_controls;
	gen rank=_n if true==1;
	gen pval_`price'=1-rank/_N if true==1;
	mat PVAL1[`price',7]=_N-1;
	keep if true==1;
	mat PVAL1[`price',6]=pval_`price'[1];
	di `price';

};

svmat PVAL1;
keep PVAL11-PVAL17;
rename PVAL11 price;
rename PVAL12 coeff;
rename PVAL13 pval;
rename PVAL14 obs;
rename PVAL15 coeff_controls;
rename PVAL16 pval_controls;
rename PVAL17 obs_controls;
keep if _n==10|_n==20|_n==40;
outsheet using fisher_pvalues_cov.xls, replace;





*/;

********************************************************************;
************* TABLE 3: NET SALES FROM CLINIC REGISTERS *************;
********************************************************************;

#delimit;
use clinic_level_nets_sales, clear;
merge clinicid using clinic_level_baselinedata;
drop _merge;

gen netprice10=0;  replace netprice10=1 if netprice==10;
	gen netprice20=0;  replace netprice20=1 if netprice==20;
	gen netprice40=0;  replace netprice40=1 if netprice==40;
gen netpricesq=netprice*netprice;
	gen netpricecube=netpricesq*netprice;
	
global indepvar1=" netprice";
global indepvar2=" netprice10 netprice20 netprice40";
global indepvar3="   week";
global indepvar4=" avefirstvisits avetotvisits week anc_fee any_CT  distanceclosestclinic disttoclosestprogramclinic";

* CREATE BASELINE CONTROLS;
gen avefirstvisits=( firstvisits_02+ firstvisits_03+ firstvisits_04+ firstvisits_05+ firstvisits_06+ firstvisits_07+ firstvisits_08+ firstvisits_09)/8 ;
gen avetotvisits=(  tot_anc_02+ tot_anc_03+ tot_anc_04 +  tot_anc_05+ tot_anc_06+ tot_anc_07+ tot_anc_08+ tot_anc_09)/8 ;

* PROBLEM OF MISSING BASELINE DATA FOR CLINIC 23;
	egen avefirstvisits23=rmean(firstvisits_01 firstvisits_06) if clinicid==23;
		replace  avefirstvisits= avefirstvisits23 if clinicid==23;
	egen avetotvisits23=rmean(tot_anc_01-tot_anc_06) if clinicid==23;
		replace avetotvisits=avetotvisits23 if clinicid==23;

* MAKE TABLE!;
sum weeklynetsales if netprice==0;
	gen mean=r(mean);
areg weeklynetsales netprice, absorb(district)  cluster(clinicid);
	outreg using netsaletotals_table.xls,  se nonotes  noni bdec(4)  noaster addstat ("Mean of Dep. Var", mean)  replace;
areg weeklynetsales netprice ${indepvar3}, absorb(district)  cluster(clinicid);
	outreg using netsaletotals_table.xls,  se nonotes  noni bdec(4)  noaster addstat ("Mean of Dep. Var", mean)  append;
areg weeklynetsales netprice  ${indepvar4}, absorb(district)  cluster(clinicid);
	outreg using netsaletotals_table.xls,  se nonotes noni bdec(4)  noaster addstat ("Mean of Dep. Var", mean)  append;
areg weeklynetsales ${indepvar2}, absorb(district) cluster(clinicid);
	outreg using netsaletotals_table.xls,  se nonotes  noni bdec(4)  noaster  append;
areg weeklynetsales ${indepvar2} ${indepvar3}, absorb(district)  cluster(clinicid);
	outreg using netsaletotals_table.xls,  se nonotes  noni bdec(4)  noaster  append;
areg weeklynetsales ${indepvar2} ${indepvar4}, absorb(district)  cluster(clinicid);
	outreg using netsaletotals_table.xls,  se nonotes  noni bdec(4)  noaster  append;



**********************************;
****RAW DATA FOR APPENDIX 3;
**********************************;
preserve;
	collapse weeklynetsales, by(clinicid netprice);
	sort netprice clinicid;
	outsheet using TableApp3row1.out, replace;
restore;


*/;
**********************************************************;
************TABLE 4: NET SALES FROM INDIV SURVEY*********;
**********************************************************;

#delimit;
use all_data, clear;

drop if retroactive==1;

*price treatment dummies;
gen netprice10=0;  replace netprice10=1 if netprice==10;
gen netprice20=0;  replace netprice20=1 if netprice==20;
gen netprice40=0;  replace netprice40=1 if netprice==40;

**** intra-cluster correlation;
moultonDE PV_buynet clinicid ;
gen rho=r(rho);
gen DE=r(DE);


****all;
sum PV_buynet;
	gen mean=r(mean);
xi: quietly areg PV_buynet netprice, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_table.xls, se nonotes noni bdec(3)  noaster addstat ("Mean of Dep. Var", mean, "Moulton Intra-Cluster Correlation", rho, "Design Effect", DE) replace;
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_table.xls,  se nonotes noni bdec(3)  noaster addstat ("Mean of Dep. Var", mean) append;
xi: quietly areg PV_buynet i.netprice, absorb(district) cluster(clinicid);
	outreg _Inetprice_10 _Inetprice_20 _Inetprice_40 using netsalesindiv_table.xls,  se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;
xi: quietly areg PV_buynet i.netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic, absorb(district) cluster(clinicid);
	outreg _Inetprice_10 _Inetprice_20 _Inetprice_40  using netsalesindiv_table.xls, se nonotes noni bdec(3)  noaster addstat ("Mean of Dep. Var", mean) append;

****first visits;
drop mean;
sum PV_buynet if PV_firstvisit==1;
	gen mean=r(mean);
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_firstvisit==1, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_table.xls, se  nonotes noni bdec(3)  addstat ("Mean of Dep. Var", mean) noaster  append;

**** first pregnancy;
drop mean;
sum PV_buynet if PV_firstpreg==1;
	gen mean=r(mean);
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_firstpreg==1, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_table.xls, se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;

****those who didn't get free net previous year;
drop mean;
sum PV_buynet if PV_freenetlastyear==0;
	gen mean=r(mean);
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_freenetlastyear==0, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_table.xls,  se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;
xi: quietly areg PV_buynet i.netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_freenetlastyear==0, absorb(district) cluster(clinicid);
	outreg _Inetprice_10 _Inetprice_20 _Inetprice_40  using netsalesindiv_table.xls, se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;


*************************************;
****RAW DATA FOR APPENDIX 3;
*************************************;
preserve;
	collapse PV_buynet, by(clinicid netprice);
	sort netprice clinicid;
	outsheet using TableApp3row2.out, replace;
restore;



*********************************************************;
*************TABLE 5: USAGE*********; 
*********************************************************;
use all_data, clear;

keep if PV_buynet==1;

gen netprice10=0;  replace netprice10=1 if netprice==10;
gen netprice20=0;  replace netprice20=1 if netprice==20;
gen netprice40=0;  replace netprice40=1 if netprice==40;

egen monthly_firstvisits=rmean(firstvisits_04 firstvisits_05 firstvisits_06);
egen monthlytotvisits=rmean( tot_anc_04  tot_anc_05  tot_anc_06);

global indepvar1=" netprice";
global indepvar2=" netprice10 netprice20 netprice40";
global time_controls="FOL_gap PV_timesinceprogstart FOL_stillpreg";
global clinic_controls="any_CT monthly_firstvisits distanceclosestclinic disttoclosestprogramclinic";

*intrac-cluster correlation;
moultonDE FOL_inuse clinicid ;
gen rho=r(rho);
gen DE=r(DE);

**SELF-REPORTED USAGE;
sum FOL_inuse;
	gen mean=r(mean);
quietly areg FOL_inuse ${indepvar1}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using usage_table.xls,  se nonotes noni bdec(3) sigsymb(***,**,*)  addstat ("Mean of Dep. Var", mean, "Moulton Intra-Cluster Correlation", rho, "Design Effect", DE) replace;
quietly areg FOL_inuse ${indepvar1} ${time_controls} ${clinic_controls}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using usage_table.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  append;
quietly areg FOL_inuse ${indepvar2}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_table.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean;
quietly areg FOL_inuse ${indepvar2}  ${time_controls} ${clinic_controls}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_table.xls, noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;

**HANGING NET;
sum FOL_hanging;
	gen mean=r(mean);
quietly areg FOL_hanging ${indepvar1}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using usage_table.xls,  se nonotes noni bdec(3)  sigsymb(***,**,*)  addstat ("Mean of Dep. Var", mean) append;
quietly areg FOL_hanging ${indepvar2}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_table.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean;



**********************************;
****RAW DATA FOR APPENDIX 3;
**********************************;
gen obsusage=1 if FOL_inuse!=.;
collapse FOL_inuse (sum) obsusage, by(clinicid netprice);
sort netprice clinicid;
outsheet using TableApp3row3.out, replace;


*******************************************************;
******************TABLE 6: UNCONDITIONAL USAGE*********; 
*******************************************************;
#delimit;
use all_data, clear;

drop if retroactive==1;
gen effec_cov=FOL_inuse;
	replace effec_cov=0 if PV_buynet==0;
replace FOL_stillpreg=0 if PV_buynet==0;

gen netprice10=0;  replace netprice10=1 if netprice==10;
gen netprice20=0;  replace netprice20=1 if netprice==20;
gen netprice40=0;  replace netprice40=1 if netprice==40;

global indepvar1=" netprice";
global indepvar2=" netprice10 netprice20 netprice40";
global time_controls=" PV_timesinceprogstart FOL_stillpreg";
global clinic_controls="anc_fee any_CT monthly_firstvisits distanceclosestclinic disttoclosestprogramclinic";
egen monthly_firstvisits=rmean(firstvisits_04 firstvisits_05 firstvisits_06);

** intra-cluster correlation;
moultonDE FOL_inuse clinicid ;
gen rho=r(rho);
gen DE=r(DE);

sum effec_cov;
	gen mean=r(mean);
sum effec_cov if netprice==0;
	gen mean0=r(mean);
quietly areg effec_cov ${indepvar1}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using unc_usage_table.xls, se nonotes noni bdec(3) noaster  addstat ("Mean of Dep. Var", mean, "Moulton Intra-Cluster Correlation", rho, "Design Effect", DE, "Mean Free", mean0) replace;
quietly areg effec_cov ${indepvar1} ${time_controls} ${clinic_controls}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using unc_usage_table.xls,  noni bdec(3) se nonotes  noaster  append;
quietly areg effec_cov ${indepvar2}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using unc_usage_table.xls,  noni bdec(3) se nonotes  noaster  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean mean0;
quietly areg effec_cov ${indepvar2} ${time_controls} ${clinic_controls}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using unc_usage_table.xls,  noni bdec(3) se nonotes  noaster  addstat("Joint Test", r(F), "Prob >F", r(p) )append;



**************************************************;
******** TABLE 7 - SUNK COST EFFECTS? *******;
**************************************************;
#delimit;
use all_data, clear;

keep if lottery==1;

gen free=0;
	replace free=1 if netprice==0|transactionprice<1;
gen notfree=1-free;


global indepvar0="FOL_stillpregnant PV_firstvisit PV_firstpreg PV_timetoclinic FOL_gap";
global indepvar1="PV_freenetlastyear";
global indepvar2="transactionprice";
global indepvar3="notfree";

sum FOL_inuse; 
gen mean=r(mean);

** SELF-REPORTED USAGE;
quietly areg FOL_inuse ${indepvar2} ${indepvar3} ${indepvar1} ${indepvar0}, absorb(clinicid);
	testparm ${indepvar2} ${indepvar3} ${indepvar1} ${indepvar0};
	outreg using expost.xls, se nonote  sigsym(***,**,*)  addstat("Sample Mean of Dep. Var", mean, "F Stat", r(F), "Prob >F", r(p) ) replace;
quietly areg FOL_inuse ${indepvar2}, absorb(clinicid);
	outreg using expost.xls,  se nonote  noaster append;
quietly areg FOL_inuse ${indepvar2} ${indepvar1} ${indepvar0}, absorb(clinicid);
	testparm ${indepvar2}  ${indepvar1} ${indepvar0};
	outreg using expost.xls,  se nonote  sigsym(***,**,*)  addstat("F Stat", r(F), "Prob >F", r(p)) append;
quietly areg FOL_inuse ${indepvar3} if lottery==1, absorb(clinicid);
	outreg using expost.xls,  se nonote  sigsym(***,**,*) append;
quietly areg FOL_inuse ${indepvar3} ${indepvar0} if lottery==1, absorb(clinicid);
	testparm ${indepvar2} ${indepvar0};
	outreg using expost.xls,  se nonote  sigsym(***,**,*)  addstat("F Stat", r(F), "Prob >F", r(p)) append;
quietly areg FOL_inuse ${indepvar3} ${indepvar1} ${indepvar0} if lottery==1, absorb(clinicid);
	testparm ${indepvar2}  ${indepvar1} ${indepvar0};
	outreg using expost.xls,  se nonote  sigsym(***,**,*)  addstat("F Stat", r(F), "Prob >F", r(p)) append;


**HANGING NET;
drop mean;
sum FOL_hanging;
gen mean=r(mean);
quietly areg FOL_hanging ${indepvar3} ${indepvar1} ${indepvar0} if lottery==1, absorb(district);
	quietly testparm ${indepvar3} ${indepvar1} ${indepvar0}; 
	outreg ${indepvar3} ${indepvar1} ${indepvar0} using expost.xls, noni bdec(3) se nonotes  sigsymb(***,**,*) addstat("F Stat", r(F), "Prob >F", r(p) )append;
drop mean;


******************************************;
***** TABLE 8 **********;
******************************************;
use all_data, clear;

drop if retroactive==1;

keep if treatment==0|PV_buynet==1;

local vars 9;
for any
PV_firstvisit  PV_walktoclinic PV_costtoclinic  
PV_readkis PV_shoes PV_c1_own_animals PV_hbrate 
PV_anemic PV_anemic2 
\ num 1/`vars': global varY="X";

local i 1;
while `i'<=`vars' {;
	sum ${var`i'} if treatment==0;
	gen mean=r(mean);
	gen sd=r(sd);
	quietly reg ${var`i'}  if treatment==0, cluster(clinicid);
		outreg using sumdiff`i'.xls, nor2 se nonote  noaster  addstat("Mean", mean, "SD", sd) replace;
	quietly reg ${var`i'} treatment if netprice==0|treatment==0, cluster(clinicid);
		outreg using sumdiff`i'.xls, nor2 se nonote  noaster  append;
	quietly reg ${var`i'} treatment if netprice==10|treatment==0, cluster(clinicid);
		outreg using sumdiff`i'.xls, nor2 se nonote  noaster  append;
	quietly reg ${var`i'} treatment if netprice==20|treatment==0, cluster(clinicid);
		outreg using sumdiff`i'.xls, nor2 se nonote   noaster  append;
	quietly reg ${var`i'} treatment if netprice==40|treatment==0, cluster(clinicid);
		outreg using sumdiff`i'.xls, nor2 se nonote   noaster  append;
		drop mean sd;
	local i=`i'+1;

};




*******************************************************************;
******************************** FIGURES ***************************;
*******************************************************************;

******************************;
*******FIGURE 1;
******************************;
use all_data, clear;

drop if retroactive==1;

tab FOL_inuse if PV_buynet==0;
gen effec_cov=FOL_inuse;
	replace effec_cov=0 if PV_buynet==0;

gen coef=.;
gen upper=.;
gen lower=.;
gen coef_2=.;
gen upper_2=.;
gen lower_2=.;
gen n=_n;

gen free=0;
replace free=1 if netprice==0;
gen p10=0;
replace p10=1 if netprice==10;
gen p20=0;
replace p20=1 if netprice==20;
gen p40=0;
replace p40=1 if netprice==40;

	reg effec_cov free p10 p20 p40,  nocons;
	mat beta=e(b);
	mat var=e(V);
	for num 1/4:
	 replace coef=beta[1,X] if _n==X \
	 replace upper=coef+2.14*var[X, X]^.5 if _n==X \
	 replace lower=coef-2.14*var[X, X]^.5 if _n==X;	 

	reg PV_buynet free p10 p20 p40,  nocons;
	mat beta2=e(b);
	mat var2=e(V);
	for num 1/4:
	 replace coef_2=beta2[1,X] if _n==X \
	 replace upper_2=coef_2+2.14*var2[X, X]^.5 if _n==X \
	 replace lower_2=coef_2-2.14*var2[X, X]^.5 if _n==X;	 

	graph twoway
	(scatter coef_2 n if _n<=4, connect(l) msymbol(T) lcolor(black) xlabel(1 "FREE" 2 "10Ksh" 3 "20Ksh" 4 "40Ksh"	 ))
	 (rcap lower_2 upper_2 n if _n<=4, msymbol(i) lcolor(black)) 
	 (scatter coef n if _n<=4, connect(l) lpattern(dash) lcolor(black) xlabel(1 "FREE" 2 "10Ksh" 3 "20Ksh" 4 "40Ksh"	 ))
	 (rcap lower upper n if _n<=4, msymbol(i) lcolor(black)), 
	 legend(label(2 "95% CI") label( 1 "Acquired ITN") label(3 "Acquired ITN and Using it") label(4 "95% CI") cols(2) ) note(" ") 
	 xtitle("Price of ITN")
	 saving(FINALfigure1, replace);




********************************;
*****FIGURE 2 ;
********************************;

use all_data, clear;

*PANEL A: DECLARED USAGE;
gen coef=.;
gen upper=.;
gen lower=.;
gen n=_n;


gen free=0;
replace free=1 if netprice==0;
gen p10=0;
replace p10=1 if netprice==10;
gen p20=0;
replace p20=1 if netprice==20;
gen p40=0;
replace p40=1 if netprice==40;

	reg FOL_inuse free p10 p20 p40,  nocons ;
	mat beta=e(b);
	mat var=e(V);

	for num 1/4:
	 replace coef=beta[1,X] if _n==X \
	 replace upper=coef+2.14*var[X, X]^.5 if _n==X \
	 replace lower=coef-2.14*var[X, X]^.5 if _n==X;	 

	graph twoway (scatter coef n if _n<=4, connect(l) lcolor(black) ylabel(0 (0.2) 1) xlabel(1 "FREE" 2 "10Ksh" 3 "20Ksh" 4 "40Ksh" ))
	 (rcap lower upper n if _n<=4, msymbol(i) lcolor(black)), title("Declare using ITN", size(medium)) 
	 legend(order(1 "Average" 2 "95% CI") rows(1)) note(" ") xtitle("ITN Price ")
	 saving(fig2a1, replace);


* PANEL B(HANGING);
replace coef=.;
replace upper=.;
replace lower=.;

	reg FOL_hanging free p10 p20 p40,  nocons;
	mat beta=e(b);
	mat var=e(V);

	for num 1/4:
	 replace coef=beta[1,X] if _n==X \
	 replace upper=coef+2.14*var[X, X]^.5 if _n==X \
	 replace lower=coef-2.14*var[X, X]^.5 if _n==X;	 

	graph twoway (scatter coef n if _n<=4, connect(l) lcolor(black) ylabel(0 (0.2) 1) xlabel(1 "FREE" 2 "10Ksh" 3 "20Ksh" 4 "40Ksh"
	 ))
	 (rcap lower upper n if _n<=4, msymbol(i) lcolor(black)), title("ITN seen visibly hanging", size(medium)) 
	 legend(order(1 "Average" 2 "95% CI") rows(1)) note(" ") xtitle("ITN Price")
	 saving(fig2b1, replace);

graph combine fig2a1.gph fig2b1.gph, col(1) saving(FINALfigure2, replace) xsize(4) ysize(6);


*****************************;
***** FIGURE 3;
*****************************;
#delimit;
use all_data, clear;

cumul PV_hbrecent if treatment==0, gen(cdf_control);
cumul PV_hbrecent if netprice==0, gen(cdf_0);
cumul PV_hbrecent if netprice==10, gen(cdf_10);
cumul PV_hbrecent if netprice==20, gen(cdf_20);
cumul PV_hbrecent if netprice==40, gen(cdf_40);

twoway (scatter cdf_control PV_hbrecent, mfcolor(white) mcolor(black) sort(cdf_control)) ||
(scatter cdf_0 PV_hbrecent, msymbol(x) mcolor(black) sort(cdf_0)),
saving(fig4a, replace) 
legend(order(1 "Control" 2 "Free Net")) xlab(5(5)15)  xtitle("Hemoglobin Level (g/dL)")
title("A: Clients at Control Clinics vs. Clients Receing Free Net", size(medium));

twoway (scatter cdf_control PV_hbrecent, mfcolor(white) mcolor(black) sort(cdf_control)) ||
(scatter cdf_10 PV_hbrecent, msymbol(x)mcolor(black) sort(cdf_0)),
saving(fig4b, replace) 
legend(order(1 "Control" 2 "10Ksh Net")) xlab(5(5)15)  xtitle("Hemoglobin Level (g/dL)")
title("B: Clients at Control Clinics vs. Clients Buying 10Ksh Net", size(medium));

twoway (scatter cdf_control PV_hbrecent, mfcolor(white) mcolor(black) sort(cdf_control)) ||
(scatter cdf_20 PV_hbrecent, msymbol(x) mcolor(black) sort(cdf_0)),
saving(fig4c, replace) 
legend(order(1 "Control" 2 "20Ksh Net")) xlab(5(5)15)  xtitle("Hemoglobin Level (g/dL)")
title("C: Clients at Control Clinics vs. Clients Buying 20Ksh Net", size(medium));

twoway (scatter cdf_control PV_hbrecent, mfcolor(white) mcolor(black) sort(cdf_control)) ||
(scatter cdf_40 PV_hbrecent, msymbol(x) sort(cdf_0) mcolor(black)),
saving(fig4d, replace) 
legend(order(1 "Control" 2 "40Ksh Net")) xlab(5(5)15)  xtitle("Hemoglobin Level (g/dL)")
title("D: Clients at Control Clinics vs. Clients Buying 40Ksh Net", size(medium));

graph combine fig4a.gph fig4b.gph fig4c.gph fig4d.gph,col(2) saving(FINALfigure3, replace) xsize(8) ysize(4) xcommon ycommon;


************************;
*K-S tests;
************************;
#delimit;
global bootreps=1000;
set matsize $bootreps;

foreach price in 0 10 20 40 {;
	use all_data, clear;
	drop if retroactive==1;
	replace netprice=50 if netprice==.;

	mat KS`price'=J(${bootreps},1,0);

	keep if netprice==50|netprice==`price';
	ksmirnov PV_hbrate, by(netprice); 
	mat KS`price'[1,1]=r(D);
	sort clinicid;
	gen obs=1 if clinicid!=clinicid[_n+1];
	egen n_clusters=sum(obs);
	save temp, replace;

	local i 2;
	while `i'<=$bootreps { ;
		use temp, clear;	
		bsample n_clusters, cluster(clinicid) idcluster(idcluster);
		egen clust=group(idcluster);
		gen randomnumber=uniform();
		bys idcluster: egen minrandom=min(randomnumber);
		sort idcluster;
		replace minrandom=. if idcluster==idcluster[_n-1];
		sort minrandom;
		gen randomrank=_n if minrandom!=.;
		bys idcluster: egen randomrankall=mean(randomrank);		
		gen randomnetprice=50;
		replace randomnetprice=`price' if randomrankall<(n_clusters-5);
		ksmirnov PV_hbrate, by(randomnetprice); 
		mat KS`price'[`i',1]=r(D);
		local i=`i'+1;
		
	};
	svmat KS`price';
	keep  KS`price'1;
	rename KS`price'1 stat;
	drop if _n>$bootreps;
		
	gen true=0;
	replace true=1 if _n==1;
	gen obs=1;
	sort stat;
	gen rank=_n if true==1;
	gen pval_`price'=1-rank/_N if true==1;
	drop if true==0;
	outsheet using pval`price'.xls, replace;
};



********************************************************************************;
********************************************************************************;
*****************               APPENDIX TABLES                 ****************;
********************************************************************************;
********************************************************************************;


*************************************;
**************APPENDIX TABLE A1;
*************************************;
#delimit;
use all_data, clear;

drop if retroactive==1;
keep if PV_same_clinic==1;

gen netprice10=0;  replace netprice10=1 if netprice==10;
gen netprice20=0;  replace netprice20=1 if netprice==20;
gen netprice40=0;  replace netprice40=1 if netprice==40;

*** intra-cluster correlation;
moultonDE PV_buynet clinicid ;
gen rho=r(rho);
gen DE=r(DE);

****all;
sum PV_buynet;
gen mean=r(mean);
xi: quietly areg PV_buynet netprice, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_tableAN.xls, se nonotes noni bdec(3)  noaster addstat ("Mean of Dep. Var", mean, "Moulton Intra-Cluster Correlation", rho, "Design Effect", DE) replace;
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_tableAN.xls,  se nonotes noni bdec(3)  noaster addstat ("Mean of Dep. Var", mean) append;
xi: quietly areg PV_buynet i.netprice, absorb(district) cluster(clinicid);
	outreg _Inetprice_10 _Inetprice_20 _Inetprice_40 using netsalesindiv_tableAN.xls,  se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;
xi: quietly areg PV_buynet i.netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic, absorb(district) cluster(clinicid);
	outreg _Inetprice_10 _Inetprice_20 _Inetprice_40  using netsalesindiv_tableAN.xls, se nonotes noni bdec(3)  noaster addstat ("Mean of Dep. Var", mean) append;

****first visits;
drop mean;
sum PV_buynet if PV_firstvisit==1;
gen mean=r(mean);
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_firstvisit==1, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_tableAN.xls, se  nonotes noni bdec(3)  addstat ("Mean of Dep. Var", mean) noaster  append;

**** first pregnancy;
drop mean;
sum PV_buynet if PV_firstpreg==1;
gen mean=r(mean);
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_firstpreg==1, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_tableAN.xls, se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;

****those who didn't get free net previous year;
drop mean;
sum PV_buynet if PV_freenetlastyear==0;
gen mean=r(mean);
xi: quietly areg PV_buynet netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_freenetlastyear==0, absorb(district) cluster(clinicid);
	outreg netprice using netsalesindiv_tableAN.xls,  se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;
xi: quietly areg PV_buynet i.netprice i.PV_dayofweek PV_timesinceprogstart anc_fee any_CT firstvisits_04 firstvisits_05 firstvisits_06 distanceclosestclinic disttoclosestprogramclinic if PV_freenetlastyear==0, absorb(district) cluster(clinicid);
	outreg _Inetprice_10 _Inetprice_20 _Inetprice_40  using netsalesindiv_tableAN.xls, se nonotes noni bdec(3) addstat ("Mean of Dep. Var", mean) noaster  append;




*******************************************************************;
*******ANNEX TABLE A2: USAGE, VISITS TO SAME CLINIC ONLY*********; 
*******************************************************************;
#delimit;
use all_data, clear;

keep if PV_buynet==1&PV_same_clinic==1;

egen monthly_firstvisits=rmean(firstvisits_04 firstvisits_05 firstvisits_06);
egen monthlytotvisits=rmean( tot_anc_04  tot_anc_05  tot_anc_06);

gen netprice10=0;  replace netprice10=1 if netprice==10;
gen netprice20=0;  replace netprice20=1 if netprice==20;
gen netprice40=0;  replace netprice40=1 if netprice==40;

global indepvar1=" netprice";
global indepvar2=" netprice10 netprice20 netprice40";
global time_controls="FOL_gap PV_timesinceprogstart FOL_stillpreg";
global clinic_controls="any_CT monthly_firstvisits distanceclosestclinic disttoclosestprogramclinic";

moultonDE FOL_inuse clinicid ;
gen rho=r(rho);
gen DE=r(DE);

*SELF-REPORTED USING;
***all;
sum FOL_inuse;
	gen mean=r(mean);
quietly areg FOL_inuse ${indepvar1}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using usage_tableAN.xls,  se nonotes noni bdec(3) sigsymb(***,**,*)  addstat ("Mean of Dep. Var", mean, "Moulton Intra-Cluster Correlation", rho, "Design Effect", DE) replace;
quietly areg FOL_inuse ${indepvar1} ${time_controls} ${clinic_controls}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using usage_tableAN.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  append;
quietly areg FOL_inuse ${indepvar2}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_tableAN.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean;

***first visits only;
sum FOL_inuse if PV_firstvisit==1;
	gen mean=r(mean);
quietly areg FOL_inuse ${indepvar1}, absorb(district) cluster(clinicid), if PV_firstvisit==1;
	outreg ${indepvar1} using usage_tableAN.xls,  se nonotes noni bdec(3)  sigsymb(***,**,*)  addstat ("Mean of Dep. Var", mean) append;
quietly areg FOL_inuse ${indepvar2}, absorb(district) cluster(clinicid), if PV_firstvisit==1;
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_tableAN.xls,  noni bdec(3) se nonotes sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean;

***first preg only;
sum FOL_inuse if PV_firstpreg==1;
	gen mean=r(mean);
quietly areg FOL_inuse ${indepvar1}, absorb(district) cluster(clinicid), if PV_firstpreg==1;
	outreg ${indepvar1} using usage_tableAN.xls,  se nonotes noni bdec(3)  sigsymb(***,**,*)  addstat ("Mean of Dep. Var", mean) append;
quietly areg FOL_inuse ${indepvar2}, absorb(district) cluster(clinicid), if PV_firstpreg==1;
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_tableAN.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean;


*** those who didn't get a free one last year;
sum FOL_inuse if PV_freenetlastyear==0;
	gen mean=r(mean);
quietly areg FOL_inuse ${indepvar1}, absorb(district) cluster(clinicid), if PV_freenetlastyear==0;
	outreg ${indepvar1} using usage_tableAN.xls,  se nonotes noni bdec(3)  sigsymb(***,**,*)  addstat ("Mean of Dep. Var", mean) append;
quietly areg FOL_inuse ${indepvar2}, absorb(district) cluster(clinicid), if PV_freenetlastyear==0;
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_tableAN.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean;

**HANGING NET;
sum FOL_hanging;
	gen mean=r(mean);
quietly areg FOL_hanging ${indepvar1}, absorb(district) cluster(clinicid);
	outreg ${indepvar1} using usage_tableAN.xls,  se nonotes noni bdec(3)  sigsymb(***,**,*)  addstat ("Mean of Dep. Var", mean) append;
quietly areg FOL_hanging ${indepvar2}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_tableAN.xls,  noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;
drop mean;

*add a column controling for stuff with price dummies;
quietly areg FOL_inuse ${indepvar2}  ${time_controls} ${clinic_controls}, absorb(district) cluster(clinicid);
	quietly testparm ${indepvar2}; 
	outreg ${indepvar2} using usage_tableAN.xls, noni bdec(3) se nonotes  sigsymb(***,**,*)  addstat("Joint Test", r(F), "Prob >F", r(p) )append;


***********************************************;
***** ANNEX TABLE 3 ;
***********************************************;
#delimit;

use all_data, clear;

keep if treatment==0|PV_buynet==1;
drop if retroactive==1;
keep if PV_same_clinic==1;

local vars 9;
for any
PV_firstvisit  PV_walktoclinic PV_costtoclinic  
PV_readkis PV_shoes PV_c1_own_animals PV_hbrate 
PV_anemic PV_anemic2 
\ num 1/`vars': global varY="X";

local i 1;
while `i'<=`vars' {;
	sum ${var`i'} if treatment==0;
	gen mean=r(mean);
	gen sd=r(sd);
	quietly reg ${var`i'}  if treatment==0, cluster(clinicid);
		outreg using sumdiffAN`i'.xls, nor2 se nonote  noaster  addstat("Mean", mean, "SD", sd) replace;
	quietly reg ${var`i'} treatment if netprice==0|treatment==0, cluster(clinicid);
		outreg using sumdiffAN`i'.xls, nor2 se nonote  noaster  append;
	quietly reg ${var`i'} treatment if netprice==10|treatment==0, cluster(clinicid);
		outreg using sumdiffAN`i'.xls, nor2 se nonote  noaster  append;
	quietly reg ${var`i'} treatment if netprice==20|treatment==0, cluster(clinicid);
		outreg using sumdiffAN`i'.xls, nor2 se nonote   noaster  append;
	quietly reg ${var`i'} treatment if netprice==40|treatment==0, cluster(clinicid);
		outreg using sumdiffAN`i'.xls, nor2 se nonote   noaster  append;
	drop mean sd;
	local i=`i'+1;

};
