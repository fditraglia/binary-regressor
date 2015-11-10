/***
*** Note that this program was updated on 6/15/2015 to correct an error in the previously provided code
***/

version 10.0
# delimit;
clear all;
set more off;
set mem 200m;
capture log close;
log using Tables.log, replace text;

/*****
*** Global Variables
*****/

*Controls from the fall 2007 survey;
global f07_child_controls "f07_heads_child_cnt f07_girl_cnt f07_age_cnt";

global f07_hh_controls "f07_duration_village_cnt f07_farsi_cnt f07_tajik_cnt f07_farmer_cnt f07_age_head_cnt 
   f07_yrs_ed_head_cnt f07_num_ppl_hh_cnt f07_jeribs_cnt f07_num_sheep_cnt f07_nearest_scl"; 

global f07_hh_controls_nodist "f07_duration_village_cnt f07_farsi_cnt f07_tajik_cnt f07_farmer_cnt f07_age_head_cnt 
   f07_yrs_ed_head_cnt f07_num_ppl_hh_cnt f07_jeribs_cnt f07_num_sheep_cnt"; 

*Controls from the spring 2008 survey;
global s08_child_controls "s08_heads_child_cnt s08_girls_cnt s08_age_cnt";

global s08_hh_controls "s08_duration_village_cnt s08_farsi_cnt s08_tajik_cnt s08_farmer_cnt s08_age_head_cnt 
   s08_yrs_ed_head_cnt s08_num_ppl_hh_cnt s08_jeribs_cnt s08_num_sheep_cnt s08_nearest_scl"; 

global s08_hh_controls_nodist "s08_duration_village_cnt s08_farsi_cnt s08_tajik_cnt s08_farmer_cnt s08_age_head_cnt 
   s08_yrs_ed_head_cnt s08_num_ppl_hh_cnt s08_jeribs_cnt s08_num_sheep_cnt"; 

*Format for all numeric output;
global strformat "%8.3f";
   
/*****
*** Programs
*****/

*Format Output for Tables;
capture program drop sig_p;
program sig_p;
   args obj_var point_est_var point_sd_var p_val row_id_var row_num;
   local point_est `point_est_var';
   local point_sd `point_sd_var';
   if `p_val' > 0.1 {;
      replace `obj_var' = string(`point_est', "$strformat") if `row_id_var' == `row_num';
      };
   if `p_val' > 0.05 & `p_val' <= 0.1  {;
      replace `obj_var' = string(`point_est', "$strformat")+"*" if `row_id_var' == `row_num';
      };
   if `p_val' > 0.01 & `p_val' <= 0.05  {;
      replace `obj_var' = string(`point_est', "$strformat")+"**" if `row_id_var' == `row_num';
      };
   if `p_val' <= 0.01 {;
      replace `obj_var' = string(`point_est', "$strformat")+"***" if `row_id_var' == `row_num';
      };
   replace `obj_var' = "<" + string(`point_sd', "$strformat") + ">" + " " if `row_id_var' == `row_num' + 1;
   end;


/*****
*** Table 1a and b: Household Level
*** Note: we do not remove outliers because we cannot remove outliers from non-surveyed households
*** Also, the standard errors calculated here for columns 3 and 7 in rows three and five are slightly
*** different from those presented in the paper. However, they are practically the same. (6/15/2015)
*****/

use "afghanistan_anonymized_HH_data", clear;

local row = 2;
forvalues var = 0(1)25 {;
   gen col`var' = "";
   };
gen row_num = _n;

replace col0 = "Panel A. Households surveyed" if row_num == `row';
local row = `row' + 1;
*HH's covered;
su f07_hh_observed if f07_hh_covered == 1 & treatment == 1;
replace col1 = string(r(N)) if row_num == `row';
su f07_hh_observed if f07_hh_covered == 1 & treatment == 0;
replace col2 = string(r(N)) if row_num == `row';
replace col3 = string(real(col1) - real(col2)) if row_num == `row';
replace col4 = string(real(col1) + real(col2)) if row_num == `row';

su s08_hh_observed if s08_hh_covered == 1 & treatment == 1;
replace col5 = string(r(N)) if row_num == `row';
su s08_hh_observed if s08_hh_covered == 1 & treatment == 0;
replace col6 = string(r(N)) if row_num == `row';
replace col7 = string(real(col5) - real(col6)) if row_num == `row';
replace col8 = string(real(col5) + real(col6)) if row_num == `row';
replace col0="Identified" if row_num ==`row';
local row = `row' + 2;

*Tested Children;
su f07_hh_observed if f07_hh_observed == 1 & treatment == 1;
replace col1 = string(r(N)) if row_num == `row';
su f07_hh_observed if f07_hh_observed == 1 & treatment == 0;
replace col2 = string(r(N)) if row_num == `row';
replace col3 = string(real(col1) - real(col2)) if row_num == `row';
replace col4 = string(real(col1) + real(col2)) if row_num == `row';

su s08_hh_observed if s08_hh_observed == 1 & treatment == 1;
replace col5 = string(r(N)) if row_num == `row';
su s08_hh_observed if s08_hh_observed == 1 & treatment == 0;
replace col6 = string(r(N)) if row_num == `row';
replace col7 = string(real(col5) - real(col6)) if row_num == `row';
replace col8 = string(real(col5) + real(col6)) if row_num == `row';
replace col0="Surveyed" if row_num == `row';
local row = `row' + 2;

*Percentage Tested;
su f07_hh_observed if f07_hh_covered == 1 & treatment == 1;
replace col1 = string(r(mean), "$strformat") if row_num == `row';
su f07_hh_observed if f07_hh_covered == 1 & treatment == 0;
replace col2 = string(r(mean), "$strformat") if row_num == `row';
reg f07_hh_observed treatment if f07_hh_covered == 1, cluster(clustercode);
test treatment;
sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  
su f07_hh_observed if f07_hh_covered == 1;
replace col4 = string(r(mean), "$strformat") if row_num == `row';

su s08_hh_observed if s08_hh_covered == 1 & treatment == 1;
replace col5 = string(r(mean), "$strformat") if row_num == `row';
su s08_hh_observed if s08_hh_covered == 1 & treatment == 0;
replace col6 = string(r(mean), "$strformat") if row_num == `row';
reg s08_hh_observed treatment if s08_hh_covered == 1, cluster(clustercode);
test treatment;
sig_p col7 _b[treatment] _se[treatment] r(p) row_num `row';  
su s08_hh_observed if s08_hh_covered == 1;
replace col8 = string(r(mean), "$strformat") if row_num == `row';
replace col0="Percent of households surveyed" if row_num == `row';
local row = `row' + 2;

*HH w/ Children;
replace col0="Panel B. Households with eligible children" if row_num == `row';
local row=`row' + 1;
su f07_with_kids if f07_with_kids == 1 & treatment == 1;
replace col1 = string(r(N)) if row_num == `row';
su f07_with_kids if f07_with_kids == 1 & treatment == 0;
replace col2 = string(r(N)) if row_num == `row';
replace col3 = string(real(col1) - real(col2)) if row_num == `row';
replace col4 = string(real(col1) + real(col2)) if row_num == `row';

su s08_with_kids if s08_with_kids == 1 & treatment == 1;
replace col5 = string(r(N)) if row_num == `row';
su s08_with_kids if s08_with_kids == 1 & treatment == 0;
replace col6 = string(r(N)) if row_num == `row';
replace col7 = string(real(col5) - real(col6)) if row_num == `row';
replace col8 = string(real(col5) + real(col6)) if row_num == `row';
replace col0="Households with children" if row_num == `row';
local row = `row' + 2;

*Percentage w/ Children;
su f07_with_kids if f07_hh_observed == 1 & treatment == 1;
replace col1 = string(r(mean), "$strformat") if row_num == `row';
su f07_with_kids if f07_hh_observed == 1 & treatment == 0;
replace col2 = string(r(mean), "$strformat") if row_num == `row';
reg f07_with_kids treatment if f07_hh_observed == 1, cluster(clustercode);
test treatment;
sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  
su f07_with_kids if f07_hh_observed == 1;
replace col4 = string(r(mean), "$strformat") if row_num == `row';

su s08_with_kids if s08_hh_observed == 1 & treatment == 1;
replace col5 = string(r(mean), "$strformat") if row_num == `row';
su s08_with_kids if s08_hh_observed == 1 & treatment == 0;
replace col6 = string(r(mean), "$strformat") if row_num == `row';
reg s08_with_kids treatment if s08_hh_observed == 1, cluster(clustercode);
test treatment;
sig_p col7 _b[treatment] _se[treatment] r(p) row_num `row';  
su s08_with_kids if s08_hh_observed == 1;
replace col8 = string(r(mean), "$strformat") if row_num == `row';
replace col0 = "Percentage with children" if row_num == `row';
local row = `row' + 2;

replace col1="Treatment Group - Fall 2007 Survey" if _n==1;
replace col2="Control Group - Fall 2007 Survey" if _n==1;
replace col3="Estimated Difference" if _n==1;
replace col4="Total" if _n==1;
replace col5="Treatment Group - Spring 2008 Survey" if _n==1;
replace col6="Control Group - Spring 2008 Survey" if _n==1;
replace col7="Estimated Difference" if _n==1;
replace col8="Total" if _n==1;

outsheet col* using "Table01_Panel_A_B.csv" if row_num < `row', comma replace;
drop col*;
 
   
/*****
*** Load Data Child Level
*****/

use afghanistan_anonymized_data, clear;

/****
*** Create Secondary Variables
****/

*Attrition Variables;
gen f07_attrit = f07_observed == 1 & s08_observed == 0;
gen f07_test_attrit = f07_test_observed == 1 & s08_test_observed == 0;
gen f07_attrit_treat = f07_attrit * treatment;
gen f07_test_attrit_treat = f07_test_attrit * treatment;
gen f07_test_observed_treat = f07_test_observed * treatment;
gen s08_test_observed_treat = s08_test_observed * treatment;

*Outlier Identification;
gen nonoutlier = 1;
replace nonoutlier = 0 if f07_num_ppl_hh_cnt > 20 & f07_observed == 1;
replace nonoutlier = 0 if f07_jeribs_cnt > 10 & f07_observed == 1;
replace nonoutlier = 0 if f07_num_sheep_cnt > 50 & f07_observed == 1;
replace nonoutlier = 0 if s08_num_ppl_hh_cnt > 20 & s08_observed == 1;
replace nonoutlier = 0 if s08_jeribs_cnt > 10 & s08_observed == 1;
replace nonoutlier = 0 if s08_num_sheep_cnt > 50 & s08_observed == 1;

/*****
*** Table 1c: Coverage Rates Child Level
*****/

local row = 2;
forvalues var = 0(1)25 {;
   gen col`var' = "";
   };
gen row_num = _n;
   
*Surveyed Children;
replace col0 = "Panel C. Children Tested" if row_num == `row';
local row = `row' + 1;
su f07_observed if nonoutlier == 1 & f07_observed == 1 & treatment == 1;
replace col1 = string(r(N)) if row_num == `row';
su f07_observed if nonoutlier == 1 &  f07_observed == 1 & treatment == 0;
replace col2 = string(r(N)) if row_num == `row';
replace col3 = string(real(col1) - real(col2)) if row_num == `row';
replace col4 = string(real(col1) + real(col2)) if row_num == `row';

su s08_observed if nonoutlier == 1 & s08_observed == 1 & treatment == 1;
replace col5 = string(r(N)) if row_num == `row';
su s08_observed if nonoutlier == 1 & s08_observed == 1 & treatment == 0;
replace col6 = string(r(N)) if row_num == `row';
replace col7 = string(real(col5) - real(col6)) if row_num == `row';
replace col8 = string(real(col5) + real(col6)) if row_num == `row';
replace col0="Identified" if row_num==`row';
local row = `row' + 2;

*Tested Children;
su f07_observed if nonoutlier == 1 &  f07_test_observed == 1 & treatment == 1;
replace col1 = string(r(N)) if row_num == `row';
su f07_observed if nonoutlier == 1 &  f07_test_observed == 1 & treatment == 0;
replace col2 = string(r(N)) if row_num == `row';
replace col3 = string(real(col1) - real(col2)) if row_num == `row';
replace col4 = string(real(col1) + real(col2)) if row_num == `row';

su s08_observed if nonoutlier == 1 & s08_test_observed == 1 & treatment == 1;
replace col5 = string(r(N)) if row_num == `row';
su s08_observed if nonoutlier == 1 & s08_test_observed == 1 & treatment == 0;
replace col6 = string(r(N)) if row_num == `row';
replace col7 = string(real(col5) - real(col6)) if row_num == `row';
replace col8 = string(real(col5) + real(col6)) if row_num == `row';
replace col0="Tested" if row_num==`row';
local row = `row' + 2;

*Percentage Tested;
su f07_test_observed if nonoutlier == 1 &  f07_observed == 1 & treatment == 1;
replace col1 = string(r(mean), "$strformat") if row_num == `row';
su f07_test_observed if nonoutlier == 1 &  f07_observed == 1 & treatment == 0;
replace col2 = string(r(mean), "$strformat") if row_num == `row';
reg f07_test_observed treatment if nonoutlier == 1 &  f07_observed == 1, cluster(clustercode);
test treatment;
sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  
su f07_test_observed if nonoutlier == 1 &  f07_observed == 1;
replace col4 = string(r(mean), "$strformat") if row_num == `row';

su s08_test_observed if nonoutlier == 1 & s08_observed == 1 & treatment == 1;
replace col5 = string(r(mean), "$strformat") if row_num == `row';
su s08_test_observed if nonoutlier == 1 & s08_observed == 1 & treatment == 0;
replace col6 = string(r(mean), "$strformat") if row_num == `row';
reg s08_test_observed treatment if nonoutlier == 1 & s08_observed == 1, cluster(clustercode);
test treatment;
sig_p col7 _b[treatment] _se[treatment] r(p) row_num `row';  
su s08_test_observed if nonoutlier == 1 & s08_observed == 1;
replace col8 = string(r(mean), "$strformat") if row_num == `row';
replace col0="Percent of children tested" if row_num == `row';
local row = `row' + 2;

replace col1="Treatment Group - Fall 2007 Survey" if _n==1;
replace col2="Control Group - Fall 2007 Survey" if _n==1;
replace col3="Estimated Difference" if _n==1;
replace col4="Total" if _n==1;
replace col5="Treatment Group - Spring 2008 Survey" if _n==1;
replace col6="Control Group - Spring 2008 Survey" if _n==1;
replace col7="Estimated Difference" if _n==1;
replace col8="Total" if _n==1;

aorder;
sort row_num;
outsheet col* using "Table01_Panel_C.csv" if row_num < `row', comma replace;
drop col*;

/*****
*** Table 2: Baseline Comparison
*****/

local row = 4;

forvalues var = 0(1)11 {;
   gen col`var' = "";
   };
   
replace col0="Panel A. Child-level variables" if _n==3;
replace col0="Panel B. Household level variables" if _n==10;

*Child Level;
foreach var of varlist $f07_child_controls {;
  replace col0 = "`var'" if row_num == `row';

  reg `var' if nonoutlier == 1 & f07_observed == 1 & treatment == 1;
  replace col1 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & f07_observed == 1 & treatment == 0;
  replace col2 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 &  f07_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  

  reg `var' if nonoutlier == 1 & f07_test_observed == 1 & treatment == 1;
  replace col5 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & f07_test_observed == 1 & treatment == 0;
  replace col6 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 &  f07_test_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col7 _b[treatment] _se[treatment] r(p) row_num `row';  
  
 local row = `row' + 2;
  };

  aorder;
  
local row=4;
reg f07_formal_school $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & treatment == 0 & f07_observed == 1, cluster(clustercode);
foreach var of varlist $f07_child_controls {;
test `var';
sig_p col9 _b[`var'] _se[`var'] r(p) row_num `row';
local row=`row' + 2;
};

local row=4;
reg f07_both_norma_total $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & treatment == 0 & f07_test_observed == 1, cluster(clustercode);
foreach var of varlist $f07_child_controls {;
test `var';
sig_p col10 _b[`var'] _se[`var'] r(p) row_num `row';
local row=`row' + 2;
};
  
*Household Level; 
 
local row=11;
foreach var of varlist $f07_hh_controls {;
  replace col0 = "`var'" if row_num == `row';

  reg `var' if nonoutlier == 1 & f07_observed == 1 & treatment == 1;
  replace col1 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & f07_observed == 1 & treatment == 0;
  replace col2 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 &  f07_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  

  reg `var' if nonoutlier == 1 & f07_test_observed == 1 & treatment == 1;
  replace col5 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & f07_test_observed == 1 & treatment == 0;
  replace col6 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 &  f07_test_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col7 _b[treatment] _se[treatment] r(p) row_num `row';  
  
 local row = `row' + 2;
  };

  aorder;
  
local row=11;
reg f07_formal_school $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & treatment == 0 & f07_observed == 1, cluster(clustercode);
foreach var of varlist $f07_hh_controls {;
test `var';
sig_p col9 _b[`var'] _se[`var'] r(p) row_num `row';
local row=`row' + 2;
};

local row=11;
reg f07_both_norma_total $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & treatment == 0 & f07_test_observed == 1, cluster(clustercode);
foreach var of varlist $f07_hh_controls {;
test `var';
sig_p col10 _b[`var'] _se[`var'] r(p) row_num `row';
local row=`row' + 2;
};
  
replace col2="All children" if _n==1;
replace col6="Tested children" if _n==1;
replace col9="Control correlations" if _n==1;
replace col1="Treatment average" if _n==2;
replace col2="Control average" if _n==2;
replace col3="Estimated difference" if _n==2;
replace col5="Treatment average" if _n==2;
replace col6="Control average" if _n==2;
replace col7="Estimated difference" if _n==2;
replace col9="Formal enrollment" if _n==2;
replace col10="Total score" if _n==2;
  sort row_num;
outsheet col* using "Table02_Baseline_f07.csv" if row_num < `row', comma replace;
drop col*;


/*****
*** Table 3: Attrition
*****/

local row = 1;
forvalues var = 0(1)25 {;
   gen col`var' = "";
   };

   replace col2="Nonattritors" if _n==1;
   replace col6="Attritors less nonattritors" if _n==1;
 
   replace col1="Treatment average" if _n==2;
   replace col2="Control average" if _n==2;
   replace col3="Estimated difference" if _n==2;
   replace col5="Treatment difference" if _n==2;
   replace col6="Control difference" if _n==2;
   replace col7="Difference-in-differences" if _n==2;
   
     local row=`row'+2;
   
*Overall Attrition Rate;
  reg f07_attrit if nonoutlier == 1 & treatment == 1 & f07_observed == 1;
  sig_p col1 _b[_cons] _se[_cons] r(p) row_num `row';  
  reg f07_attrit if nonoutlier == 1 & treatment == 0 & f07_observed == 1;
  sig_p col2 _b[_cons] _se[_cons] r(p) row_num `row';  
  reg f07_attrit treatment if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  
  replace col0="Panel A. Attrition rates" if row_num == `row';
  local row = `row' + 2;

*Control Variables: Child Level;
replace col0="Panel B. Child characteristics" if row_num == `row';
local row = `row' + 1;
foreach var of varlist $f07_child_controls {;
  replace col0 = "`var'" if row_num == `row';

  reg `var' if nonoutlier == 1 & treatment == 1 & f07_observed == 1 & f07_attrit == 0;
  replace col1 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & treatment == 0 & f07_observed == 1 & f07_attrit == 0;
  replace col2 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 & f07_observed == 1 & f07_attrit == 0, cluster(clustercode);
  test treatment;
  sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  

  reg `var' f07_attrit if nonoutlier == 1 & treatment == 1 & f07_observed == 1;
  sig_p col5 _b[f07_attrit] _se[f07_attrit] r(p) row_num `row';  
  reg `var' f07_attrit if nonoutlier == 1 & treatment == 0 & f07_observed == 1;
  sig_p col6 _b[f07_attrit] _se[f07_attrit] r(p) row_num `row';  
  reg `var' treatment f07_attrit f07_attrit_treat if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
  test f07_attrit_treat;
  sig_p col7 _b[f07_attrit_treat] _se[f07_attrit_treat] r(p) row_num `row';  

  local row = `row' + 2;
  };

  *Control Variables: Household Level;
  local row=12;
  replace col0="Panel C. Household characteristics" if row_num == `row';
  local row=`row'+1;
  foreach var of varlist $f07_hh_controls {;
  replace col0 = "`var'" if row_num == `row';

  reg `var' if nonoutlier == 1 & treatment == 1 & f07_observed == 1 & f07_attrit == 0;
  replace col1 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & treatment == 0 & f07_observed == 1 & f07_attrit == 0;
  replace col2 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 & f07_observed == 1 & f07_attrit == 0, cluster(clustercode);
  test treatment;
  sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  

  reg `var' f07_attrit if nonoutlier == 1 & treatment == 1 & f07_observed == 1;
  sig_p col5 _b[f07_attrit] _se[f07_attrit] r(p) row_num `row';  
  reg `var' f07_attrit if nonoutlier == 1 & treatment == 0 & f07_observed == 1;
  sig_p col6 _b[f07_attrit] _se[f07_attrit] r(p) row_num `row';  
  reg `var' treatment f07_attrit f07_attrit_treat if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
  test f07_attrit_treat;
  sig_p col7 _b[f07_attrit_treat] _se[f07_attrit_treat] r(p) row_num `row';  

  local row = `row' + 2;
  };
aorder;
sort row_num;

outsheet col* using "Table03_Attrit_f07.csv" if row_num < `row', comma replace;
drop col*;

/****
*** Table 4
*** Please note that Panel B of this table in the paper erroneously omits the standard errors
*** for the treatment effect.
****/

forvalues i = 0(1)6 {;
   gen col`i' ="";
   };

replace col1="No controls - Formal enrollment" if _n==1;
replace col2="Controls - Formal enrollment" if _n==1;
replace col3="No controls - Fall 2007" if _n==1;
replace col4="Controls - Fall 2007" if _n==1;
replace col5="No controls - Spring 2008" if _n==1;
replace col6="Controls - Spring 2008" if _n==1;
replace col0="Panel A. Girls" if _n==2;
replace col0="Panel B. Boys" if _n==10;

*No Controls;

*Formal enrollment;
local i=1;
local row=17;
forvalues k=0(1)1 {;
reg f07_formal_school treatment chagcharan if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == `k', cluster(clustercode);
replace col`i'="No" if row_num==`row';
replace col0="Demographic Controls" if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(r2), "$strformat") if row_num==`row';
replace col0="R2" if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(N), "$strformat") if row_num==`row';
replace col0="Observations" if row_num==`row';
local row=`row'-2;
test treatment;
sig_p col`i' _b[treatment] _se[treatment] r(p) row_num `row';
replace col0="Treatment" if row_num==`row';
local row=`row'-2;
};
*Fall 2007 Test;
local i=3;
local row=17;
forvalues k=0(1)1 {;
reg f07_both_norma_total treatment chagcharan if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == `k', cluster(clustercode);
replace col`i'="No" if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(r2), "$strformat") if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(N), "$strformat") if row_num==`row';
local row=`row'-2;
test treatment;
sig_p col`i' _b[treatment] _se[treatment] r(p) row_num `row';
local row=`row'-2;
};
*Spring 2008 Test;
local i=5;
local row=17;
forvalues k=0(1)1 {;
reg s08_both_norma_total treatment chagcharan if nonoutlier == 1 & s08_test_observed == 1 & s08_girls_cnt == `k', cluster(clustercode);
replace col`i'="No" if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(r2), "$strformat") if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(N), "$strformat") if row_num==`row';
local row=`row'-2;
test treatment;
sig_p col`i' _b[treatment] _se[treatment] r(p) row_num `row';
local row=`row'-2;
};

*With Controls;
*Formal enrollment;
local i=2;
local row=17;
forvalues k=0(1)1 {;
reg f07_formal_school treatment $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == `k', cluster(clustercode);
replace col`i'="Yes" if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(r2), "$strformat") if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(N), "$strformat") if row_num==`row';
local row=`row'-2;
test treatment;
sig_p col`i' _b[treatment] _se[treatment] r(p) row_num `row';
local row=`row'-2;
};
*Fall 2007 Test;
local i=4;
local row=17;
forvalues k=0(1)1 {;
reg f07_both_norma_total treatment $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == `k', cluster(clustercode);
replace col`i'="Yes" if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(r2), "$strformat") if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(N), "$strformat") if row_num==`row';
local row=`row'-2;
test treatment;
sig_p col`i' _b[treatment] _se[treatment] r(p) row_num `row';
local row=`row'-2;
};
*Spring 2008 Test;
local i=6;
local row=17;
forvalues k=0(1)1 {;
reg s08_both_norma_total treatment $s08_child_controls $s08_hh_controls chagcharan if nonoutlier == 1 & s08_test_observed == 1 & s08_girls_cnt == `k', cluster(clustercode);
replace col`i'="Yes" if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(r2), "$strformat") if row_num==`row';
local row=`row'-2;
replace col`i'=string(e(N), "$strformat") if row_num==`row';
local row=`row'-2;
test treatment;
sig_p col`i' _b[treatment] _se[treatment] r(p) row_num `row';
local row=`row'-2;
};
aorder;
sort row_num;

outsheet col* using "Table04.csv", comma replace;
drop col*;

/****
*** Table 5
****/

*Rescale Age Variables;
replace s08_age_cnt = s08_age_cnt - 6;
replace f07_age_cnt = f07_age_cnt - 6;

*Generate Interaction Variables;
gen s08_treat_girl = treatment * s08_girls_cnt;
gen f07_treat_girl = f07_girl_cnt * treatment;
gen s08_treat_age = s08_age_cnt * treatment;
gen f07_treat_age = f07_age_cnt * treatment;
gen s08_treat_girl_age = treatment * s08_age_cnt * s08_girls_cnt;
gen f07_treat_girl_age = treatment * f07_age_cnt * f07_girl_cnt;
gen s08_girl_age_cnt = s08_age_cnt * s08_girls_cnt;
gen f07_girl_age_cnt = f07_age_cnt * f07_girl_cnt;

forvalues i = 0(1)5 {;
   gen col`i' ="";
   };

replace col1="Girls - Formal enrollment" if _n==1;
replace col2="Boys - Formal enrollment" if _n==1;
replace col3="Girls - Fall 2007" if _n==1;
replace col4="Boys - Fall 2007" if _n==1;
replace col0="Treatment" if _n==2;
replace col0="Treatment x age" if _n==4;
replace col0="Age" if _n==6;
replace col0="Observations" if _n==9;
replace col0="R2" if _n==10;
replace col0="Demographic controls" if _n==11;


local i=2;

forvalues k=0(1)1 {;
local row=2;
foreach var of varlist treatment f07_treat_age f07_age_cnt {;
reg f07_formal_school treatment f07_treat_age $f07_child_controls $f07_hh_controls chagcharan if nonoutlier ==1 & f07_observed ==1 & f07_girl_cnt==`k', cluster(clustercode);
test `var';
sig_p col`i' _b[`var'] _se[`var'] r(p) row_num `row';
local row=`row'+2;
};

local row=`row'+1;
reg f07_formal_school treatment f07_treat_age $f07_child_controls $f07_hh_controls chagcharan if nonoutlier ==1 & f07_observed ==1 & f07_girl_cnt==`k', cluster(clustercode);
ereturn list;
replace col`i' = string(e(N), "$strformat")  if row_num == `row';

replace col`i'=string(e(r2), "$strformat") if row_num==`row'+1;
replace col`i'="Yes" if row_num==`row'+2;
local i=`i'-1;
};

local i=4;

forvalues k=0(1)1 {;
local row=2;
foreach var of varlist treatment f07_treat_age f07_age_cnt {;
reg f07_both_norma treatment f07_treat_age $f07_child_controls $f07_hh_controls chagcharan if nonoutlier ==1 & f07_observed ==1 & f07_girl_cnt==`k', cluster(clustercode);
test `var';
sig_p col`i' _b[`var'] _se[`var'] r(p) row_num `row';
local row=`row'+2;
};

local row=`row'+1;
reg f07_both_norma treatment f07_treat_age $f07_child_controls $f07_hh_controls chagcharan if nonoutlier ==1 & f07_observed ==1 & f07_girl_cnt==`k', cluster(clustercode);
ereturn list;
replace col`i' = string(e(N), "$strformat")  if row_num == `row';

replace col`i'=string(e(r2), "$strformat") if row_num==`row'+1;
replace col`i'="Yes" if row_num==`row'+2;
local i=`i'-1;
};

aorder;
sort row_num;

outsheet col* using "Table05_Outcomes.csv" if row_num < `row'+3, comma replace;
drop col*;


*Rescale Age Variables;
replace f07_age_cnt = f07_age_cnt + 6;
replace s08_age_cnt = s08_age_cnt + 6;

/*****
*** Table A1: Baseline Comparison S08, Footnote 20
*****/

local row = 1;
forvalues var = 0(1)25 {;
   gen col`var' = "";
   };

*Control Variables;
foreach var of varlist $s08_child_controls $s08_hh_controls {;
  replace col0 = "Child Level Tested `var'" if row_num == `row';

  reg `var' if nonoutlier == 1 & treatment == 1 & s08_observed == 1;
  replace col1 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & treatment == 0 & s08_observed == 1;
  replace col2 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  

  reg `var' if nonoutlier == 1 & treatment == 1 & s08_test_observed == 1;
  replace col6 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & treatment == 0 & s08_test_observed == 1;
  replace col7 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 & s08_test_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col8 _b[treatment] _se[treatment] r(p) row_num `row';  

  local row = `row' + 2;
  };

aorder;
sort row_num;

outsheet col* using "TableA1_Baseline_s08.csv" if row_num < `row', comma replace;
drop col*;

/*****
*** Table A2: Attrition Test Observed F07, Footnote 20
*****/

local row = 1;
forvalues var = 0(1)25 {;
   gen col`var' = "";
   };

*Overall Attrition Rate;
  reg f07_test_attrit if nonoutlier == 1 & treatment == 1 & f07_test_observed == 1;
  sig_p col1 _b[_cons] _se[_cons] r(p) row_num `row';  
  reg f07_test_attrit if nonoutlier == 1 & treatment == 0 & f07_test_observed == 1;
  sig_p col2 _b[_cons] _se[_cons] r(p) row_num `row';  
  reg f07_test_attrit treatment if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
  test treatment;
  sig_p col3 _b[treatment] _se[treatment] r(p) row_num `row';  
  local row = `row' + 2;

*Control Variables;
foreach var of varlist $f07_child_controls $f07_hh_controls {;
  replace col0 = "Child Level Tested `var'" if row_num == `row';

  reg `var' f07_test_attrit if nonoutlier == 1 & treatment == 1 & f07_test_observed == 1;
  sig_p col1 _b[f07_test_attrit] _se[f07_test_attrit] r(p) row_num `row';  
  reg `var' f07_test_attrit if nonoutlier == 1 & treatment == 0 & f07_test_observed == 1;
  sig_p col2 _b[f07_test_attrit] _se[f07_test_attrit] r(p) row_num `row';  
  reg `var' treatment f07_test_attrit f07_test_attrit_treat if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
  test f07_test_attrit_treat;
  sig_p col3 _b[f07_test_attrit_treat] _se[f07_test_attrit_treat] r(p) row_num `row';  

  reg `var' if nonoutlier == 1 & treatment == 1 & f07_observed == 1 & f07_test_attrit == 0;
  replace col6 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' if nonoutlier == 1 & treatment == 0 & f07_test_observed == 1 & f07_test_attrit == 0;
  replace col7 = string(_b[_cons], "$strformat") if row_num == `row';
  reg `var' treatment if nonoutlier == 1 & f07_test_observed == 1 & f07_test_attrit == 0, cluster(clustercode);
  test treatment;
  sig_p col8 _b[treatment] _se[treatment] r(p) row_num `row';  

  local row = `row' + 2;
  };

aorder;
sort row_num;

outsheet col* using "TableA2_Attrit_Test_f07.csv" if row_num < `row', comma replace;
drop col*;

/*****
*** Table A3: Observed vs Tested Characteristics, Footnote 20
*****/

local row = 1;
forvalues var = 0(1)25 {;
   gen col`var' = "";
   };

*Control Variables;
foreach var of varlist $f07_child_controls $f07_hh_controls {;
  replace col0 = "Child Level Tested `var'" if row_num == `row';

  reg `var' f07_test_observed if nonoutlier == 1 & treatment == 1 & f07_observed == 1;
  sig_p col1 _b[f07_test_observed] _se[f07_test_observed] r(p) row_num `row';  
  reg `var' f07_test_observed if nonoutlier == 1 & treatment == 0 & f07_observed == 1;
  sig_p col2 _b[f07_test_observed] _se[f07_test_observed] r(p) row_num `row';  
  reg `var' treatment f07_test_observed f07_test_observed_treat if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
  test f07_test_observed_treat;
  sig_p col3 _b[f07_test_observed_treat] _se[f07_test_observed_treat] r(p) row_num `row';  

  reg `var' s08_test_observed if nonoutlier == 1 & treatment == 1 & s08_observed == 1;
  sig_p col6 _b[s08_test_observed] _se[s08_test_observed] r(p) row_num `row';  
  reg `var' s08_test_observed if nonoutlier == 1 & treatment == 0 & s08_observed == 1;
  sig_p col7 _b[s08_test_observed] _se[s08_test_observed] r(p) row_num `row';  
  reg `var' treatment s08_test_observed s08_test_observed_treat if nonoutlier == 1 & s08_observed == 1, cluster(clustercode);
  test s08_test_observed_treat;
  sig_p col8 _b[s08_test_observed_treat] _se[s08_test_observed_treat] r(p) row_num `row';  

  local row = `row' + 2;
  };

aorder;
sort row_num;

outsheet col* using "TableA3_Test_Coverage_Char.csv" if row_num < `row', comma replace;
drop col*;

/*****
*** Statistics Reported in Text
*****/

*LATE Estimates for effect of formal school enrollment;
ivreg f07_both_norma_total (f07_formal_school = treatment) $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 0, cluster(clustercode);

ivreg f07_both_norma_total (f07_formal_school = treatment) $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 1, cluster(clustercode);


log close;
