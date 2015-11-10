version 11
clear all
set more off
#delimit ;
set mem 500M;
capture log close;
log using Fischer_Test_New, replace text;

/*****
*** Global Variables
*****/

*Directories;
global dir_temp "c:\temp"; *This is just a directory for temporary files;

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

*List of Relevant Outcome Variables;
global outcome_variables "f07_both_norma_total f07_formal_school s08_both_norma_total";   
   
*Format for all numeric output;
global strformat "%8.3f";
   
/*****
*** Set up file with all permutations
*** The experiment was a clustered randomized controll trial with 7 clusters
*** and the indicated number of villages groups in each cluster.
*** This code generates a data set that contains all  64 possible outcomes of this
*** randomization strategy.
*****/

set obs 2;
forvalues i = 1(1)11 {;
   gen sim_treat`i' = 0;
   };
replace sim_treat2 = 1 if _n == 1;
replace sim_treat5 = 1 if _n == 2;

gen perm1 = _n;
expand 2;
bysort perm1: replace sim_treat3 = 1 if _n == 1;
bysort perm1: replace sim_treat4 = 1 if _n == 2;

gen perm2 = _n;
expand 2;
bysort perm2: replace sim_treat1 = 1 if _n == 1;
bysort perm2: replace sim_treat7 = 1 if _n == 2;

gen perm3 = _n;
expand 2;
bysort perm3: replace sim_treat6 = 1 if _n == 1;

gen perm4 = _n;
expand 4;
bysort perm4: replace sim_treat8 = 1 if _n == 1;
bysort perm4: replace sim_treat9 = 1 if _n == 2;
bysort perm4: replace sim_treat10 = 1 if _n == 3;
bysort perm4: replace sim_treat11 = 1 if _n == 4;

drop perm*;
gen permutation_number = _n;
reshape long sim_treat, i(permutation_number) j(clustercode);
sort permutation_number clustercode;
save "${dir_temp}\permutations", replace;

/*****
*** Load in Primary Data
*****/

use "afghanistan_anonymized_data", clear;

*Keep only the variables needed for the analysis;
keep treat clustercode f07_observed f07_test_observed chagcharan $outcome_variables $f07_hh_controls 
   $f07_child_controls $s08_hh_controls $s08_child_controls s08_observed s08_test_observed;

/*****
*** Create Additional Variables
*****/

*Order clustercodes consecutively;
replace clustercode = clustercode - 4 if clustercode >= 12;

*Create randomization group ID's;
gen rand_group2 = clustercode == 3 | clustercode == 4;
gen rand_group3 = clustercode == 1 | clustercode == 7;
gen rand_group4 = clustercode == 6;

*Outlier Identification;
gen nonoutlier = 1;
replace nonoutlier = 0 if f07_num_ppl_hh_cnt > 20 & f07_observed == 1;
replace nonoutlier = 0 if f07_jeribs_cnt > 10 & f07_observed == 1;
replace nonoutlier = 0 if f07_num_sheep_cnt > 50 & f07_observed == 1;
replace nonoutlier = 0 if s08_num_ppl_hh_cnt > 20 & s08_observed == 1;
replace nonoutlier = 0 if s08_jeribs_cnt > 10 & s08_observed == 1;
replace nonoutlier = 0 if s08_num_sheep_cnt > 50 & s08_observed == 1;

*rescale age variables for age interactions;
replace f07_age_cnt = f07_age_cnt - 6;

*Interaction variables;
gen treat_age_cnt = treatment*f07_age_cnt;
gen treat_girl = treatment * f07_girl_cnt;
gen girl_age = f07_girl_cnt * f07_age_cnt;
gen treat_girl_age = treat_girl * f07_age_cnt;

/*****
*** Limit sample to that used for analysis
*****/

keep if nonoutlier == 1;

/*****
*** Create estimates from the actual outcome of the randomization
*****/

   *Unadjusted;
   regress f07_formal_school treatment chagcharan if f07_observed == 1 & f07_girl_cnt == 0;
   local rank_uadj_male_school_b = _b[treatment];
   regress f07_both_norma_total treatment chagcharan if f07_test_observed == 1 & f07_girl_cnt == 0;
   local rank_uadj_male_f07_test_b = _b[treatment];
   regress s08_both_norma_total treatment chagcharan if s08_test_observed == 1 & s08_girls_cnt == 0;
   local rank_uadj_male_s08_test_b = _b[treatment];

   regress f07_formal_school treatment chagcharan if f07_observed == 1 & f07_girl_cnt == 1;
   local rank_uadj_female_school_b = _b[treatment];
   regress f07_both_norma_total treatment chagcharan if f07_test_observed == 1 & f07_girl_cnt == 1;
   local rank_uadj_female_f07_test_b = _b[treatment];
   regress s08_both_norma_total treatment chagcharan if s08_test_observed == 1 & s08_girls_cnt == 1;
   local rank_uadj_female_s08_test_b = _b[treatment];

   *Boys;
   regress f07_formal_school treatment chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 0;
   local rank_male_school_b = _b[treatment];
   regress f07_both_norma_total treatment chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 0;
   local rank_male_f07_test_b = _b[treatment];
   regress f07_formal_school treatment treat_age_cnt chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 0;
   local rank_male_school_age_treat_b = _b[treatment];
   local rank_male_school_age_age_b = _b[treat_age_cnt];
   regress f07_both_norma_total treatment treat_age_cnt chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 0;
   local rank_male_test_age_treat_b = _b[treatment];
   local rank_male_test_age_age_b = _b[treat_age_cnt];
   regress s08_both_norma_total treatment chagcharan ${s08_child_controls} ${s08_hh_controls} if nonoutlier == 1 & s08_test_observed == 1 & s08_girls_cnt == 0;
   local rank_male_s08_test_b = _b[treatment];
   
   *Girls;
   regress f07_formal_school treatment chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 1;
   local rank_female_school_b = _b[treatment];
   regress f07_both_norma_total treatment chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 1;
   local rank_female_f07_test_b = _b[treatment];
   regress f07_formal_school treatment treat_age_cnt chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 1;
   local rank_female_school_age_treat_b = _b[treatment];
   local rank_female_school_age_age_b = _b[treat_age_cnt];
   regress f07_both_norma_total treatment treat_age_cnt chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 1;
   local rank_female_test_age_treat_b = _b[treatment];
   local rank_female_test_age_age_b = _b[treat_age_cnt];
   regress s08_both_norma_total treatment chagcharan ${s08_child_controls} ${s08_hh_controls} if nonoutlier == 1 & s08_test_observed == 1 & s08_girls_cnt == 1;
   local rank_female_s08_test_b = _b[treatment];

   *Both;
   regress f07_formal_school treatment treat_girl chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1;
   local rank_both_school_treat_b = _b[treatment];
   local rank_both_school_treat_gen_b = _b[treat_girl];
   regress f07_both_norma_total treatment treat_girl chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1;
   local rank_both_f07_test_treat_b = _b[treatment];
   local rank_both_f07_test_treat_gen_b = _b[treat_girl];
   regress f07_formal_school treatment treat_age_cnt treat_girl girl_age treat_girl_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1;
   local rank_both_asch_treat_b = _b[treatment];
   local rank_both_asch_treat_gen_b = _b[treat_girl];
   local rank_both_asch_treat_age_b = _b[treat_age];
   local rank_both_asch_treat_age_gen_b =_b[treat_girl_age];
   regress f07_both_norma_total treatment treat_age_cnt treat_girl girl_age treat_girl_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1;
   local rank_both_atest_treat_b = _b[treatment];
   local rank_both_atest_treat_gen_b = _b[treat_girl];
   local rank_both_atest_treat_age_b = _b[treat_age];
   local rank_both_atest_treat_age_gen_b =_b[treat_girl_age];
   replace treat_girl = treatment * s08_girls_cnt;
   regress s08_both_norma_total treatment treat_girl chagcharan ${s08_child_controls} ${s08_hh_controls} if nonoutlier == 1 & s08_test_observed == 1;
   local rank_both_s08_test_treat_b = _b[treatment];
   local rank_both_s08_test_treat_gen_b = _b[treat_girl];

/*****
*** Merge with data on all possible outcomes of randomization
*****/   

*Expand and Save Data set;	
gen observation_number = _n;
expand 64;
bysort observation_number: gen permutation_number = _n;

sort permutation_number clustercode;
merge permutation_number clustercode using "${dir_temp}\permutations";
assert _merge == 3;

/*****
*** Create Treatment Estimates for All Possible Outcomes of Randomization
*****/

*Creat blank variables to populate;
gen sim_treat_age = sim_treat * f07_age_cnt;

gen uadj_male_school_treat = .;
gen uadj_male_f07_test_treat = .;
gen uadj_male_s08_test_treat = .;
gen uadj_female_school_treat = .;
gen uadj_female_f07_test_treat = .;
gen uadj_female_s08_test_treat = .;

gen male_school_treat = .;
gen male_f07_test_treat = .;
gen male_school_age_treat_treat = .;
gen male_school_age_age_treat = .;
gen male_test_age_treat_treat = .;
gen male_test_age_age_treat = .;
gen male_s08_test_treat = .;

gen female_school_treat = .;
gen female_f07_test_treat = .;
gen female_school_age_treat_treat = .;
gen female_school_age_age_treat = .;
gen female_test_age_treat_treat = .;
gen female_test_age_age_treat = .;
gen female_s08_test_treat = .;

gen both_school_treat_treat = .;
gen both_school_treat_gen_treat = .;
gen both_f07_test_treat_treat = .;
gen both_f07_test_treat_gen_treat = .;
gen both_asch_treat_treat = .;
gen both_asch_treat_gen_treat = .;
gen both_asch_treat_age_treat = .;
gen both_asch_treat_age_gen_treat = .;
gen both_atest_treat_treat = .;
gen both_atest_treat_gen_treat = .;
gen both_atest_treat_age_treat = .;
gen both_atest_treat_age_gen_treat = .;
gen both_s08_test_treat_treat = .;
gen both_s08_test_treat_gen_treat = .;


gen sim_treat_girl = .;
gen sim_treat_girl_age = .;

forvalues i = 1(1)64 {;
   display "Permutation number `i' of `number_permutations'";
   
   *Unadjusted;
   quietly regress f07_formal_school sim_treat chagcharan if f07_observed == 1 & f07_girl_cnt == 0 & permutation_number == `i';
   quietly replace uadj_male_school_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress f07_both_norma_total sim_treat chagcharan if f07_test_observed == 1 & f07_girl_cnt == 0 & permutation_number == `i';
   quietly replace uadj_male_f07_test_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress s08_both_norma_total sim_treat chagcharan if s08_test_observed == 1 & s08_girls_cnt == 0 & permutation_number == `i';
   quietly replace uadj_male_s08_test_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress f07_formal_school sim_treat chagcharan if f07_observed == 1 & f07_girl_cnt == 1 & permutation_number == `i';
   quietly replace uadj_female_school_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress f07_both_norma_total sim_treat chagcharan if f07_test_observed == 1 & f07_girl_cnt == 1 & permutation_number == `i';
   quietly replace uadj_female_f07_test_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress s08_both_norma_total sim_treat chagcharan if s08_test_observed == 1 & s08_girls_cnt == 1 & permutation_number == `i';
   quietly replace uadj_female_s08_test_treat = _b[sim_treat] if permutation_number == `i';
      
   *Boys;
   quietly regress f07_formal_school sim_treat chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 0 & permutation_number == `i';
   quietly replace male_school_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress f07_both_norma_total sim_treat chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 0 & permutation_number == `i';
   quietly replace male_f07_test_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress f07_formal_school sim_treat sim_treat_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 0 & permutation_number == `i';
   quietly replace male_school_age_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace male_school_age_age_treat = _b[sim_treat_age] if permutation_number == `i';

   quietly regress f07_both_norma_total sim_treat sim_treat_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 0 & permutation_number == `i';
   quietly replace male_test_age_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace male_test_age_age_treat = _b[sim_treat_age] if permutation_number == `i';

   quietly regress s08_both_norma_total sim_treat chagcharan ${s08_child_controls} ${s08_hh_controls} if nonoutlier == 1 & s08_test_observed == 1 & s08_girls_cnt == 0 & permutation_number == `i';
   quietly replace male_s08_test_treat = _b[sim_treat] if permutation_number == `i';

   *Girls;
   quietly regress f07_formal_school sim_treat chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 1 & permutation_number == `i';
   quietly replace female_school_treat = _b[sim_treat] if permutation_number == `i';
   
   quietly regress f07_both_norma_total sim_treat chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 1 & permutation_number == `i';
   quietly replace female_f07_test_treat = _b[sim_treat] if permutation_number == `i';

   quietly regress f07_formal_school sim_treat sim_treat_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & f07_girl_cnt == 1 & permutation_number == `i';
   quietly replace female_school_age_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace female_school_age_age_treat = _b[sim_treat_age] if permutation_number == `i';

   quietly regress f07_both_norma_total sim_treat sim_treat_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & f07_girl_cnt == 1 & permutation_number == `i';
   quietly replace female_test_age_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace female_test_age_age_treat = _b[sim_treat_age] if permutation_number == `i';

   quietly regress s08_both_norma_total sim_treat chagcharan ${s08_child_controls} ${s08_hh_controls} if nonoutlier == 1 & s08_test_observed == 1 & s08_girls_cnt == 1 & permutation_number == `i';
   quietly replace female_s08_test_treat = _b[sim_treat] if permutation_number == `i';
   
   *Both;
   quietly replace sim_treat_girl = f07_girl_cnt*sim_treat;
   quietly regress f07_formal_school sim_treat sim_treat_girl chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & permutation_number == `i';
   quietly replace both_school_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace both_school_treat_gen_treat = _b[sim_treat_girl] if permutation_number == `i';

   quietly regress f07_both_norma_total sim_treat sim_treat_girl chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & permutation_number == `i';
   quietly replace both_f07_test_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace both_f07_test_treat_gen_treat = _b[sim_treat_girl] if permutation_number == `i';

   quietly replace sim_treat_girl_age = sim_treat_girl * f07_age_cnt;
   quietly regress f07_formal_school sim_treat sim_treat_age sim_treat_girl sim_treat_girl_age girl_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_observed == 1 & permutation_number == `i';
   quietly replace both_asch_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace both_asch_treat_gen_treat = _b[sim_treat_girl] if permutation_number == `i';
   quietly replace both_asch_treat_age_treat = _b[sim_treat_age] if permutation_number == `i';
   quietly replace both_asch_treat_age_gen_treat = _b[sim_treat_girl_age] if permutation_number == `i';

   quietly regress f07_both_norma_total sim_treat sim_treat_age sim_treat_girl sim_treat_girl_age girl_age chagcharan ${f07_child_controls} ${f07_hh_controls} if nonoutlier == 1 & f07_test_observed == 1 & permutation_number == `i';
   quietly replace both_atest_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace both_atest_treat_gen_treat = _b[sim_treat_girl] if permutation_number == `i';
   quietly replace both_atest_treat_age_treat = _b[sim_treat_age] if permutation_number == `i';
   quietly replace both_atest_treat_age_gen_treat = _b[sim_treat_girl_age] if permutation_number == `i';

   quietly replace sim_treat_girl = s08_girls_cnt*sim_treat;
   quietly regress s08_both_norma_total sim_treat sim_treat_girl chagcharan ${s08_child_controls} ${s08_hh_controls} if nonoutlier == 1 & s08_test_observed == 1 & permutation_number == `i';
   quietly replace both_s08_test_treat_treat = _b[sim_treat] if permutation_number == `i';
   quietly replace both_s08_test_treat_gen_treat = _b[sim_treat_girl] if permutation_number == `i';
   };

/*****
*** Estimate p-values
*****/

global Variables "
   uadj_male_school uadj_male_f07_test uadj_male_s08_test uadj_female_school uadj_female_f07_test uadj_female_s08_test
   male_school male_f07_test male_s08_test male_school_age_treat male_school_age_age male_test_age_treat 
   male_test_age_age
   female_school female_f07_test female_s08_test female_school_age_treat female_school_age_age female_test_age_treat 
   female_test_age_age
   both_school_treat both_school_treat_gen both_f07_test_treat both_f07_test_treat_gen both_s08_test_treat both_s08_test_treat_gen
   both_asch_treat both_asch_treat_gen both_asch_treat_age both_asch_treat_age_gen
   both_atest_treat both_atest_treat_gen both_atest_treat_age both_atest_treat_age_gen";
local number_permutations = 64;
global test_precision = 0.00001;

bysort permutation_number: keep if _n == 1;

*Create critical value numbers;
global upper_1 = `number_permutations' - ceil(`number_permutations'*0.01/2);
global lower_1 = floor(`number_permutations'*0.01/2);
global upper_5 = `number_permutations' - ceil(`number_permutations'*0.05/2);
global lower_5 = floor(`number_permutations'*0.05/2);
global upper_10 = `number_permutations' - ceil(`number_permutations'*0.1/2);
global lower_10 = floor(`number_permutations'*0.1/2);

foreach var in $Variables {;
   quietly count if round(`var'_treat, $test_precision) > round(`rank_`var'_b', $test_precision);
   display "`var' estimate is " round(`rank_`var'_b', $test_precision) " p-value is " r(N)/`number_permutations'*2;
   quietly sort `var'_treat;
   display "`var' 1  percent lower: " round(`var'_treat[${lower_1}], $test_precision) " upper " round(`var'_treat[${upper_1}], $test_precision);
   display "`var' 5  percent lower: " round(`var'_treat[${lower_5}], $test_precision) " upper " round(`var'_treat[${upper_5}], $test_precision);
   display "`var' 10 percent lower: " round(`var'_treat[${lower_10}], $test_precision) " upper " round(`var'_treat[${upper_10}], $test_precision);
   
   display " ";
   };

foreach var in $Variables {;
   if `rank_`var'_b' > 0 {;
      quietly count if round(`var'_treat, $test_precision) > round(`rank_`var'_b', $test_precision);
      display "`var' estimate is " round(`rank_`var'_b', $test_precision) " p-value is " r(N)/`number_permutations'*2;
      };
   if `rank_`var'_b' < 0 {;
      quietly count if round(`var'_treat, $test_precision) < round(`rank_`var'_b', $test_precision);
      display "`var' estimate is " round(`rank_`var'_b', $test_precision) " p-value is " r(N)/`number_permutations'*2;
      };
   };
log close;
