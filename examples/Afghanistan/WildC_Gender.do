version 11
# delimit;
clear all;
set more off;
set mem 200m;
capture log close;
log using WildC_Gender.log, replace text;

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

*Format for all numeric output;
global strformat "%8.3f";
   
*Variables for Bootstrap;
global var_stems "formal total total_s08 late fgage tgage";

set seed 1533;
local tot_iterations = 10000;

/*****
*** Functions
*****/

capture program drop p_vals;
program p_vals;
   args tstat_var var iterations;

   gen abs_`tstat_var' = abs(`tstat_var');

   quietly count if `tstat_var' > _b[`var']/_se[`var'];
   quietly local t_upper_p = r(N)/`iterations';
   quietly count if `tstat_var' < _b[`var']/_se[`var'];
   quietly local t_lower_p = r(N)/`iterations';

   display "Prob >= t-stat: `t_upper_p'  Prob <= t-stat: `t_lower_p'";

   end;

/*****
*** Set up Data
*****/

use afghanistan_anonymized_data, clear;

/*****
*** Create Additional Variables
*****/

*Outlier Identification;
gen nonoutlier = 1;
replace nonoutlier = 0 if f07_num_ppl_hh_cnt > 20 & f07_observed == 1;
replace nonoutlier = 0 if f07_jeribs_cnt > 10 & f07_observed == 1;
replace nonoutlier = 0 if f07_num_sheep_cnt > 50 & f07_observed == 1;
replace nonoutlier = 0 if s08_num_ppl_hh_cnt > 20 & s08_observed == 1;
replace nonoutlier = 0 if s08_jeribs_cnt > 10 & s08_observed == 1;
replace nonoutlier = 0 if s08_num_sheep_cnt > 50 & s08_observed == 1;

*Rescale Age Variables for Age Interactions;
replace f07_age_cnt = f07_age_cnt - 6;

*Create Interaction Variables;
gen f07_treat_girl = treatment*f07_girl_cnt;
gen f07_treat_age = f07_age_cnt * treatment;
gen f07_treat_girl_age = f07_girl_cnt * f07_age_cnt * treatment;
gen f07_formal_girl = f07_formal_school * f07_girl_cnt;
gen f07_girl_age_cnt = f07_girl_cnt * f07_age_cnt;
gen s08_treat_girl = treatment*s08_girls_cnt;

/******
*** Create Residuals
******/

*Limit sample to non-outlier boys for the fall 2007 sample;
keep if nonoutlier == 1 & f07_observed == 1;

*Enrollment;
reg f07_formal_school treatment f07_treat_girl $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
global formal_treat = _b[treatment];
global formal_treat_girl = _b[f07_treat_girl];
global formal_girl = _b[f07_girl_cnt];
predict est_formal;
gen resid_formal = f07_formal_school - est_formal;

* F07 Test Scores;
reg f07_both_norma_total treatment f07_treat_girl $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
global total_treat = _b[treatment];
global total_treat_girl = _b[f07_treat_girl];
global total_girl = _b[f07_girl_cnt];
predict est_total;
gen resid_total = f07_both_norma_total - est_total if f07_test_observed == 1;

* S08 Test Scores;
reg s08_both_norma_total treatment s08_treat_girl $s08_child_controls $s08_hh_controls chagcharan if nonoutlier == 1 & s08_test_observed == 1, cluster(clustercode);
global total_s08_treat = _b[treatment];
global total_s08_treat_girl = _b[s08_treat_girl];
global total_s08_girl = _b[s08_girls_cnt];
predict est_total_s08;
gen resid_total_s08 = s08_both_norma_total - est_total if s08_test_observed == 1;

*LATE Test Scores;
ivreg f07_both_norma_total (f07_formal_school f07_formal_girl = treatment f07_treat_girl) $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
global late_formal = _b[f07_formal_school];
global late_formal_girl = _b[f07_formal_girl];
global late_girl = _b[f07_girl_cnt];
predict est_late;
gen resid_late = f07_both_norma_total - est_late if f07_test_observed == 1;

*Age-Gender Enrollment;
reg f07_formal_school f07_girl_cnt f07_age_cnt f07_girl_age_cnt treatment f07_treat_girl f07_treat_age f07_treat_girl_age f07_heads_child $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
global fgage_girl = _b[f07_girl_cnt];
global fgage_age = _b[f07_age_cnt];
global fgage_girl_age = _b[f07_girl_age_cnt];
global fgage_treat = _b[treatment];
global fgage_treat_girl = _b[f07_treat_girl];
global fgage_treat_age = _b[f07_treat_age];
global fgage_treat_girl_age = _b[f07_treat_girl_age];
predict est_fgage;
gen resid_fgage = f07_formal_school - est_fgage;

*Age-Gender test scores;
reg f07_both_norma_total f07_girl_cnt f07_age_cnt f07_girl_age_cnt treatment f07_treat_girl f07_treat_age f07_treat_girl_age f07_heads_child $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
global tgage_girl = _b[f07_girl_cnt];
global tgage_age = _b[f07_age_cnt];
global tgage_girl_age = _b[f07_girl_age_cnt];
global tgage_treat = _b[treatment];
global tgage_treat_girl = _b[f07_treat_girl];
global tgage_treat_age = _b[f07_treat_age];
global tgage_treat_girl_age = _b[f07_treat_girl_age];
predict est_tgage;
gen resid_tgage = f07_both_norma_total - est_tgage;

*Reorder clustercodes to put controls first;
bysort clustercode: gen one_cluster = _n == 1;
sort treatment clustercode;
gen temp = sum(one_cluster);
bysort clustercode: egen newclustercode = max(temp);
drop clustercode;
rename newclustercode clustercode;

*Order clusters so that children with tests are first;
gsort clustercode -f07_test_observed;
by clustercode: gen resid_num = _n;
by clustercode: gen resid_tot_en = _N;
by clustercode: egen resid_tot_test = sum(f07_test_observed);

*Create variables with residual counts for each cluster;
forvalues i = 1(1)11 {;
   replace clustercode = -1 if clustercode == `i';
   sort clustercode;
   local resid_tot_en_`i' = resid_tot_en[1];
   local resid_tot_pen_`i' = resid_tot_en[1];
   local resid_tot_test_`i' = resid_tot_test[1];
   replace clustercode = `i' if clustercode == -1;
   };
save "$dir_temp/full_data_penroll", replace;

gen test_ob = f07_test_observed == 1;
keep clustercode resid_num resid_* test_ob treatment;
rename clustercode cluster_match;
rename resid_num resid_num_match;
foreach var in $var_stems {;
   rename resid_`var' resid_`var'_match;
   };
save "$dir_temp/temp_penroll", replace;

drop test_ob treatment;
drop resid_total_* resid_late_* resid_tgage_*;
rename cluster_match cluster_match_en;
rename resid_num_match resid_num_match_en;
sort cluster_match_en resid_num_match_en;
save "$dir_temp/residuals_enrollment", replace;

use "$dir_temp/temp_penroll", clear;
keep if test_ob == 1;
keep cluster_match resid_num_match resid_total_* resid_late_* resid_tgage_*;
rename cluster_match cluster_match_test;
rename resid_num_match resid_num_match_test;
sort cluster_match_test resid_num_match_test;
save "$dir_temp/residuals_test", replace;

/*******
*** Bootstrap Procedures
*******/

use "$dir_temp/full_data_penroll", clear;

set obs 10000;
gen obsnum = _n;
gen cluster_match_en = .;
gen resid_num_match_en = .;
gen cluster_match_test = .;
gen resid_num_match_test = .;
foreach var in $var_stems {;
   gen resid_`var'_match = .;
   gen y_`var' = .;
   gen w_`var'_treat = .;
   gen w_`var'_girl = .;
   gen w_`var'_treat_girl = .;
   };
gen w_fgage_age = .;
gen w_fgage_girl_age = .;
gen w_fgage_treat_age = .;
gen w_fgage_treat_girl_age = .;
gen w_tgage_age = .;
gen w_tgage_girl_age = .;
gen w_tgage_treat_age = .;
gen w_tgage_treat_girl_age = .;
gen w_late_formal = .;
gen w_late_formal_girl = .;

gen obsorder = .;

forvalues iter = 1(1)`tot_iterations' {;

   display("Iternation: `iter'");

   *Reset residual variables;
   quietly replace cluster_match_en = .;
   quietly replace resid_num_match_en = .;
   quietly replace cluster_match_test = .;
   quietly replace resid_num_match_test = .;
   foreach var in $var_stems {;
      quietly drop resid_`var'_match;
      };
   *drop obsorder;
   drop y_*;
   
   *Assign Residuals;
   forvalues i = 1(1)11 {;

      *Enrollment;
      global temp_cluster_match = ceil(uniform()*11);
      quietly replace cluster_match_en = $temp_cluster_match if clustercode == `i';
      sort obsnum;
      quietly replace resid_num_match_en = ceil(uniform()*`resid_tot_en_$temp_cluster_match') if clustercode == `i';

      *Test Scores;
      global temp_cluster_match = ceil(uniform()*11);
      quietly replace cluster_match_test = $temp_cluster_match if clustercode == `i';
      sort obsnum;
      quietly replace resid_num_match_test = ceil(uniform()*`resid_tot_test_$temp_cluster_match') if clustercode == `i';
      };

   *Merge Enrollment;
   sort cluster_match_en resid_num_match_en;
   quietly merge cluster_match_en resid_num_match_en using "$dir_temp/residuals_enrollment";
   quietly drop if _merge == 2;
   drop _merge;

   *Merge Test Scores;
   sort cluster_match_test resid_num_match_test;
   quietly merge cluster_match_test resid_num_match_test using "$dir_temp/residuals_test";
   quietly drop if _merge == 2;
   drop _merge;

   *Balanced weighting;
   forvalues i = 1(1)11 {;
       local temp_rand = uniform();
       foreach var in $var_stems {;
          quietly replace resid_`var'_match = resid_`var'_match*(-1) if `temp_rand' < 0.5 & clustercode == `i';
          };
       };

   *Create estimate;
   foreach var in $var_stems {;
      quietly gen y_`var' = est_`var' + resid_`var'_match;
      };

   *Do Regressions: Enrollment;
   quietly reg y_formal treatment f07_treat_girl $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
   quietly replace w_formal_treat = (_b[treatment] - $formal_treat)/_se[treatment] if obsnum == `iter';
   quietly replace w_formal_treat_girl = (_b[f07_treat_girl] - $formal_treat_girl)/_se[f07_treat_girl] if obsnum == `iter';
   quietly replace w_formal_girl = (_b[f07_girl_cnt] - $formal_girl)/_se[f07_girl_cnt] if obsnum == `iter';

   *Do Regressions: F07 Test Scores;
   quietly reg y_total treatment f07_treat_girl $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
   quietly replace w_total_treat = (_b[treatment] - $total_treat)/_se[treatment] if obsnum == `iter';
   quietly replace w_total_treat_girl = (_b[f07_treat_girl] - $total_treat_girl)/_se[f07_treat_girl] if obsnum == `iter';
   quietly replace w_total_girl = (_b[f07_girl_cnt] - $total_girl)/_se[f07_girl_cnt] if obsnum == `iter';

   *Do Regressions: S08 Test Scores;
   quietly reg y_total treatment s08_treat_girl $s08_child_controls $s08_hh_controls chagcharan if nonoutlier == 1 & s08_test_observed == 1, cluster(clustercode);
   quietly replace w_total_s08_treat = (_b[treatment] - $total_treat)/_se[treatment] if obsnum == `iter';
   quietly replace w_total_s08_treat_girl = (_b[s08_treat_girl] - $total_treat_girl)/_se[s08_treat_girl] if obsnum == `iter';
   quietly replace w_total_s08_girl = (_b[s08_girls_cnt] - $total_girl)/_se[s08_girls_cnt] if obsnum == `iter';

   *Do Regressions: LATE;
   quietly ivreg y_late (f07_formal_school f07_formal_girl = treatment f07_treat_girl) $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
   quietly replace w_late_formal = (_b[f07_formal_school] - $late_formal)/_se[f07_formal_school] if obsnum == `iter';
   quietly replace w_late_formal_girl = (_b[f07_formal_girl] - $late_formal_girl)/_se[f07_formal_girl] if obsnum == `iter';
   quietly replace w_late_girl = (_b[f07_girl_cnt] - $late_girl)/_se[f07_girl_cnt] if obsnum == `iter';

   *Age-Gender: Formal;
   quietly reg y_fgage f07_girl_cnt f07_age_cnt f07_girl_age_cnt treatment f07_treat_girl f07_treat_age f07_treat_girl_age f07_heads_child $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
   quietly replace w_fgage_girl = (_b[f07_girl_cnt] - $fgage_girl)/_se[f07_girl_cnt] if obsnum == `iter';
   quietly replace w_fgage_age = (_b[f07_age_cnt] - $fgage_age)/_se[f07_age_cnt] if obsnum == `iter';
   quietly replace w_fgage_girl_age = (_b[f07_girl_age_cnt] - $fgage_girl_age)/_se[f07_girl_age_cnt] if obsnum == `iter';
   quietly replace w_fgage_treat = (_b[treatment] - $fgage_treat)/_se[treatment] if obsnum == `iter';
   quietly replace w_fgage_treat_girl = (_b[f07_treat_girl] - $fgage_treat_girl)/_se[f07_treat_girl] if obsnum == `iter';
   quietly replace w_fgage_treat_age = (_b[f07_treat_age] - $fgage_treat_age)/_se[f07_treat_age] if obsnum == `iter';
   quietly replace w_fgage_treat_girl_age = (_b[f07_treat_girl_age] - $fgage_treat_girl_age)/_se[f07_treat_girl_age] if obsnum == `iter';

   *Age-Gender: Total;
   quietly reg y_tgage f07_girl_cnt f07_age_cnt f07_girl_age_cnt treatment f07_treat_girl f07_treat_age f07_treat_girl_age f07_heads_child $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
   quietly replace w_tgage_girl = (_b[f07_girl_cnt] - $tgage_girl)/_se[f07_girl_cnt] if obsnum == `iter';
   quietly replace w_tgage_age = (_b[f07_age_cnt] - $tgage_age)/_se[f07_age_cnt] if obsnum == `iter';
   quietly replace w_tgage_girl_age = (_b[f07_girl_age_cnt] - $tgage_girl_age)/_se[f07_girl_age_cnt] if obsnum == `iter';
   quietly replace w_tgage_treat = (_b[treatment] - $tgage_treat)/_se[treatment] if obsnum == `iter';
   quietly replace w_tgage_treat_girl = (_b[f07_treat_girl] - $tgage_treat_girl)/_se[f07_treat_girl] if obsnum == `iter';
   quietly replace w_tgage_treat_age = (_b[f07_treat_age] - $tgage_treat_age)/_se[f07_treat_age] if obsnum == `iter';
   quietly replace w_tgage_treat_girl_age = (_b[f07_treat_girl_age] - $tgage_treat_girl_age)/_se[f07_treat_girl_age] if obsnum == `iter';
   };

*Enrollment;
reg f07_formal_school treatment f07_treat_girl $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
p_vals w_formal_treat treatment `tot_iterations';
p_vals w_formal_treat_girl f07_treat_girl `tot_iterations';
p_vals w_formal_girl f07_girl_cnt `tot_iterations';
summarize w_formal_treat w_formal_treat_girl w_formal_girl;

*F07 Test Scores;
reg f07_both_norma_total treatment f07_treat_girl $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
p_vals w_total_treat treatment `tot_iterations';
p_vals w_total_treat_girl f07_treat_girl `tot_iterations';
p_vals w_total_girl f07_girl_cnt `tot_iterations';
summarize w_total_treat w_total_treat_girl w_total_girl;

*S08 Test Scores;
reg s08_both_norma_total treatment s08_treat_girl $s08_child_controls $s08_hh_controls chagcharan if nonoutlier == 1 & s08_test_observed == 1, cluster(clustercode);
p_vals w_total_s08_treat treatment `tot_iterations';
p_vals w_total_s08_treat_girl s08_treat_girl `tot_iterations';
p_vals w_total_s08_girl s08_girls_cnt `tot_iterations';
summarize w_total_s08_treat w_total_s08_treat_girl w_total_s08_girl;

*LATE;
ivreg f07_both_norma_total (f07_formal_school f07_formal_girl = treatment f07_treat_girl) $f07_child_controls $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
p_vals w_late_formal f07_formal_school `tot_iterations';
p_vals w_late_formal_girl f07_formal_girl `tot_iterations';
p_vals w_late_girl f07_girl_cnt `tot_iterations';
summarize w_late_formal w_late_formal_girl w_late_girl;

*Age-Gender Formal;
reg f07_formal_school f07_girl_cnt f07_age_cnt f07_girl_age_cnt treatment f07_treat_girl f07_treat_age f07_treat_girl_age f07_heads_child $f07_hh_controls chagcharan if nonoutlier == 1 & f07_observed == 1, cluster(clustercode);
p_vals w_fgage_girl f07_girl_cnt `tot_iterations';
p_vals w_fgage_age f07_age_cnt `tot_iterations';
p_vals w_fgage_girl_age f07_girl_age_cnt `tot_iterations';
p_vals w_fgage_treat treatment `tot_iterations';
p_vals w_fgage_treat_girl f07_treat_girl `tot_iterations';
p_vals w_fgage_treat_age f07_treat_age `tot_iterations';
p_vals w_fgage_treat_girl_age f07_treat_girl_age `tot_iterations';
summarize w_fgage_girl w_fgage_age w_fgage_girl_age w_fgage_treat w_fgage_treat_girl w_fgage_treat_age w_fgage_treat_girl_age;

*Age-Gender Total;
reg f07_both_norma_total f07_girl_cnt f07_age_cnt f07_girl_age_cnt treatment f07_treat_girl f07_treat_age f07_treat_girl_age f07_heads_child $f07_hh_controls chagcharan if nonoutlier == 1 & f07_test_observed == 1, cluster(clustercode);
p_vals w_tgage_girl f07_girl_cnt `tot_iterations';
p_vals w_tgage_age f07_age_cnt `tot_iterations';
p_vals w_tgage_girl_age f07_girl_age_cnt `tot_iterations';
p_vals w_tgage_treat treatment `tot_iterations';
p_vals w_tgage_treat_girl f07_treat_girl `tot_iterations';
p_vals w_tgage_treat_age f07_treat_age `tot_iterations';
p_vals w_tgage_treat_girl_age f07_treat_girl_age `tot_iterations';
summarize w_tgage_girl w_tgage_age w_tgage_girl_age w_tgage_treat w_tgage_treat_girl w_tgage_treat_age w_tgage_treat_girl_age;

log close;

