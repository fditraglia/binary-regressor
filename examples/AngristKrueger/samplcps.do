/* Sample program from analyzing CPS extract from Angrist and Krueger (1995)
*/
clear 
set mem 800m
log using samplcps.log

use extract.dta

keep if yob >= 49 & annwage >0 & weeks >0

replace annwage = annwage*(65.2/82.4) if year==81
replace annwage = annwage*(65.2/90.9) if year==82
replace annwage = annwage*(65.2/96.5) if year==83
replace annwage = annwage*(65.2/99.6) if year==84
replace annwage = annwage*(65.2/103.9) if year==85
  

gen lnyrwage = log(annwage)
gen wkwage = annwage/weeks
gen lnwkwage = log(wkwage)

gen coarse1 = lott1 + lott2 + lott3
gen coarse2 = lott4 + lott5 + lott6
gen coarse3 = lott7 + lott8 + lott9

gen ceiling = coarse1+coarse2

  gen lot1b50 = coarse1*(yob<=50)
  gen lot2b50 = coarse2*(yob<=50)
  gen lot3b50 = coarse3*(yob<=50)
  gen lot1b51 = coarse1*(yob==51)
  gen lot2b51 = coarse2*(yob==51)
  gen lot3b51 = coarse3*(yob==51)
  gen lot1b52 = coarse1*(yob==52)
  gen lot2b52 = coarse2*(yob==52)
  gen lot3b52 = coarse3*(yob==52)
  gen lot1b53 = coarse1*(yob==53)
  gen lot2b53 = coarse2*(yob==53)
  gen lot3b53 = coarse3*(yob==53)
  

  local regions midatl eastnth westnth ///
midatl eastnth westnth ///


local interact lot1b50 lot2b50 lot3b50 ///
lot1b51 lot2b51 lot3b51 ///
lot1b52 lot2b52 lot3b52 ///
lot1b53 lot2b53 lot3b53 ///

keep `interact' `regions' black other city balsmsa spsepres yr81-yr85 ///
      yob45-yob53 educ veteran lnyrwage annwage lnwkwage wkwage ///
      ceiling coarse1-coarse3 lott1-lott13 recode

/* OLS equations with covariates */	  
reg lnyrwage `regions' black other city balsmsa ///
spsepres yr81-yr85 yob50-yob53 educ veteran

reg lnwkwage `regions' black other city balsmsa ///
spsepres yr81-yr85 yob50-yob53 educ veteran	  

/* 2sls with 3-way instrument */
ivreg lnyrwage (veteran = `regions' black other city ///
balsmsa spsepres yr81-yr85 yob50-yob53 educ `interact') ///
`regions' black other city balsmsa spsepres yr81-yr85 ///
yob50-yob53 educ veteran

ivreg lnwkwage (veteran = `regions' black other city ///
balsmsa spsepres yr81-yr85 yob50-yob53 educ `interact') ///
`regions' black other city balsmsa spsepres yr81-yr85 ///
yob50-yob53 educ veteran

log close
