
clear
cls

cd "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_May1\Year"

/**/
cls

capture log close
log using "YearModels_i.post2020_May2.smcl" , replace

use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21.dta", clear
gen post2020=0
replace post2020=1 if year==2021

display("")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display(" ALL YEARS W YEAR (post-2020 dummy=1)")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display("")

display("")
display("---------------------------------------------------------------------")
display("Model 1: Null")
capture noisily melogit suicide || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 2: Fixed Year")
capture noisily melogit suicide i.post2020 || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 3: Random Year")
capture noisily melogit suicide i.post2020 || stratum3: i.post2020 , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 4: Year (post-2020 dummy=1) as fixed + Additive Main effects")
capture noisily melogit suicide i.post2020 i.race i.gender i.sexorient || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 5: Additive Main effects + Year (post-2020 dummy=1) as random coefficient")
capture noisily melogit suicide i.post2020 i.race i.gender i.sexorient || stratum3: i.post2020, binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 6: Additive Main effects + Year (post-2020 dummy=1) as random coefficient + Covariates")
capture noisily melogit suicide i.post2020 i.race i.gender i.sexorient pblack_st phisp_st popdens_st econf || stratum3: i.post2020, binomial(n)  covariance(unstructured) dnumerical or baselevels
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1


translate @Results YearModels_i.post2020_May2.txt, replace 
* Close log file
capture log close
translate "YearModels_i.post2020_May2.smcl" "YearModels_i.post2020_May2.pdf", replace





clear
cls


capture log close
log using "YearModels_Pre2020_Models1-3_May2.smcl" , replace

display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21.dta", clear
keep if year < 2020

display("")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display(" PRE-2020")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display("")

display("---------------------------------------------------------------------")
display("Model 1 (Pre-2020): Null")
capture noisily melogit suicide || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 2 (Pre-2020): Additive Main effects")
capture noisily melogit suicide i.race i.gender i.sexorient || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model (Pre-2020): Additive Main effects + Covariates")
capture noisily melogit suicide i.race i.gender i.sexorient pblack_st phisp_st popdens_st econf || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1


translate @Results YearModels_Pre2020_Models1-3_May2.txt, replace 
* Close log file
capture log close
translate "YearModels_Pre2020_Models1-3_May2.smcl" "YearModels_Pre2020_Models1-3_May2.pdf", replace





clear
cls


capture log close
log using "YearModels_Post2020_Models1-3_May2.smcl" , replace

display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21.dta", clear
keep if year > 2020

display("")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display(" POST-2020")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display("")

display("---------------------------------------------------------------------")
display("Model 1 (Post-2020): Null")
capture noisily melogit suicide || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 2 (Post-2020): Additive Main effects")
capture noisily melogit suicide i.race i.gender i.sexorient || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model (Post-2020): Additive Main effects + Covariates")
capture noisily melogit suicide i.race i.gender i.sexorient pblack_st phisp_st popdens_st econf || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1


translate @Results YearModels_Post2020_Models1-3_May2.txt, replace 
* Close log file
capture log close
translate "YearModels_Post2020_Models1-3_May2.smcl" "YearModels_Post2020_Models1-3_May2.pdf", replace




/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/


clear
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21_DROPPEDSTATES.dta", clear

cls
capture log close
log using "YearModels_DroppedStates_i.post2020_May2.smcl" , replace

use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21_DROPPEDSTATES.dta", clear

gen post2020=0
replace post2020=1 if year==2021

display("")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display(" ALL YEARS W YEAR (post-2020 dummy=1)")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display("")

display("")
display("---------------------------------------------------------------------")
display("Model 1: Null")
capture noisily melogit suicide || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 2: Fixed Year")
capture noisily melogit suicide i.post2020 || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 3: Random Year")
capture noisily melogit suicide i.post2020 || stratum3: i.post2020 , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 4: Year (post-2020 dummy=1) as fixed + Additive Main effects")
capture noisily melogit suicide i.post2020 i.race i.gender i.sexorient || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 5: Additive Main effects + Year (post-2020 dummy=1) as random coefficient")
capture noisily melogit suicide i.post2020 i.race i.gender i.sexorient || stratum3: i.post2020, binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 6: Additive Main effects + Year (post-2020 dummy=1) as random coefficient + Covariates")
capture noisily melogit suicide i.post2020 i.race i.gender i.sexorient pblack_st phisp_st popdens_st econf || stratum3: i.post2020, binomial(n)  covariance(unstructured) dnumerical or baselevels
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1


display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21_DROPPEDSTATES.dta", clear
keep if year < 2020

display("")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display(" PRE-2020")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display("")

display("---------------------------------------------------------------------")
display("Model 1 (Pre-2020): Null")
capture noisily melogit suicide || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 2 (Pre-2020): Additive Main effects")
capture noisily melogit suicide i.race i.gender i.sexorient || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model (Pre-2020): Additive Main effects + Covariates")
capture noisily melogit suicide i.race i.gender i.sexorient pblack_st phisp_st popdens_st econf || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21_DROPPEDSTATES.dta", clear
keep if year > 2020

display("")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display(" POST-2020")
display("---------------------------------------------------------------------")
display("---------------------------------------------------------------------")
display("")

display("---------------------------------------------------------------------")
display("Model 1 (Post-2020): Null")
capture noisily melogit suicide || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model 2 (Post-2020): Additive Main effects")
capture noisily melogit suicide i.race i.gender i.sexorient || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels 
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1

display("")
display("---------------------------------------------------------------------")
display("Model (Post-2020): Additive Main effects + Covariates")
capture noisily melogit suicide i.race i.gender i.sexorient pblack_st phisp_st popdens_st econf || stratum3: , binomial(n)  covariance(unstructured) dnumerical or baselevels
capture noisily estimates store m1
capture noisily estat icc
capture noisily estat vce, block 
capture noisily estimates replay m1, baselevels
capture noisily drop _est_m1


translate @Results YearModels_DroppedStates_i.post2020_May2.txt, replace 
* Close log file
capture log close
translate "YearModels_DroppedStates_i.post2020_May2.smcl" "YearModels_DroppedStates_i.post2020_May2.pdf", replace








