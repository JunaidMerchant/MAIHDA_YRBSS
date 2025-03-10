clear
cls

cd "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles"

*-------------------------------------------------------------------------------
* CLEANING DATA FILES
*-------------------------------------------------------------------------------


*-------------------------------------------------------------------------------
* clean the ACS data
use "C:\Users\jmerch\Desktop\LGBTQ\YRBS\Download_Dec12\ACS_state_2017_2019_2021.dta", clear
drop if white_st==.
drop if popdens_st==.
save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\ACS_state_2017_2019_2021.dta", replace

*-------------------------------------------------------------------------------
* clean race sent data
use "C:\Users\jmerch\Desktop\LGBTQ\YRBS\Download_Dec12\YRBSS_wRaceSentByState_2017_2019_2021.dta", clear

drop if statefp>55
drop if statefp==53
drop if statefp==41
drop if statefp==39
drop if statefp==25
drop if statefp==27
drop if statefp==11
drop x_merge sitename sitetype


replace statefp=36 if state=="NY"
replace statefp=20 if state=="KS"

foreach var of varlist age-sexpart2 qbikehelmet-qtransgender {
	replace `var'="" if `var'=="NA"
	destring `var', replace
}

save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\YRBSS_wRaceSentByState_2017_2019_2021.dta", replace


*-------------------------------------------------------------------------------
* clean lgbtq sent
use "C:\Users\jmerch\Desktop\LGBTQ\YRBS\Download_Dec12\combine_2017-2021_unfiltered_countyandstatefips_lgbtqcats_happysad_smaller_byYearState.dta", clear
drop if statefp>55
gen percsentsadlgbtq=sentsadlgbtq*100
gen percsentlgbtq=sentlgbtq*100
save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\LGBTQ_SentByState_2017_2019_2021.dta", replace


*-------------------------------------------------------------------------------
* MERGE DATA FILES
*-------------------------------------------------------------------------------
* load yrbss/race sent data
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\YRBSS_wRaceSentByState_2017_2019_2021.dta", clear
merge m:1 year statefp using "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\LGBTQ_SentByState_2017_2019_2021.dta"
drop if _merge==2
drop _merge

merge m:1 year statefp using "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\ACS_state_2017_2019_2021.dta"
drop if _merge==2
drop _merge

save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\YRBSS_wRaceLGBTSentACS_2017_2019_2021.dta", replace



*-------------------------------------------------------------------------------
* COLLAPSE TO MAKE BINOMIAL ANALYSIS READY FILE
*-------------------------------------------------------------------------------
* read in file
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\YRBSS_wRaceLGBTSentACS_2017_2019_2021.dta", clear

* remove missing data
drop if suicide==.
drop if sex==.
drop if race7==.
drop if sexorient=="missing"
drop if state==""
drop if statefp==.

* create gender
gen gender_char="Male" if sex==2
replace gender_char="Female" if sex==1

drop gender
gen gender=1 if gender_char=="Male"
replace gender=2 if gender_char=="Female"
label var gender "Gender"
label val gender gender
label define gender 1"1:Male" 2"2:Female"

* create race
gen race_char=race
drop race
gen race=1 if race_char=="White"
replace race=2 if race_char=="Asian"
replace race=3 if race_char=="Black"
replace race=4 if race_char=="Hispanic"
replace race=5 if (race_char=="Multi-Race" | race_char=="Native/Alaska" | race_char=="Pacific/Hawaiin")
label var race "Race"
label val race race
label define race 1"1:White" 2"2:Asian" 3"3:Black" 4"4:Hispanic" 5"5:MultiOtherRace"
replace race_char= "MultiOtherRace" if race==5

* create sexual orientation
gen sexorient_char=sexorient
drop sexorient
gen sexorient=1 if sexorient_char=="Heterosexual"
replace sexorient=2 if sexorient_char=="Gay/Lesbian"
replace sexorient=3 if sexorient_char=="Bisexual"
replace sexorient=4 if sexorient_char=="Other"
replace sexorient_char="Homosexual" if sexorient_char=="Gay/Lesbian"
replace sexorient_char="Othersexual" if sexorient_char=="Other"
label var sexorient "Sexual Orientation"
label val sexorient sexorient
label define sexorient 1"1:Heterosexual" 2"2:Homosexual" 3"3:Bisexual" 4"4:Othersexual" 

* create stratum3
gen stratum3 = gender*100 + race*10 + sexorient*1
gen stratum3_char=gender_char + "-" + race_char + "-" + sexorient_char if (race_char!="" & gender_char!="" & sexorient_char!="")

* Move stratum to the beginning of the dataset
order stratum3
bysort stratum3: generate n = _N

save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.dta", replace


collapse (count) n = suicide (sum) suicide (mean) suicide_perc=suicide percsentsadrace=percsentsadraces_nowhite percsentrace=percsentraces percsentsadlgbtq percsentlgbtq pblack_st phisp_st popdens_st econf, by(year state statefp stratum3 stratum3_char race gender sexorient)

save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_Mar21.dta", replace



*-------------------------------------------------------------------------------
* MERGE ICPSR PRRI FILES
*-------------------------------------------------------------------------------
use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_Mar21.dta", clear

merge m:1 year state using "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Feb26\PRRI_3q.dta"
drop if _merge==2
drop _merge

merge m:1 state using  "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Feb26\ICPSR_small.dta"
drop if _merge==2
drop _merge

save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21.dta", replace

/*
. summarize

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
    stratum3 |      3,464      183.69    52.09103        111        254
        year |      3,464    2019.099    1.602971       2017       2021
       state |          0
     statefp |      3,464    28.21709    14.85001          1         55
      gender |      3,464    1.510104    .4999701          1          2
-------------+---------------------------------------------------------
        race |      3,464    3.020208    1.439971          1          5
   sexorient |      3,464    2.477483     1.13616          1          4
stratum3_c~r |          0
           n |      3,464    108.9382    469.8498          1      10655
     suicide |      3,464     20.1836    69.12743          0       1458
-------------+---------------------------------------------------------
suicide_perc |      3,464     .319537    .2503013          0          1
percsentsa~e |      3,464    40.76249    4.881663   26.46568   49.46727
percsentrace |      3,464     5.95512    .8784614   4.411886   8.310343
percsentsa~q |      3,464     38.5836    12.78022          0   69.23077
percsentlg~q |      3,464    7.974468    7.490515          0         50
-------------+---------------------------------------------------------
   pblack_st |      3,464    6.923822    8.439579          0   37.57905
    phisp_st |      3,464    9.375838    11.85994          0   48.78564
  popdens_st |      3,464    298.3581         289   10.80386    1260.11
       econf |      3,464   -.1971808    .8136713  -1.729722   2.737971
    location |          0
-------------+---------------------------------------------------------
       favor |      3,464     73.4642    6.497419         58         87
      oppose |      3,464    21.04301     4.54002         10         34
dontknowre~e |      3,242    5.927822    3.219096          1         14
 favor_ssmar |      2,233    65.16301    8.671749         44         82
oppose_ssmar |      2,233    29.78012    8.009152         17         55
-------------+---------------------------------------------------------
dkrefuse_s~r |      2,061    5.362931    3.991285          1         15
favor_relref |      3,464    34.54186    5.638087         21         53
oppose_rel~f |      3,464     60.0586    7.066719         43         78
dkrefuse_r~f |      3,263    5.798958    3.238436          1         13
   overall20 |      3,464    15.31416     13.4697       -6.5      34.75
-------------+---------------------------------------------------------
overall20_~t |      2,960    1.365878    1.144621          0          3
        so20 |      3,464    8.421334    5.800031         -2       17.5
   so20_4cat |      2,786    1.463029     1.28668          0          3
        gi20 |      3,464    6.892826    7.835379       -4.5      18.25
   gi20_4cat |        992           0           0          0          0
-------------+---------------------------------------------------------
       rpr20 |      3,464    3.666643    1.554106        1.5          7
nondiscri~20 |      3,464    4.692263    3.831585         -2          9
religious~20 |      3,464   -1.082419    1.356711         -6          0
     youth20 |      3,464    3.404446    3.263895         -2          8
   overall10 |      3,464     3.24004    7.358849         -6      19.25
-------------+---------------------------------------------------------
overall10_~t |      2,605    2.591171    .5450507          1          3
        so10 |      3,464    3.634382    4.851645         -3         12
   so10_4cat |      2,780    2.652518    .7579096          1          3
        gi10 |      3,464   -.3943418    3.040127         -5       7.25
   gi10_4cat |          0
-------------+---------------------------------------------------------
       rpr10 |      3,464    1.165055    2.044786         -1       5.75
nondiscri~10 |      3,464    3.266022    3.511259          0          9
religious~10 |      3,464    -.431582    .5526783         -2          0
     youth10 |      3,464    .9406755    1.869366         -2          5

*/


cls
* Log analysis and save log


use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21.dta", clear

drop if (state == "AL" | state == "CA" | state == "DE" | state == "IN" | state == "KS" | state == "MO" | state == "MS" | state == "NJ" | state == "SC" | state == "UT" | state == "VA" | state == "VT" | state == "WA" | state == "WY" | state == "TN" | state == "SD"  | state == "OR" | state == "OH" | state == "MT" | state == "MN" | state == "LA" | state == "MA"  | state == "GA" | state == "ID"  | state == "AK")

tab state


save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_Mar21_DROPPEDSTATES.dta", replace




use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.dta", clear


collapse (count) n = suicide (sum) suicide (mean) q* bully ebully sad suicide_perc=suicide percsentsadrace=percsentsadraces_nowhite percsentrace=percsentraces percsentsadlgbtq percsentlgbtq pblack_st phisp_st popdens_st econf, by(year state statefp stratum3 stratum3_char race gender sexorient)

merge m:1 year state using "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Feb26\PRRI_3q.dta"
drop if _merge==2
drop _merge

merge m:1 state using  "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Feb26\ICPSR_small.dta"
drop if _merge==2
drop _merge

save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_April23.dta", replace

export delimited  "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_April23.csv", replace



use "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.dta", clear


collapse (count) n = suicide (sum) suicide (mean) q* bully ebully sad suicide_perc=suicide percsentsadrace=percsentsadraces_nowhite percsentrace=percsentraces percsentsadlgbtq percsentlgbtq pblack_st phisp_st popdens_st econf, by(year state statefp)

merge m:1 year state using "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Feb26\PRRI_3q.dta"
drop if _merge==2
drop _merge

merge m:1 state using  "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Feb26\ICPSR_small.dta"
drop if _merge==2
drop _merge

save "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_STATEONLYNOSTRATA_April23.dta", replace

export delimited  "C:\Users\jmerch\Desktop\LGBTQ\MAIHDA\StataResults_Mar21\DataFiles\binomialform_analysis_ready_data_wPRRI_ICPSR_STATEONLYNOSTRATA_April23.csv", replace