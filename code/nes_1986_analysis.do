/* This file:
- Imports 1986 ANES data
- Defines variables and constructs indices 
- Performs regression analyses
- Generates descriptive statistics/frequencies

Last updated September 21, 2023 by Mark Weiss */


/* I: Import data, operationalize variables 
------------------------------------------------------------- */ 
cd "/Users/markweiss/Documents/Research/Projects/kinder_ra/data"

use nes1986, clear

* identify form A and form B respondents
rename V860046 _form

local forms a b 
foreach form of local forms {
	g form_`form' = 0
}

replace form_a = 1 if _form == 1
replace form_b = 1 if _form == 2

* only keep necessary variables
keep form_a form_b V860405 V860491 V860428 V860102 V860489 V860059 V860066 V860100 V860130 V860149 V860755 V860756 V860757 V860754 V860761 V860599 V860490 V860343 V860344 V860345 V860346 V860347 V860348 V860102 V860301 V860485 V860580 V860568 V860579 V860566 V860448 V860487 V860521 V860298 V860297 V860300 V860365 

* create variables for demographic information
rename V860755 sex
rename V860756 race
rename V860757 hispanic 
rename V860599 education
rename V860754 rent_own 

g white = . // white means non-hispanic whites
replace white = 0 if inrange(race, 1, 4) & race != 99 
replace white = 1 if race == 1 & hispanic == 5 & race != 99 

g black = .
replace black = 1 if race == 2
replace black = 0 if race == 1 | (race > 2 & race != 99)

g nhs = .
replace nhs = 1 if education < 12 
replace nhs = 0 if education >= 12 & education != 99 

g hs = . 
replace hs = 1 if education == 12
replace hs = 0 if education < 12 | (education > 12 & education != 99) 

g somecol = . 
replace somecol = 1 if inrange(education, 13, 15)
replace somecol = 0 if education < 12 | (education > 15 & education != 99)  

g college = .
replace college = 1 if education == 16 
replace college = 0 if education < 16 | (education > 16 & education != 99) 

g own = . 
replace own = 1 if rent_own == 1
replace own = 0 if inrange(rent_own, 2, 7)

replace sex = 0 if sex == 2

/*g female = .
replace female = 1 if sex == 2 
replace female = 0 if sex == 1 */ 

* rename variables related to political knowledge
forval y = 3/8 { // replace each relevent variable name with its label
	foreach v of var V86034`y' {
		local x : var label `v'
		local x = strtoname("`x'")
		rename `v' `x' 
	}
}

rename DOES_R_KNW_CAP_WEINBERGR DOES_R_KNOW_CAP_WEINBERGR
local names GEORGE_BUSH CAP_WEINBERGR WM_REHNQUIST PAUL_VOLKER ROBERT_DOLE TIP_O_NEILL

* sum correct responses to form knowledge of politics index
foreach name of local names {
	g ident_`name' = . 
	replace ident_`name' = 1 if DOES_R_KNOW_`name' == 1 
	replace ident_`name' = 0 if inlist(DOES_R_KNOW_`name', 5, 8)
}

egen knowledge = rowmean(ident_GEORGE_BUSH ident_CAP_WEINBERGR ident_WM_REHNQUIST ident_PAUL_VOLKER ident_ROBERT_DOLE ident_TIP_O_NEILL)

* taste for politics variable
rename V860102 interest_1
rename V860059 interest_2

recode interest_1 (1 = 0.5) (2 = 0.334) (4 = 0.167) (5 = 0) (8 = .) (9 = .), g(interest_a)
recode interest_2 (1 = 0.5) (3 = 0.334) (5 = 0.167) (9 = .), g(interest_b)

egen taste = rowtotal(interest_a interest_b)

* variable for economic sanctions opinions
rename V860490 support_sanctions
rename V860489 know_africa 

g know_sanct = . 
replace know_sanct = 1 if inlist(support_sanctions, 1, 5)
replace know_sanct = 0 if inlist(support_sanctions, 0, 8)

g sanctions = . // takes a position on sanctions 
replace sanctions = 1 if support_sanctions == 1
replace sanctions = 0 if inlist(support_sanctions, 5, 8) 

/* modern racism variable based on the standard four questions
rename variables for ease of use */
forval i = 79/80 {
	rename V8605`i' rr_`i'
} 
foreach i in 66 68 {
	rename V8605`i' rr_`i'
}

foreach i in 66 80 { // reverse coded
	recode rr_`i' (1 = -1) (2 = -0.5) (3 = 0) (4 = 0.5) (5 = 1) (8 = .) (9 = .)
}
foreach i in 68 79 { // normal coded
	recode rr_`i' (1 = 1) (2 = 0.5) (3 = 0) (4 = -0.5) (5 = -1) (8 = .) (9 =. )
}

egen racism = rowmean(rr_66 rr_68 rr_79 rr_80) if !mi(rr_66) & (rr_68) & !mi(rr_79) & !mi(rr_80)

* alternative outcomes for question 5 
local outcomes govt_services coop_russia import_restr defense_spend interv_ca

rename V860448 govt_services 
rename V860487 coop_russia 
rename V860521 import_restr 
rename V860405 defense_spend 
rename V860428 interv_ca 

foreach outcomes of local outcomes {
	replace `outcome' = . if inlist(`outcome', 0, 8, 9)
}

replace import_restr = 0 if import_restr == 5

* partisanship variable
rename V860300 party_id 
recode party_id (0 = -1) (1 = -.666) (2 = -.333) (3 = 0) (4 = .333) (5 = .666) (6 = 1)
replace party_id = . if inlist(party_id, 7, 8, 9)

/* II: Prepare descriptive Statistics, histograms, etc.
------------------------------------------------------------- */ 

* variable for strength of support/opposition for sanctions
rename V860491 strength

local feels strong somewhat 
foreach feel of local feels {
	g support_`feel' = .
	g oppose_`feel' = . 

	replace support_`feel' = . if inlist(strength, 0, 8, 9) // missings or don't know 
	replace oppose_`feel' = . if inlist(strength, 0, 8, 9)
}

replace strength = 0 if strength == 8 
 
replace support_strong = 1 if strength == 1
replace support_strong = 0 if inlist(strength, 2, 4, 5)

replace oppose_strong = 1 if strength == 5 
replace oppose_strong = 0 if inlist(strength, 1, 2, 4)

replace support_somewhat = 1 if strength == 2
replace support_somewhat = 0 if inlist(strength, 1, 4, 5)

replace oppose_somewhat = 1 if strength == 4
replace oppose_somewhat = 0 if inlist(strength, 1, 2, 5)


/* III: Regression Analyses
------------------------------------------------------------- */ 
local controls college somecol nhs knowledge taste sex 
local surveys form_a form_b
local populations black white 
local results white_form_a white_form_b white_bothform black_bothform 

local outcomes govt_services coop_russia import_restr defense_spend interv_ca

* question three: 
est drop _all 
foreach population of local populations { // white and Black separately, form A+B
	eststo africa_`population'_bothform: qui logit sanctions `controls' if `population' == 1

}
foreach survey of local surveys { // white only, forms A and B separately 
	eststo africa_white_`survey': qui logit sanctions `controls' if `survey' == 1 

}

estout africa_black_bothform africa_white_bothform africa_white_form_a africa_white_form_b, cells(b(star fmt(3)) se(par fmt(2)) p)

* question four: 
eststo africa_opp_white_formb: qui logit sanctions `controls' racism if white == 1 & form_b == 1

* question five:
foreach outcome of local outcomes { // assuming non-hispanic whites, form B
	eststo results_`outcome': qui logit `outcome' `controls' racism if white == 1 & form_b == 1
}

/* IV: Log file, to deliver to Don 
------------------------------------------------------------- 
log using anes1986_analysis.log, replace 

/* 1) Frequencies for non-Hispanic whites on sanctions support: 
(variable `strength' indicates support, not asked recoded to 0): */

ta strength if white == 1 & form_a == 1 // form A
ta strength if white == 1 & form_b == 1 // form B
ta strength if white == 1 // form A and form B

* 2) Frequencies for Black Americans:

ta strength if black == 1 & form_a == 1 // form A
ta strength if black == 1 & form_b == 1 // form B
ta strength if black == 1 // form A and form B

/* 3) Accounting for no opinion, regression results 
less than hs is ommitted from the models because of collinearity */
estout africa_black_bothform africa_white_bothform africa_white_form_a africa_white_form_b, cells(b(star fmt(3)) se(par fmt(2)) p)

* 4) accounting for no opinion with racial resentment, regression results
estout africa_opp_white_formb, cells(b(star fmt(3)) se(par fmt(2)) p)
log close

* 5) identical model for other outcomes
end
logfile */
